package feh.tec.astar

import akka.actor._
import akka.dispatch.{PriorityGenerator, UnboundedStablePriorityMailbox}
import com.typesafe.config.Config
import feh.tec.astar.A_*.{AStarException, SortedPossibilities}
import feh.tec.astar.LimitedHorizon.Parallel.Finished
import feh.util.{RecFunc, _}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}

/** Abstract <b>Limited Depth / Horizon</b> implementation. */
trait LimitedHorizon[T] {
  self : A_*[T] =>

  /** The maximum recursion depth. */
  def maxDepth: Int

  /** Select the best open (partial solution 'open' pruning) */
  def selectTheBest: SortedPossibilities[Heuristic, T] => Map[Heuristic, Set[T]]


  /** Handles partial solutions. */
  def handlePartialSolution(ps: PartialSolution): RecFunc.Res[T, (Try[T], History[T])]

  /** Executes A* with Limited Horizon. */
  protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result

  case class PartialSolution(best: Set[T])

  /** An instance of [[SearchInnerResult]] to denote [[PartialSolution]]s. */
  case class PartialSolutionReturn(result: Try[PartialSolution],
                                   history: History[T] = NoHistory()) extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy(history = h)
  }


  /** Returns partial solution if the maximum recursion depth was reached. */
  override protected def searchInnerExtraLogic: Decide =
    count => {
      case Some((best, open)) if count == maxDepth - 1 =>
        val bestSel = selectTheBest  apply (open + best)
        PartialSolutionReturn(Success(PartialSolution(bestSel.values.toSet.flatten)))
    }

  val HistoryManagement: HistoryManagement

  /** History management interface. */
  trait HistoryManagement{
    /** Guard history. */
    def saveHistory(h: History[T])
    /** List current history. */
    def listHistory: List[History[T]]
    /** Clear current history. */
    def clearHistory(): Unit
  }

  /** Runs [[A_*.searchInner]] and processes the results. */
  protected def searchLH = RecFunc[T, Result]{
    t =>
      if (isSolution(t)) {
        val hist = HistoryRecord(HistoryEntry(t) :: Nil)
        HistoryManagement.saveHistory(hist)
        RecFunc Ret Success(t) -> hist
      }
      else runSearchInner(t) match {
        case SearchInnerReturn(res, history) =>
          HistoryManagement.saveHistory(history)
          RecFunc Ret (res -> history)
        case PartialSolutionReturn(Success(ps), history) =>
          HistoryManagement.saveHistory(history)
          handlePartialSolution(ps)
        case PartialSolutionReturn(fail, history) =>
          HistoryManagement.saveHistory(history)
          RecFunc Ret fail.asInstanceOf[Failure[T]] -> history
        case _: SearchInnerRecCall =>
          RecFunc Ret searchInnerRecCallEscaped_!
      }
  }

  /** Searches for a solution with limited horizon.
    *
    * @param initial The initial state.
    * @return `Some(solution)` or `None` if the solution wasn't found.
    */
  override def search(initial: T) = execSearchLH(searchLH)(initial)


}


object LimitedHorizon{

  object HistManagement{

    /** Im memory [[LimitedHorizon.HistoryManagement]] implementation. */
    trait InMemory[T]{
      self: LimitedHorizon[T] =>

      protected val historyInMem = ListBuffer.empty[History[T]]
      protected var lastRun      = 0

      val HistoryManagement: HistoryManagement = new HistoryManagement{
        def saveHistory(h: History[T]): Unit = synchronized{
          lastRun += 1
          historyInMem += h.map(_.copy(runId = lastRun))
        }
        def listHistory = historyInMem.toList
        def clearHistory() = {
          historyInMem.clear()
          lastRun = 0
        }
      }
    }

  }

  /** Defines a mutable buffer for guarding partial solutions (kind of global 'open'). */
  protected trait PartialSolutionsBuffer[T]{
    self : A_*[T] with LimitedHorizon[T] =>

    protected var buffer = SortedPossibilities.empty[Heuristic, T]

    /** Saves the [[PartialSolution]]. */
    def guardPartialSolution(ps: PartialSolution): Unit = { buffer ++= ps.best }

    /** Pops the best [[PartialSolution]] (removing from the buffer). */
    def popBestPartialSolution(): Option[T] = extractTheBest(buffer) map {
      case (best, rest) =>
        buffer = rest
        best
    }

    /** Clears the [[PartialSolution]] buffer. */
    protected def clearPartialSolutions() = buffer = SortedPossibilities.empty

  }

  /** Sequential [[LimitedHorizon]] executor. */
  trait Sequential[T] extends LimitedHorizon[T] with PartialSolutionsBuffer[T] {
    self : A_*[T] =>

    def handlePartialSolution(ps: PartialSolution) = {
      guardPartialSolution(ps)
      popBestPartialSolution()
        .map(RecFunc.Rec.apply)
        .getOrElse(RecFunc.Ret(Failure(AStarException("no more partial solutions to search")) -> NoHistory()))
    }

    protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result = RecFunc.TCO(state, f)

    /** Searches for a solution with limited horizon.
      *
      * @param initial The initial state.
      * @return `Some(solution)` or `None` if the solution wasn't found.
      */
    override def search(initial: T): (Try[T], History[T]) = {
      clearPartialSolutions()
      super.search(initial)
    }
  }

  /** Parallel [[LimitedHorizon]] executor.
   *  Is implemented with [[akka.actor]] actors.
   */
  trait Parallel[T] extends LimitedHorizon[T] {
    self: A_*[T] =>

    /** Execution time limit. */
    def maxExecTime: FiniteDuration
    /** The number of parallel executors. */
    def executorPoolSize: Int

    /** An [[ActorRefFactory]] for creating actors. */
    protected def aFactory: ActorRefFactory

    /** A message, requesting execution of A*
      *
      * @param state   initial state.
      * @param promise a [[Promise]] for the result.
      * @param f       a recursive function to execute.
      */
    protected case class InitExec(state: T, promise: Promise[Result], f: RecFunc[T, Result])

    /** Commands an executor to exec the given function with the given 'state' argument. */
    protected case class Exec(state: T, f: RecFunc[T, Result])


    protected case object AlreadyRunning extends Exception("A* is already running in parallel, use another instance")
    protected case object NoExecutor extends Exception("A* has no free concurrent executors")

    /** A [[ThreadLocal]] scoped state for an executor. */
    protected val executor = new ScopedState[ActorRef](null)

    /** An abstract execution controller actor. */
    protected abstract class Controller extends Actor with ActorLogging{
      /** Current result promise. */
      protected var promiseOpt: Option[Promise[Result]] = None
      /** Current function to execute. */
      protected var func: RecFunc[T, Result] = null

      /** Get a free executor. */
      protected def getFree: Option[ActorRef]
      /** Number of free executors. */
      protected def countFree: Int
      /** Free an executor actor. */
      protected def free: ActorRef => Unit

      /** Guard a partial solution. */
      protected def guardPartialSolution: PartialSolution => Unit
      /** Get the next state from the global 'open'. */
      protected def nextTask: Option[T]

      /** Run next task (if any) with a free executor (if any). */
      def runNextTask() = for{
        next <- nextTask
        exec <- getFree
      } yield exec ! Exec(next, func)

      /** Runs tasks as long as there are free executors and tasks to execute. */
      def runNextTasks() = while ( runNextTask().isDefined ) {}

      /** The controller's [[Actor.receive]] function, processing incoming messages. */
      def receive: Receive = {
        case InitExec(_, promise, _) if promiseOpt.isDefined => promise.failure(AlreadyRunning)
        case InitExec(init, promise, f) => 
          promiseOpt = Option(promise)
          func = f
          getFree match {
            case Some(exec) => exec ! Exec(init, f)
            case _          => promise.failure(NoExecutor)
          }
        case _ if promiseOpt.isEmpty =>
          free(sender())
        case Finished(Left(ps : PartialSolution)) =>
          guardPartialSolution(ps)
          free(sender())
          runNextTasks()
        case Finished(Right(res : Try[T])) =>
          promiseOpt.get.success(res -> NoHistory())
          promiseOpt = None
          func = null
      }
    }

    /** Task executor actor. */
    protected class Executor(val controller: ActorRef) extends Actor with ActorLogging{
      def receive: Actor.Receive = {
        case Exec(st, f) =>
          executor.doWith(self){ execSearchLH(f)(st) }
      }
    }

    protected lazy val controllerRef: ActorRef = aFactory.actorOf(controllerProps)

    def handlePartialSolution(ps: PartialSolution): RecFunc.Res[T, Result] = {
//      executor.get.log("handlePartialSolution " +  executor.get)
      implicit val sender = executor.get
      controllerRef ! Finished(Left(ps))
      RecFunc Rec null.asInstanceOf[T]
    }


    /** Searches for a solution with limited horizon.
      *
      * @param initial The initial state.
      * @return `Some(solution)` or `None` if the solution wasn't found.
      */
    override def search(initial: T): (Try[T], History[T]) = {
      val promise = Promise[Result]()
      controllerRef ! InitExec(initial, promise, searchLH)
      Await.result(promise.future, maxExecTime)
    }


    protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result = f(state) match {
      case RecFunc.Ret(r@(res, _))  =>
        implicit val sender = executor.get
        controllerRef ! Finished(Right(res))
        r
      case RecFunc.Rec(null) => null
    }

    protected def controllerProps = Props(new Controller {
      val execPool = Stream.fill(executorPoolSize)(aFactory.actorOf(executorProps(self)))
      var freePool = execPool

      var partialSolutions = SortedPossibilities.empty

      protected def getFree: Option[ActorRef] =
        freePool.headOption.map{
          h =>
            freePool = freePool.tail
            h
        }

      protected def free: ActorRef => Unit = freePool +:= _
      protected def countFree = freePool.size

      protected def guardPartialSolution: PartialSolution => Unit = ps => partialSolutions ++= ps.best
      protected def nextTask: Option[T] = extractTheBest(partialSolutions) match {
        case Some((t, ps)) =>
          partialSolutions = ps
          Some(t)
        case _ => None
      }
    })
    
    protected def executorProps(parent: ActorRef) = Props(new Executor(parent))

  }

  object Parallel{
    /** A message, notifiyng that an executor has found the result (or failed with error). */
    protected case class Finished[L, R](res: Either[L, Try[R]])

    /** A custom mailbox for actors, with priority for [[Finished]] message. */
    class PriorityMailbox(settings: ActorSystem.Settings, config: Config)
      extends UnboundedStablePriorityMailbox(
        PriorityGenerator {
          case Finished(Right(res)) => 0
          case PoisonPill           => 2
          case _                    => 1
        }
      )
  }
}