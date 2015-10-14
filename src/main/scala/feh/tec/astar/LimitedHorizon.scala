package feh.tec.astar

import akka.actor._
import feh.tec.astar.A_*.{AStarException, SortedPossibilities}
import feh.util.{RecFunc, _}

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success, Try}

trait LimitedHorizon[T] {
  self : A_*[T] =>

  def maxDepth: Int

  def selectTheBest: SortedPossibilities[Heuristic, T] => Map[Heuristic, Set[T]]



  def handlePartialSolution(ps: PartialSolution): RecFunc.Res[T, (Try[T], History[T])]

  protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result

  case class PartialSolution(best: Set[T])

  case class PartialSolutionReturn(result: Try[PartialSolution],
                                   history: History[T] = NoHistory()) extends SearchInnerResult
  {
    def changeHistory(h: History[T]) = copy(history = h)
  }


  override protected def searchInnerExtraLogic: Decide =
    count => {
      case Some((best, open)) if count == maxDepth - 1 =>
        val bestSel = selectTheBest  apply (open + best)
        PartialSolutionReturn(Success(PartialSolution(bestSel.values.toSet.flatten)))
    }

  val HistoryManagement: HistoryManagement

  trait HistoryManagement{
    def saveHistory(h: History[T])
    def listHistory: List[History[T]]
    def clearHistory(): Unit
  }

  protected def searchLH = RecFunc[T, Result]{
    t =>
      runSearchInner(t) match {
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

    trait InMemory[T]{
      self: LimitedHorizon[T] =>

      protected val historyInMem = ListBuffer.empty[History[T]]

      val HistoryManagement: HistoryManagement = new HistoryManagement{
        def saveHistory(h: History[T]): Unit = synchronized{ historyInMem += h }
        def listHistory = historyInMem.toList
        def clearHistory() = historyInMem.clear()
      }
    }

  }


  protected trait PartialSolutionsBuffer[T]{
    self : A_*[T] with LimitedHorizon[T] =>

    protected var buffer = SortedPossibilities.empty[Heuristic, T]

    def guardPartialSolution(ps: PartialSolution): Unit = { buffer ++= ps.best }

    def popBestPartialSolution(): Option[T] = extractTheBest(buffer) map {
      case (best, rest) =>
        buffer = rest
        best
    }

  }


  trait Sequential[T] extends LimitedHorizon[T] with PartialSolutionsBuffer[T] {
    self : A_*[T] =>

    def handlePartialSolution(ps: PartialSolution) = {
      guardPartialSolution(ps)
      popBestPartialSolution()
        .map(RecFunc.Rec.apply)
        .getOrElse(RecFunc.Ret(Failure(AStarException("no more partial solutions to search")) -> NoHistory()))
    }

    protected def execSearchLH(f: RecFunc[T, Result])(state: T): Result = RecFunc.TCO(state, f)
  }

  /** uses akka actors
   */
  trait Parallel[T] extends LimitedHorizon[T] {
    self: A_*[T] =>

    def maxExecTime: FiniteDuration
    def executorPoolSize: Int
    
    protected def aFactory: ActorRefFactory
    
    protected case class InitExec(state: T, promise: Promise[Result], f: RecFunc[T, Result])
    protected case class Exec(state: T, f: RecFunc[T, Result])
    protected case class Finished(res: Either[PartialSolution, Try[T]])

//    protected case class SaveHistory(hist: History[T])
//    protected case object ClearHistory
//    protected case object ListHistory

    protected case object AlreadyRunning extends Exception("A* is already running in parallel, use another instance")
    protected case object NoExecutor extends Exception("A* has no free concurrent executors")

    protected val executor = new ScopedState[ActorRef](null)

//    protected val histScope = new ScopedState[HistoryManagement](???)

    protected abstract class Controller extends Actor with ActorLogging{
      protected var promiseOpt: Option[Promise[Result]] = None
      protected var func: RecFunc[T, Result] = null

      protected def getFree: Option[ActorRef]
      protected def free: ActorRef => Unit

      protected def guardPartialSolution: PartialSolution => Unit
      protected def nextTask: Option[T]

      def receive: Receive = {
        case InitExec(_, promise, _) if promiseOpt.isDefined => promise.failure(AlreadyRunning)
        case InitExec(init, promise, f) => 
          promiseOpt = Option(promise)
          func = f
          log.debug("InitExec")
          getFree match {
            case Some(exec) => exec ! Exec(init, f)
            case _          => promise.failure(NoExecutor)
          }
        case _ if promiseOpt.isEmpty =>
          log.debug("promiseOpt.isEmpty")
          free(sender())
        case Finished(Left(ps)) =>
          log.debug("Finished(Left(ps))")
          guardPartialSolution(ps)
          nextTask match{
            case Some(next) => sender() ! Exec(next, func)
            case _          => free(sender())
          }
        case Finished(Right(res)) =>
          log.debug("Finished(Right(res))")
          promiseOpt.get.success(res -> NoHistory())
          promiseOpt = None
          func = null
      }
    }

    protected class Executor(val controller: ActorRef) extends Actor with ActorLogging{
      def receive: Actor.Receive = {
        case Exec(st, f) =>
          log.debug("Exec")
          executor.doWith(self){ execSearchLH(f)(st) }
      }
    }

    protected lazy val controllerRef: ActorRef = aFactory.actorOf(controllerProps)

    def handlePartialSolution(ps: PartialSolution): RecFunc.Res[T, Result] = {
      executor.get.log("handlePartialSolution " +  executor.get)
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
      println("search")
      controllerRef ! InitExec(initial, promise, searchLH)
      println("await")
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

      val partialSolutions = ListBuffer.empty[T]
      
      protected def getFree: Option[ActorRef] = freePool.headOption
      protected def free: ActorRef => Unit = freePool +:= _

      protected def guardPartialSolution: PartialSolution => Unit = ps => partialSolutions ++= ps.best
      protected def nextTask: Option[T] =
        if (partialSolutions.nonEmpty) Some(partialSolutions.remove(0))
        else None
    })
    
    protected def executorProps(parent: ActorRef) = Props(new Executor(parent))

//    def HistoryManagement: HistoryManagement = new HistoryManagement{
//      def saveHistory(h: History[T]): Unit = ???
//
//      def listHistory: List[History[T]] = ???
//
//      def clearHistory(): Unit = ???
//    }
  }
}