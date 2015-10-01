package feh.tec.astar

import java.awt.{Dimension, Point}

import scala.collection.mutable
import feh.util._

trait VisualizeHistory[T] {
  vis =>

  val drawState: VisualizeState[T]

  protected def heuristic: T => Any
  protected def description: T => String
  protected def depthOf: T => Int

  def drawHistory(h: History[T])

  def abstractHistoryTree(h: History[T]): AbstractHistoryTree[HNode] = {
    // depth -> state (at this depth) -> children (should be on the next depth)
    val acc = mutable.HashMap.empty[Int, mutable.HashMap[(T, Int), Set[T]]]

    def putInAcc: Int => HistoryEntry[T] => Unit =
      order => {
        case HistoryEntry(state, children) =>
          val depth = depthOf(state)
          acc.getOrElseUpdate(depth, mutable.HashMap((state, order) -> children))
             .getOrElseUpdate(state -> order, children)
      }

    h.get.zipWithIndex foreach (Function uncurried flip(putInAcc)).tupled

    val accOrd = acc.toList.sortBy(_._1)
    val ((root, 0), _) = accOrd.head.ensuring(_._2.size == 1)._2.head
    val rootNode = new HNode(root, 0, None)

    val parentOf = mutable.HashMap.empty[T, HNode]
    val nodeOf   = mutable.HashMap(root -> rootNode)

    def buildHTree(depth: Int, level: mutable.HashMap[(T, Int), Set[T]]): Unit =
      for {
        ((state, order), children) <- level
        parent = parentOf.get(state)
        node   = nodeOf(state)
      } {
        node.orderUpd(order)

        for (child <- children) {
          val n = new HNode(child, depth+1, Some(node))
          parentOf += child -> node
          nodeOf += child -> n

          node.childrenUpd(_ + n)
        }
      }

    accOrd.foreach((buildHTree _).tupled)

    new AbstractHistoryTree[HNode]{
      lazy val root = nodeOf(acc(0).head._1.ensuring(_._2 == 0)._1)
    }
  }

  trait AbstractHistoryTree[Node <: AbstractHistoryNode[Node]]{
    val root: Node

    lazy val byDepth: Map[Int, Seq[Node]] = {
      val acc = mutable.HashMap.empty[Int, Seq[Node]]

      def fillMap(depth: Int, nodes: Seq[Node]): Unit =
        if (nodes.nonEmpty) {
          acc += depth -> nodes
          val next = nodes.flatMap(_.children)
          fillMap(depth + 1, next)
        }
      fillMap(0, root :: Nil)
      acc.toMap
    }

    object Debug{
      var depthIndent  = ":    "
      var alwaysIndent = "|--> "

      def listTree() = Y[(Int, Set[Node]), Unit](
        rec => {
          case (depth, nodes) => for{
            node <- nodes
          }{
            println(depthIndent*depth + alwaysIndent + node.toString)
            rec(depth+1, node.children)
          }
        }
      )(0 -> Set(root))
    }
  }

  trait AbstractHistoryNode[Node <: AbstractHistoryNode[Node]]{
    def order: Option[Int]
    def state: T
    def depth: Int
    def parent: Option[Node]
    def children: Set[Node]

    lazy val description = vis.description(state)
    lazy val heuristic = vis.heuristic(state).toString
  }

  trait HistoryNode[Node <: HistoryNode[Node]] extends AbstractHistoryNode[Node]{
    /** left-upper corner */
    def position: Point
  }

  class HNode(val state: T,
              val depth: Int,
              val parent: Option[HNode]) extends HistoryNode[HNode]
  {

    protected var childrenVar: Set[HNode] = Set()
    protected var positionOpt: Option[Point] = None

    protected var orderVar: Option[Int] = None

    def order = orderVar
    def orderUpd(ord: Int) = orderVar = Option(ord)

    /** left-upper corner */
    def position = positionOpt.get
    def positionDefined = positionOpt.isDefined
    def positionUpd(p: Point) = positionOpt = Option(p)

    def children = childrenVar
    def childrenUpd(f: Set[HNode] => Set[HNode]) = childrenVar = f(childrenVar)

    override lazy val toString = "HNode(" +
                                 s"${order map("order="+_+", ") getOrElse ""}${parent.map(_ => "").getOrElse("is root, ")}" +
                                 s"depth=$depth, $state, $description, heuristic=$heuristic" +
                                 s", childen=${children.size}, position=${positionOpt getOrElse "undefined"}"
  }



}

trait VisualizeState[T]{

  /** Draw the `state` at point `p` (left-upper corner) with given `scale`
    *
    * @param state the state to visualize.
    * @param point where to draw,
    * @param scale of the visualization. 1 by default. (not supported yet)
    */
  def drawAt(state: T, point: Point, scale: Float = 1)

  /** The size of visualization with scale 1.
   */
  def normalSize: Dimension

}

//abstract class AWT