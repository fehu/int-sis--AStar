package feh.tec.astar

import java.awt.{Dimension, Point}

import scala.collection.mutable
import feh.util._

trait VisualizeHistory[T] extends AwtHelper{
  vis =>

  val drawNode: VisualizeNode

  def distanceBetweenH: Int
  def distanceBetweenV: Int

  protected def heuristic: T => Any
  protected def description: T => String
  protected def depthOf: T => Int

  protected def setCanvasSize(dim: Dimension)
  protected def drawArrow(from: Point, to: Point)

  /** Draws the history tree.
   */
  def drawHistory(h: History[T]): Unit = {
    drawHistory(setPositions(abstractHistoryTree(h)))
  }
  
  def drawHistory(tr: HistoryTree[HNode]): Unit = {
    drawHistoryPrepare(tr)
    drawHistoryRepaint(tr)
  }

  def drawHistoryPrepare(tr: HistoryTree[HNode]): Unit = {
    val deepestN = tr.root.deepChildrenLeafs
    val width = deepestN * drawNode.size.width + (deepestN - 1) * distanceBetweenH
    val height = tr.byDepth.size * drawNode.size.height + (tr.byDepth.size - 1) * distanceBetweenV
    setCanvasSize(width -> height)
  }

  def drawHistoryRepaint(tr: HistoryTree[HNode]): Unit = {
    // draw nodes
    tr.byDepth.values.flatten.foreach(drawNode.draw)

    // draw arrows
    tr.byDepth.toList.sortBy(_._1).foreach {
      case (_, nodes) => nodes.foreach{
        node => node.children.foreach(child => drawArrow(shiftFullDown compose shiftRight apply  node.position,
                                                         shiftRight apply child.position))
      }
    }
  }
  
  protected def shiftLeft: Point => Point     = p => p.x - drawNode.size.width/2 -> p.y
  protected def shiftRight: Point => Point    = p => p.x + drawNode.size.width/2 -> p.y
  protected def shiftFullDown: Point => Point = p => p.x -> (p.y + drawNode.size.height)

  def setPositions(tr: HistoryTree[HNode]): tr.type = {
    // resolve the `deepChildrenLeafs`
    tr.byDepth.toList.sortBy(-_._1).foreach{
      case (depth, nodes) => nodes.foreach {
        case node if node.children.isEmpty => node.deepChildrenLeafsUpd(1)
        case node =>
          val x = node.children.toSeq.map{
            case child if child.deepChildrenLeafs == -1 =>
              sys.error("not set `deepChildrenLeafs` at depth " + (depth-1).toString)
            case child => child.deepChildrenLeafs
          }
          node.deepChildrenLeafsUpd(x.sum)
      }
    }

    // each state at depth =>
    //    each child of state  =>
    //        place child in the middle of corresponding totalWidth

    def totalWidth(node: HNode) = {
      val n = node.deepChildrenLeafs
      n * drawNode.size.width + (n - 1) * distanceBetweenH
    }
    def pointsInMiddle(nodes: Seq[HNode], relTo: HNode): Seq[Point] = {
      val y = relTo.position.y + drawNode.size.height + distanceBetweenV
      val firstX = relTo.position.x - totalWidth(relTo) / 2

      ((List.empty[Point], firstX) /: nodes){
        case ((acc, xacc), node) =>
          val (x, newXAcc) = xMiddle(totalWidth(node), xacc)

          ((x -> y : Point) :: acc) -> newXAcc
      }._1.reverse
    }
    def xMiddle(width: Int, acc: Int): (Int, Int) = acc + width / 2 -> (acc + width + distanceBetweenH)

    tr.root.positionUpd(shiftLeft(totalWidth(tr.root) / 2 -> 0))

    tr.byDepth.toList.sortBy(_._1).foreach{
      case (_, nodes) => nodes.foreach{
        node =>
          val children = node.children.toSeq
          children.zip(pointsInMiddle(children, node)).foreach{
            case (child, point) => child.positionUpd(point)
          }
      }
    }

    tr
  }
  
  def abstractHistoryTree(h: History[T]): HistoryTree[HNode] = {
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

    new HistoryTree[HNode]{
      lazy val root = nodeOf(acc(0).head._1.ensuring(_._2 == 0)._1)
    }
  }

  trait HistoryTree[Node <: AbstractHistoryNode[Node]]{
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
    def deepChildrenLeafs: Int
  }

  class HNode(val state: T,
              val depth: Int,
              val parent: Option[HNode]) extends HistoryNode[HNode]
  {

    protected var childrenVar: Set[HNode] = Set()
    protected var positionOpt: Option[Point] = None
    protected var deepChildrenLeafsVar: Option[Int] = None
    protected var orderVar: Option[Int] = None

    def order = orderVar
    def orderUpd(ord: Int) = orderVar = Option(ord)

    /** left-upper corner */
    def position = positionOpt.get
    def positionDefined = positionOpt.isDefined
    def positionUpd(p: Point) = positionOpt = Option(p)

    def children = childrenVar
    def childrenUpd(f: Set[HNode] => Set[HNode]) = childrenVar = f(childrenVar)
    
    def deepChildrenLeafs = deepChildrenLeafsVar.getOrElse(-1)
    def deepChildrenLeafsUpd(n: Int) = deepChildrenLeafsVar = Option(n)

    override lazy val toString = "HNode(" +
                                 s"${order map("order="+_+", ") getOrElse ""}${parent.map(_ => "").getOrElse("is root, ")}" +
                                 s"depth=$depth, $state, $description, heuristic=$heuristic" +
                                 s", childen=${children.size}, position=${positionOpt getOrElse "undefined"}" +
                                 s", deepChildrenLeafs=$deepChildrenLeafs"
  }

  
  trait VisualizeNode{

    /** Draws the node.
      */
    def draw(node: HNode)

    /** The size of visualization.
      */
    def size: Dimension

  }


}