package feh.util

/** A recursive function container. */
trait RecFunc[A, B] extends (A => RecFunc.Res[A, B])

object RecFunc{
  /** Recursive function result. */
  sealed trait Res[+A, +B]
  /** Recursive function result: recursive call. */
  case class Rec[A](a: A) extends Res[A, Nothing]
  /** Recursive function result: return value. */
  case class Ret[B](b: B) extends Res[Nothing, B]

  def apply[A, B](f: A => RecFunc.Res[A, B]): RecFunc[A, B] = new RecFunc[A, B] {
    def apply(v1: A) = f(v1)
  }

  /** Run the [[RecFunc]] with Tail Call Optimisation. */
  object TCO{

    def apply[A, B](initial: A, f: RecFunc[A, B]): B = {
      var flag = true
      var arg  = initial
      var res: B = null.asInstanceOf[B]

      while(flag)
        f(arg) match {
          case Rec(a) => arg = a
          case Ret(b) => res = b; flag = false
        }

      res
    }
  }

}
