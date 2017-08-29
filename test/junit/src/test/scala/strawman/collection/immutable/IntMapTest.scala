package strawman.collection
package immutable

class IM[T] {
  def map[B, U](f: ((Int, T)) => (B, U)): Int = 1
  def map[U](f: ((Int, T)) => (Int, U)): String = "a"
}

abstract class BTypes { trait Foo }

class C[BT <: BTypes](val bt: BT) {
  import bt.Foo
  new IM[String].map { case (i, x) => (i+1, x) }
  new IM[Foo].map { case (i, x) => (i+1, x) }
}
