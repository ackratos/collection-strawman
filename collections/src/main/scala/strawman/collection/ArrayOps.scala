package strawman
package collection

import java.util
import java.lang.System

import scala.{AnyVal, AnyRef, Array, ArrayIndexOutOfBoundsException, Byte, Boolean, Short, Float, Double, Long, Char, Unit, Int, throws}
import scala.Predef.???
import mutable.{ArrayBuffer, GrowableBuilder}

import scala.reflect.ClassTag

trait ArrayOps[A]
    extends IterableOnce[A]
    with IndexedSeqOps[A, immutable.IndexedSeq, Array[A]]
    with StrictOptimizedIterableOps[A, Seq, Array[A]]
    with ArrayLike[A] {

  val xs: Array[A]
  def toIterable = ArrayView(xs)
  protected[this] def coll: Array[A] = xs
  override def toSeq: immutable.Seq[A] = fromIterable(toIterable)

  def length = xs.length
  @throws[ArrayIndexOutOfBoundsException]
  def apply(i: Int) = xs.apply(i)

  override def view = ArrayView(xs)

  def elemTag: ClassTag[A] = ClassTag(xs.getClass.getComponentType)

  def iterableFactory = immutable.IndexedSeq

  protected[this] def fromTaggedIterable[B: ClassTag](coll: Iterable[B]): Array[B] = coll.toArray[B]
  protected[this] def fromSpecificIterable(coll: Iterable[A]): Array[A] = coll.toArray[A](elemTag)

  protected[this] def newSpecificBuilder() = ArrayBuffer.newBuilder[A]().mapResult(_.toArray(elemTag))

  override def knownSize = xs.length

  override def className = "Array"

  def map[B: ClassTag](f: A => B): Array[B] = fromTaggedIterable(View.Map(toIterable, f))

  def mapInPlace(f: A => A): Array[A] = {
    var i = 0
    while (i < xs.length) {
      xs.update(i, f(xs(i)))
      i = i + 1
    }
    xs
  }

  def flatMap[B: ClassTag](f: A => IterableOnce[B]): Array[B] = fromTaggedIterable(View.FlatMap(toIterable, f))

  def ++[B >: A : ClassTag](xs: Iterable[B]): Array[B] = fromTaggedIterable(View.Concat(toIterable, xs))

  def zip[B: ClassTag](that: Iterable[B]): Array[(A, B)] = fromTaggedIterable(View.Zip(toIterable, that))

  override final def slice(from: Int, until: Int): Array[A] = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= length)
      return emptyImpl
    val end = if (until > length) length else until
    sliceImpl(start, end)
  }

  protected def emptyImpl: Array[A]
  protected def sliceImpl(from: Int, until: Int): Array[A]
}

object ArrayOps {

  private val emptyByteArray = new Array[Byte](0)
  private val emptyShortArray = new Array[Short](0)
  private val emptyIntArray = new Array[Int](0)
  private val emptyLongArray = new Array[Long](0)
  private val emptyFloatArray = new Array[Float](0)
  private val emptyDoubleArray = new Array[Double](0)
  private val emptyUnitArray = new Array[Unit](0)
  private val emptyCharArray = new Array[Char](0)
  private val emptyBooleanArray = new Array[Boolean](0)

  /** A class of `ArrayOps` for arrays containing reference types. */
  final class ofRef[A <: AnyRef](override val xs: Array[A]) extends ArrayOps[A] {

    protected override def emptyImpl:Array[A] = util.Arrays.copyOf[A](xs,0)
    protected override def sliceImpl(from: Int, until: Int): Array[A] = util.Arrays.copyOfRange[A](xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `byte`s. */
  final class ofByte(override val xs: Array[Byte]) extends ArrayOps[Byte] {

    protected override def emptyImpl = emptyByteArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `short`s. */
  final class ofShort(override val xs: Array[Short]) extends ArrayOps[Short] {

    protected override def emptyImpl = emptyShortArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `char`s. */
  final class ofChar(override val xs: Array[Char]) extends ArrayOps[Char] {

    protected override def emptyImpl = emptyCharArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `int`s. */
  final class ofInt(override val xs: Array[Int]) extends ArrayOps[Int] {

    protected override def emptyImpl = emptyIntArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `long`s. */
  final class ofLong(override val xs: Array[Long]) extends ArrayOps[Long] {

    protected override def emptyImpl = emptyLongArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `float`s. */
  final class ofFloat(override val xs: Array[Float]) extends ArrayOps[Float] {

    protected override def emptyImpl = emptyFloatArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `double`s. */
  final class ofDouble(override val xs: Array[Double]) extends ArrayOps[Double] {

    protected override def emptyImpl = emptyDoubleArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays containing `boolean`s. */
  final class ofBoolean(override val xs: Array[Boolean]) extends ArrayOps[Boolean] {

    protected override def emptyImpl = emptyBooleanArray
    protected override def sliceImpl(from: Int, until: Int) = util.Arrays.copyOfRange(xs, from, until)
  }

  /** A class of `ArrayOps` for arrays of `Unit` types. */
  final class ofUnit(override val xs: Array[Unit]) extends ArrayOps[Unit] {

    protected override def emptyImpl = emptyUnitArray
    protected override def sliceImpl(from: Int, until: Int) = {
      // cant use util.Arrays.copyOfRange[Unit](repr, from, until) - Unit is special and doesnt compile
      val res = new Array[Unit](until-from)
      System.arraycopy(xs, from, res, 0, res.size)
      res
    }
  }
}

case class ArrayView[A](xs: Array[A]) extends IndexedView[A] {
  def length = xs.length
  @throws[ArrayIndexOutOfBoundsException]
  def apply(n: Int) = xs(n)
  override def className = "ArrayView"
}
