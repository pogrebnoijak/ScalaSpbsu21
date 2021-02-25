package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList.{MyNil, undef}

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[T2 >: T](f: T2 => T2): MyGenericList[T2]
  def ::[T2 >: T](elem: T2): MyGenericList[T2]  = HList[T2](elem, MyNil)
  def foldLeft[B](z: B)(op: (B, T) => B): B     = this match {
    case HList(x, xs) => xs.foldLeft(op(z, x))(op)
    case _            => z
  }
}

object MyGenericList {
  def undef: Nothing                                = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T]     = seq.foldRight(MyNil[T])(HList[T])
  def size[T](genList: MyGenericList[T]): Int       = genList.foldLeft(0){(a, _) => a + 1}
  def sum[T <: Int](genList: MyGenericList[T]): Int = genList match {
    case HList(_, _)  => genList.foldLeft(0){_ + _}
    case _            => undef
  }
  def MyNil[T]: MyGenericList[T]                    = MyNilList.asInstanceOf[MyGenericList[T]]
}

case class HList[T](x: T, xs: MyGenericList[T]) extends MyGenericList[T] {
  override def head: T                                      = x
  override def tail: MyGenericList[T]                       = xs
  override def drop(n: Int): MyGenericList[T]               = if (n <= 0) this else tail.drop(n-1)
  override def take(n: Int): MyGenericList[T]               = if (n <= 0) MyNil else HList[T](x, xs.take(n-1))
  override def map[T2 >: T](f: T2 => T2): MyGenericList[T2] = HList[T2](f(x.asInstanceOf[T2]), xs.map(f))
}

case object MyNilList extends MyGenericList[Any] {
  override def head: Any                                  = undef
  override def tail: MyGenericList[Any]                   = undef
  override def drop(n: Int): MyGenericList[Any]           = if (n == 0) this else undef
  override def take(n: Int): MyGenericList[Any]           = if (n == 0) this else undef
  override def map[T >: Any](f: T => T): MyGenericList[T] = this
}
