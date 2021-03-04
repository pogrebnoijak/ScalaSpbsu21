package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList.{MyNil, undef}

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[T2](f: T => T2): MyGenericList[T2]
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

case class HList[T](head: T, tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T]         = if (n <= 0) this else tail.drop(n-1)
  override def take(n: Int): MyGenericList[T]         = if (n <= 0) MyNil else HList[T](head, tail.take(n-1))
  override def map[T2](f: T => T2): MyGenericList[T2] = HList[T2](f(head), tail.map(f))
}

case object MyNilList extends MyGenericList[Nothing] {
  override def head: Nothing                              = undef
  override def tail: MyGenericList[Nothing]               = undef
  override def drop(n: Int): MyGenericList[Nothing]       = if (n == 0) this else undef
  override def take(n: Int): MyGenericList[Nothing]       = if (n == 0) this else undef
  override def map[T](f: Nothing => T): MyGenericList[T]  = MyNil
}
