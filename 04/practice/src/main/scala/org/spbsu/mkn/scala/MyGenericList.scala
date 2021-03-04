package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList.{MyNil, sum, turnerInt, undef}
import java.util.Comparator

sealed trait MyGenericList[+T] {
  def head: T
  def tail: MyGenericList[T]
  def drop(n: Int): MyGenericList[T]
  def take(n: Int): MyGenericList[T]
  def map[T2](f: T => T2): MyGenericList[T2]
  def filter(f: T => Boolean): MyGenericList[T]
  def ::[T2 >: T](elem: T2): MyGenericList[T2]  = HList[T2](elem, MyNil)
  def foldLeft[B](z: B)(op: (B, T) => B): B     = this match {
    case HList(x, xs) => xs.foldLeft(op(z, x))(op)
    case MyNilList    => z
  }
}

object MyGenericList {
  def undef: Nothing                                = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[T](seq: Seq[T]): MyGenericList[T]     = seq.foldRight(MyNil[T])(HList[T])
  def size[T](genList: MyGenericList[T]): Int       = genList.foldLeft(0){(a, _) => a + 1}
  def sum[T, B](genList: MyGenericList[T])(b: B)(implicit turn: (B,T) => B): B = genList match {
    case HList(_, _)  => genList.foldLeft(b)((bb, t) => turn(bb, t))
    case MyNilList    => undef
  }
  def MyNil[T]: MyGenericList[T]                    = MyNilList.asInstanceOf[MyGenericList[T]]
  def sort[T](list: MyGenericList[T])(implicit comparator: Comparator[T]): MyGenericList[T] = list match {
    case HList(head, tail) => plus(sort(tail.filter(comparator.compare(head, _) <= 0))(comparator),
      HList(head, sort(tail.filter(comparator.compare(head, _) > 0))(comparator)))
    case MyNilList         => MyNilList
  }
  def plus[T](list1: MyGenericList[T], list2: MyGenericList[T]): MyGenericList[T] = list1 match {
    case HList(head, tail)  => HList(head, plus(tail, list2))
    case MyNilList          => list2
  }

  implicit def ComparatorGList[T]: Comparator[MyGenericList[T]] =
    (a: MyGenericList[T], b: MyGenericList[T])  => size(a) compare size(b)
  implicit def turnerInt: (Int, Int)            => Int = {_ + _}
  implicit def turnerStr: (Int, String)         => Int = {_ + _.length}
}

case class HList[T](head: T, tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T]             = if (n <= 0) this else tail.drop(n-1)
  override def take(n: Int): MyGenericList[T]             = if (n <= 0) MyNil else HList(head, tail.take(n-1))
  override def map[T2](f: T => T2): MyGenericList[T2]     = HList(f(head), tail.map(f))
  override def filter(f: T => Boolean): MyGenericList[T]  = if (f(head)) HList(head, tail.filter(f)) else tail.filter(f)
}

case object MyNilList extends MyGenericList[Nothing] {
  override def head: Nothing                                          = undef
  override def tail: MyGenericList[Nothing]                           = undef
  override def drop(n: Int): MyGenericList[Nothing]                   = if (n == 0) this else undef
  override def take(n: Int): MyGenericList[Nothing]                   = if (n == 0) this else undef
  override def map[T](f: Nothing => T): MyGenericList[T]              = MyNil
  override def filter(f: Nothing => Boolean): MyGenericList[Nothing]  = MyNil
}

object Comparator {
  def ComparatorGList2[T <: Int]: Comparator[MyGenericList[T]]  =
    (a: MyGenericList[T], b: MyGenericList[T]) => sum(a)(0) compare sum(b)(0)
  def turnerStr2: (String, String) => String                    = {_ + _}
}