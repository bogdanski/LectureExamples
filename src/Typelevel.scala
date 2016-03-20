package com.zenecture.playground

import com.zenecture.playground.Number._

/**
 * @author bogdanski
 * @since 17.09.15
 */


object Number {

  trait Number

  case class Next[+N <: Number]() extends Number

  type _0 = Number
  type _1 = Next[_0]
  type _2 = Next[_1]
  type _3 = Next[_2]
  type _4 = Next[_3]
  type _5 = Next[_4]

}


trait Sum[Left <: Number, Right <: Number, Res <: Number]

object Sum {

  def apply[Left <: Number, Right <: Number, Res <: Number](implicit sum: Sum[Left, Right, Res]): Sum[Left, Right, Res] = sum

  //type Aux[A <: Number, B <: Number, C <: Number] = Sum[A, B] { type Res = C }

  implicit def seed[Right <: Number]: Sum[_0, Right, Right] = new Sum[_0, Right, Right] {}

  implicit def plus[Left <: Number, Right <: Number, Res <: Number](implicit sum: Sum[Left, Next[Right], Res]): Sum[Next[Left], Right, Res] =
    new Sum[Next[Left], Right, Res] {}

}


trait Diff[Left <: Number, Right <: Number, Res <: Number]

object Diff {

  def apply[Left <: Number, Right <: Number, Res <: Number](implicit diff: Diff[Left, Right, Res]): Diff[Left, Right, Res] = diff

  implicit def seed[Right <: Number]: Diff[Right, _0, Right] = new Diff[Right, _0, Right] {}

  implicit def diff1[Left <: Number, Right <: Number, Res <: Number](implicit diff: Diff[Left, Right, Res]) =
    new Diff[Next[Left], Next[Right], Res] {}
}

/*

505 514 523 532 541 550
404 413 422 431 440
303 312 321 330
202 211 220
101 100
000

*/


object Main extends App {

  val a = Sum[_0, _1, _1]

  println(a)

  val b = Diff[_5, _0, _5]

}
