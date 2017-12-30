/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This file is part of Sequoia, an OWL 2 reasoner that supports the SRIQ subset of OWL 2 DL.
 * Copyright 2017 by Andrew Bate <code@andrewbate.com>.
 *
 * This code is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 only,
 * as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License version 3 for more details (a copy is
 * included in the LICENSE file that accompanied this code).
 *
 * You should have received a copy of the GNU General Public License
 * version 3 along with this work.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * This file is available under and governed by the GNU General Public
 * License version 3 only, as published by the Free Software Foundation.
 * However, the following notice accompanied the original version of this
 * file:
 *
 * Copyright (c) 2016, Andrew Bate, University of Oxford <andrew.bate@cs.ox.ac.uk>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the copyright holder nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.sequoiareasoner

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

/** This package provides fast array operations in a declarative style.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
package object arrayops {

  private[this] def cexistsImplCommon[A](c: Context)(arr: c.Expr[Array[A]], predicate: c.universe.Tree): c.Expr[Boolean] = {
    import c.universe._
    val tree =
    q"""
      var indexVar: Int = 0
      val lengthVal: Int = $arr.length
      var existsVar: Boolean = false
      while (!existsVar && indexVar < lengthVal) {
        existsVar = ($predicate)($arr(indexVar))
        indexVar += 1
      }
      existsVar
    """
    c.Expr(tree)
  }

  def cexistsImpl[A](c: Context)(arr: c.Expr[Array[A]])(pred: c.Expr[A => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    cexistsImplCommon(c)(arr, q"$pred")
  }

  def cexists[A](arr: Array[A])(pred: A => Boolean): Boolean = macro cexistsImpl[A]

  def ccontainsImpl[A : c.WeakTypeTag, A1 >: A](c: Context)(arr: c.Expr[Array[A]], elem: c.Expr[A1]): c.Expr[Boolean] = {
    import c.universe._
    cexistsImplCommon(c)(arr, q"{ x: ${weakTypeOf[A]} => x == $elem }")
  }

  def ccontains[A, A1 >: A](arr: Array[A], elem: A1): Boolean = macro ccontainsImpl[A,A1]

  def cforallImpl[A](c: Context)(arr: c.Expr[Array[A]])(pred: c.Expr[A => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val tree =
    q"""
      var indexVar: Int = 0
      val lengthVal: Int = $arr.length
      var forallVar: Boolean = true
      while (forallVar && indexVar < lengthVal) {
        forallVar = $pred($arr(indexVar))
        indexVar += 1
      }
      forallVar
    """
    c.Expr(tree)
  }

  def cforall[A](arr: Array[A])(pred: A => Boolean): Boolean = macro cforallImpl[A]

  private[this] def cforallRangeImplCommon(c: Context)(start: c.Expr[Int], end: c.Expr[Int], step: c.Expr[Int])(pred: c.Expr[Int => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val tree =
      q"""
      var index: Int = $start
      var forallVar: Boolean = true
      while (forallVar && index < $end) {
        forallVar = $pred(index)
        index += $step
      }
      forallVar
    """
    c.Expr(tree)
  }

  def cforallRangeStepImpl(c: Context)(start: c.Expr[Int], end: c.Expr[Int], step: c.Expr[Int])(pred: c.Expr[Int => Boolean]): c.Expr[Boolean] =
    cforallRangeImplCommon(c)(start, end, step)(pred)

  def cforall(start: Int, end: Int, step: Int)(pred: Int => Boolean): Boolean = macro cforallRangeStepImpl

  def cforallRange1Impl(c: Context)(start: c.Expr[Int], end: c.Expr[Int])(pred: c.Expr[Int => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    cforallRangeImplCommon(c)(start, end, c.Expr(q"1"))(pred)
  }

  def cforall(start: Int, end: Int)(pred: Int => Boolean): Boolean = macro cforallRange1Impl

  def csumImpl(c: Context)(start: c.Expr[Int], end: c.Expr[Int])(f: c.Expr[Int => Int]): c.Expr[Int] = {
    import c.universe._
    val tree =
      q"""
      var index: Int = $start
      var sumVar: Int = 0
      while (index < $end) {
        sumVar += $f(index)
        index += 1
      }
      sumVar
    """
    c.Expr(tree)
  }

  def csum(start: Int, end: Int)(f: Int => Int): Int = macro csumImpl

  private[this] def cfilterImplCommon[A : c.WeakTypeTag](c: Context)(arr: c.Expr[Array[A]], pred: c.Expr[A => Boolean], isFlipped: Boolean): c.Expr[Array[A]] = {
    import c.universe._
    val tree =
    q"""
      var readIndexVar: Int = 0
      var writeIndexVar: Int = 0
      val lengthVal: Int = $arr.length
      val array1Val = new Array[${weakTypeOf[A]}](lengthVal)
      while (readIndexVar < lengthVal) {
        if ($pred($arr(readIndexVar)) != $isFlipped) {
          array1Val(writeIndexVar) = $arr(readIndexVar)
          writeIndexVar += 1
        }
        readIndexVar += 1
      }
      if (writeIndexVar == lengthVal) {
        array1Val
      } else {
        val array2Val = new Array[${weakTypeOf[A]}](writeIndexVar)
        System.arraycopy(array1Val, 0, array2Val, 0, writeIndexVar)
        array2Val
      }
    """
    c.Expr[Array[A]](tree)
  }

  def cfilterImpl[A : c.WeakTypeTag](c: Context)(arr: c.Expr[Array[A]])(pred: c.Expr[A => Boolean]): c.Expr[Array[A]] =
    cfilterImplCommon(c)(arr, pred, isFlipped = false)

  def cfilter[A](arr: Array[A])(pred: A => Boolean): Array[A] = macro cfilterImpl[A]

  def cfilterNotImpl[A : c.WeakTypeTag](c: Context)(arr: c.Expr[Array[A]])(pred: c.Expr[A => Boolean]): c.Expr[Array[A]] =
    cfilterImplCommon(c)(arr, pred, isFlipped = true)

  def cfilterNot[A](arr: Array[A])(pred: A => Boolean): Array[A] = macro cfilterNotImpl[A]

  def ccollectImpl[B <: A : c.WeakTypeTag, A](c: Context)(arr: c.Expr[Array[A]]): c.Expr[Array[B]] = {

    import c.universe._
    val t = c.weakTypeOf[B].typeSymbol.name.toTypeName
    val tree =
    q"""
      var readIndexVar: Int = 0
      var writeIndexVar: Int = 0
      val lengthVal: Int = $arr.length
      val array1Val = new Array[${weakTypeOf[B]}](lengthVal)
      while (readIndexVar < lengthVal) {
        if ($arr(readIndexVar).isInstanceOf[${weakTypeOf[B]}]) {
          array1Val(writeIndexVar) = $arr(readIndexVar).asInstanceOf[${weakTypeOf[B]}]
          writeIndexVar += 1
        }
        readIndexVar += 1
      }
      if (writeIndexVar == lengthVal) {
        array1Val
      } else {
        val array2Val = new Array[${weakTypeOf[B]}](writeIndexVar)
        System.arraycopy(array1Val, 0, array2Val, 0, writeIndexVar)
        array2Val
      }
    """
    c.Expr[Array[B]](tree)
  }

  def ccollect[B <: A, A](arr: Array[A]): Array[B]  = macro ccollectImpl[B, A]

  def cforeachImpl[A, U](c: Context)(arr: c.Expr[Array[A]])(pred: c.Expr[A => U]): c.Expr[Unit] = {
    import c.universe._
    val tree =
    q"""
      var indexVar: Int = 0
      val lengthVal: Int = $arr.length
      while (indexVar < lengthVal) {
        $pred($arr(indexVar))
        indexVar += 1
      }
    """
    c.Expr(tree)
  }

  def cforeach[A, U](arr: Array[A])(pred: A => U): Unit = macro cforeachImpl[A, U]

  def cmapImpl[A, B : c.WeakTypeTag](c: Context)(arr: c.Expr[Array[A]])(f: c.Expr[A => B]): c.Expr[Array[B]] = {
    import c.universe._
    val tree =
    q"""
      var indexVar: Int = 0
      val lengthVal: Int = $arr.length
      val arrayVal = new Array[${weakTypeOf[B]}](lengthVal)
      while (indexVar < lengthVal) {
        arrayVal(indexVar) = $f($arr(indexVar))
        indexVar += 1
      }
      arrayVal
    """
    c.Expr[Array[B]](tree)
  }

  def cmap[A,B](arr: Array[A])(f: A => B): Array[B] = macro cmapImpl[A,B]

  def crangeImpl(c: Context)(start: c.Expr[Int], end: c.Expr[Int])(body: c.Expr[Int => Unit]): c.Expr[Unit] = {
    import c.universe._
    val tree =
      q"""
      var index: Int = $start
      while (index < $end) {
        $body(index)
        index += 1
      }
    """
    c.Expr(tree)
  }

  def crange(start: Int, end: Int)(body: Int => Unit): Unit = macro crangeImpl

  def cyield1Impl[A : c.WeakTypeTag](c: Context)(start: c.Expr[Int], end: c.Expr[Int])(f: c.Expr[Int => A]): c.Expr[Array[A]] = {
    import c.universe._
    val tree =
      q"""
      val arrayVal = new Array[${weakTypeOf[A]}]($end - $start)
      var index: Int = $start
      while (index < $end) {
        arrayVal(index) = $f(index)
        index += 1
      }
      arrayVal
    """
    c.Expr[Array[A]](tree)
  }

  def cyield[A](start: Int, end: Int)(f: Int => A): Array[A] = macro cyield1Impl[A]

  def cyield2Impl[A : c.WeakTypeTag](c: Context)(start1: c.Expr[Int], end1: c.Expr[Int], start2: c.Expr[Int => Int], end2: c.Expr[Int => Int])(f: c.Expr[(Int, Int) => A]): c.Expr[Array[A]] = {
    import c.universe._
    val tree =
      q"""
      var index1: Int = $start1
      val end1: Int = $end1
      var arrayValLength: Int = 0
      while (index1 < end1) {
        val start: Int = $start2(index1)
        val end: Int = $end2(index1)
        arrayValLength += end - start
        index1 += 1
      }
      val arrayVal = new Array[${weakTypeOf[A]}](arrayValLength)
      var arrayValIndex: Int = 0
      index1 = $start1
      while (index1 < end1) {
        val start: Int = $start2(index1)
        val end: Int = $end2(index1)
        var index2: Int = start
        while (index2 < end) {
          arrayVal(arrayValIndex) = $f(index1, index2)
          arrayValIndex += 1
          index2 += 1
        }
        index1 += 1
      }
      arrayVal
    """
    c.Expr[Array[A]](tree)
  }

  def cyield[A](start1: Int, end1: Int, start2: Int => Int, end2: Int => Int)(f: (Int, Int) => A): Array[A] = macro cyield2Impl[A]

}