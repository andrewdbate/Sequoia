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

package com.sequoiareasoner.cli.util

import java.io.{PrintWriter, StringWriter, Writer}

/** A table data structure that has a list of column names and list of data rows.
  * The only purpose of this class is to print the data in a tabular format.
  */
final class TablePrinter[A](colNames: Seq[String], rightAligned: Seq[Boolean], entries: Seq[Seq[A]]) {
  require(rightAligned.length == colNames.size, s"Alignment has ${rightAligned.length} elements but table has ${colNames.size} columns.")
  for (row <- entries)
    require(row.size == colNames.size, s"Row has ${row.size} elements but table has ${colNames.size} columns.")

  private[this] val columnSeparator: String = " | "

  private[this] def toString[B](xs: Seq[B]): Seq[String] = xs map {
    name => if (name == null) "<null>" else name.toString
  }

  private[this] def columnWidths: Seq[Int] = (colNames +: entries).transpose map {
    column => (toString(column) map {_.length}).max
  }

  private[this] val lineWidth: Int =
    columnWidths.sum + ((columnWidths.length - 1) * columnSeparator.length)

  def print(writer: Writer): Unit = printText(writer)

  private[this] def printText(writer: Writer): Unit = {
    val pw: PrintWriter = writer match {
      case pw: PrintWriter => pw
      case _ => new PrintWriter(writer)
    }
    printRow(pw, colNames)
    pw.print("=" * lineWidth)
    pw.println
    for (rowData <- entries)
      printRow(pw, toString(rowData))
    pw.flush
  }

  private[this] def printRow(pw: PrintWriter, row: Seq[String]) {
    for (col <- 0 until row.length) {
      val s: String = row(col)
      val pad: Int = columnWidths(col)
      val buff = new StringBuilder
      if (col > 0) buff.append(columnSeparator)
      val padding = " " * (pad - s.length)
      if (rightAligned(col))
        buff.append(padding).append(s)
      else
        buff.append(s).append(padding)
      pw.print(buff)
    }
    pw.println
  }

  override def toString: String = {
    val sw = new StringWriter
    printText(sw)
    sw.toString
  }

}
