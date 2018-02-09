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

package com.sequoiareasoner.owlapi

trait UnsupportedAPIMethodHandler {

  /** This method is to be called when an unsupported OWL API method is invoked.
    *
    * @param method  the method which is not supported
    */
  def handleMethod(method: String): Nothing

  /** This method is to be called when an unsupported OWL API method is invoked.
    *
    * @param method  method which some case is not supported
    * @param detail  the reason why the case is not supported
    */
  def handleMethodWithDetail(method: String, detail: String): Nothing

}

/** The default handling for unsupported OWL API method invocations.
  *
  */
class DefaultUnsupportedAPIMethodHandler extends UnsupportedAPIMethodHandler with Serializable  {

  protected[this] def reportUnsupported(method: String): Unit = {}

  protected[this] def reportUnsupported(method: String, detail: String): Unit = {}

  /** Throw an exception when an unsupported OWL API method is invoked.
    *
    * @param method  the method which is not supported
    * @throws `UnsupportedOperationException` saying that this method is not supported
    */
  final override def handleMethod(method: String): Nothing = {
    reportUnsupported(method)
    throw new SequoiaUnsupportedException(s"Unsupported method: OWL API reasoner method is not implemented: $method.")
  }

  /** Throw an exception when an unsupported OWL API method is invoked.
    *
    * @param method  method which some case is not supported
    * @param detail  the reason why the case is not supported
    * @throws `UnsupportedOperationException` saying that this method is not supported
    */
  final override def handleMethodWithDetail(method: String, detail: String): Nothing = {
    reportUnsupported(method: String, detail: String)
    throw new SequoiaUnsupportedException(s"Unsupported method: OWL API reasoner method is not implemented: $method: $detail.")
  }

}
