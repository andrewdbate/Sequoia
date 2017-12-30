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

package com.sequoiareasoner.kernel.structural

import com.sequoiareasoner.kernel.owl.model.{Axiom, ClassExpression}
import com.sequoiareasoner.kernel.reasoner.SequoiaRuntimeException

/** This exception is to be thrown whenever an unsupported axiom is encountered and the reasoner is
  * set to throw an exception on unsupported axioms.
  */
final class UnsupportedAxiomException(ax: Axiom)
  extends SequoiaRuntimeException(s"Sequoia does not support the axiom $ax.")

/** This exception is to be thrown whenever an unsupported class expression is encountered and the reasoner is
  * set to throw an exception on unsupported class expressions.
  */
final case class UnsupportedClassExpressionException(ce: ClassExpression)
  extends SequoiaRuntimeException(s"Sequoia does not support the class expression $ce.")

trait UnsupportedFeatureObserver extends Serializable {
  def reportUnsupported(ax: Axiom): Unit
  def reportUnsupported(ce: ClassExpression): Unit
}

final class UnsupportedFeatureObserverThrowException extends UnsupportedFeatureObserver {
  override def reportUnsupported(ax: Axiom): Nothing =
    throw new UnsupportedAxiomException(ax)
  override def reportUnsupported(ce: ClassExpression): Nothing =
    throw new UnsupportedClassExpressionException(ce)
}

final class UnsupportedFeatureObserverIgnore extends UnsupportedFeatureObserver {
  override def reportUnsupported(ax: Axiom): Unit = {}
  override def reportUnsupported(ce: ClassExpression): Unit = {}
}
