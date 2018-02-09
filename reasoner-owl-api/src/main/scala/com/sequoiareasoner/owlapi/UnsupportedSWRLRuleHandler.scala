/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This file is part of Sequoia, an OWL 2 reasoner that supports the SRIQ subset of OWL 2 DL.
 * Copyright 2018 by Andrew Bate <code@andrewbate.com>.
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

import com.sequoiareasoner.kernel.logging.Logger
import org.semanticweb.owlapi.model.SWRLRule

trait UnsupportedSWRLRuleHandler {

  /** This method is to be called when a SWRL rule is encountered in the input ontology.
    *
    * @param rule  the rule which is not supported
    */
  def handleSWRLRule(rule: SWRLRule): Unit

}

/** Throws an exception when a SWRL rule is encountered in the input.
  *
  */
class UnsupportedSWRLRuleHandlerThrowException(logger: Logger) extends UnsupportedSWRLRuleHandler with Serializable {

  protected[this] def reportUnsupported(rule: SWRLRule): Unit = logger.error(s"SWRL rules are unsupported: $rule.")

  /** Throw an exception when a SWRL rule is encountered.
    *
    * @param rule  the rule which is not supported
    * @throws `UnsupportedOperationException` saying that this rule is not supported
    */
  final override def handleSWRLRule(rule: SWRLRule): Nothing = {
    reportUnsupported(rule)
    throw new SequoiaUnsupportedException(s"SWRL rules are unsupported: $rule.")
  }

}

/** Throws an exception when a SWRL rule is encountered in the input.
  *
  */
class UnsupportedSWRLRuleHandlerIgnore(logger: Logger) extends UnsupportedSWRLRuleHandler with Serializable {

  protected[this] def reportUnsupported(rule: SWRLRule): Unit = logger.warn(s"SWRL rules are unsupported: $rule.")

  /** Log a warning only when a SWRL rule is encountered.
    *
    * @param rule  the rule which is not supported
    */
  final override def handleSWRLRule(rule: SWRLRule): Unit = reportUnsupported(rule)

}
