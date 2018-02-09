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

package com.sequoiareasoner.owlapi;

/** The available options for handling unsupported ontology features. Unsupported features include datatypes, class and
  * property assertions, and SWRL rules.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
public enum UnsupportedFeatureTreatment {
    /** Specifies that an exception is to be thrown when an unsupported ontology feature is encountered. */
    THROW_EXCEPTION,
    /** Specifies that unsupported ontology features should be ignored. */
    IGNORE,
}
