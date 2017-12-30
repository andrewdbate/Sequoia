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

package com.sequoiareasoner.protege.ui

import java.awt.BorderLayout
import javax.swing.{ImageIcon, JFrame, WindowConstants}

import org.protege.editor.core.ui.util.Icons

/** A popup window to display the messages reported from Sequoia.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class EventTraceFrame extends JFrame("Sequoia Info Panel") {
  // Set the Protege icon to the new frame.
  setIconImage(Icons.getIcon("logo32.gif").asInstanceOf[ImageIcon].getImage)

  private[this] val eventTracePanel = new EventTracePanel

  setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE)
  setLayout(new BorderLayout)
  add(eventTracePanel, BorderLayout.CENTER)
  setSize(500, 500)
  pack

  /** Add a message reported from Sequoia to the window.
    *
    * @param message  the message to add.
    */
  def addEventToDisplay(message: String): Unit = eventTracePanel.addEventToDisplay(message)

}
