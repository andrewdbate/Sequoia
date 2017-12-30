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

import java.awt.Dimension
import javax.swing.{Box, BoxLayout, JCheckBox, JLabel, JOptionPane, JPanel}

import com.sequoiareasoner.buildinfo.BuildInfo
import com.sequoiareasoner.owlapi.DefaultUnsupportedAPIMethodHandler
import com.sequoiareasoner.kernel.owl.model.{Axiom, ClassExpression}
import com.sequoiareasoner.kernel.owl.printers.OWLFunctionalStylePrinter
import com.sequoiareasoner.kernel.structural.UnsupportedFeatureObserver
import com.sequoiareasoner.protege.SequoiaReasonerPreferences

/** Creates a window to log messages about logical features unsupported by Sequoia, or OWL API methods
  * that are unimplemented in the current version of Sequoia.
  *
  * In addition, unsupported OWL API methods will show a pop-up dialog to the user. This dialog displays information
  * on how to switch off some inference types after an error or a warning has been thrown within Sequoia.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class UnsupportedWarningDialogs extends DefaultUnsupportedAPIMethodHandler with UnsupportedFeatureObserver {

  private[this] val eventTraceFrame = new EventTraceFrame

  /** The information message to append to the log window.
    *
    * @param message  the message to append.
    */
  private[this] def appendMessage(message: String): Unit = {
    eventTraceFrame.addEventToDisplay(message)
    eventTraceFrame.setVisible(true)
  }

  override def reportUnsupported(ax: Axiom): Unit =
    appendMessage(s"Axiom Unsupported: ${OWLFunctionalStylePrinter.fssString(ax)}")

  override def reportUnsupported(ce: ClassExpression): Unit =
    appendMessage(s"Class Expression Unsupported: ${OWLFunctionalStylePrinter.fssString(ce)}")

  override def reportUnsupported(method: String): Unit = {
    showOWLAPIMessage
    appendMessage(s"Unsupported OWL API Method: $method")
  }

  override def reportUnsupported(method: String, detail: String): Unit = {
    showOWLAPIMessage
    appendMessage(s"Unsupported OWL API Method: $method $detail")
  }

  protected def showOWLAPIMessage: Unit =
    if (SequoiaReasonerPreferences.showUnsupportedOWLAPIMethodWarning) {
      val panel = new JPanel
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS))
      panel.add(new JLabel(s"<html><b>Prot&eacute;g&eacute; has called an OWL API method that Sequoia ${BuildInfo.version} does not support.</b></html>"))
      panel.add(Box.createRigidArea(new Dimension(0, 5)))
      val displayLabel: String =
        s"""|<html><div style="width:380px">
            |<p>
            |To avoid these warnings, go to <b>Reasoner -> Configure -> Displayed Inferences</b> and disable the following types of inference:
            |<ul>
            |  <li><b>Disjoint Classes</b> in <b>Class inferences</b>
            |  <li>All <b>Data Property Inferences</b>
            |  <li>Everything except <b>Types</b> in <b>Individual inferences</b>
            |</ul>
            |</p>
            |</div></html>""".stripMargin
      panel.add(new JLabel(displayLabel))
      panel.add(Box.createRigidArea(new Dimension(0, 10)))
      val ignoreMessageButton = new JCheckBox("Do not show this message again in this session")
      panel.add(ignoreMessageButton)
      JOptionPane.showMessageDialog(null, panel, s"Unsupported Inference Type (Sequoia ${BuildInfo.version})", JOptionPane.INFORMATION_MESSAGE)
      SequoiaReasonerPreferences.showUnsupportedOWLAPIMethodWarning = !ignoreMessageButton.isSelected
    }

}
