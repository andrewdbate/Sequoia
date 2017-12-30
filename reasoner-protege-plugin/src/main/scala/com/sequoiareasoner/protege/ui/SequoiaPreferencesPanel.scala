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

import java.awt.{BorderLayout, Color}
import javax.swing._
import javax.swing.event.{DocumentEvent, DocumentListener}

import com.sequoiareasoner.protege.SequoiaReasonerPreferences

import org.protege.editor.core.ui.preferences.PreferencesLayoutPanel
import org.protege.editor.owl.ui.preferences.OWLPreferencesPanel
import com.sequoiareasoner.protege.util.TimeUtils
import net.java.balloontip.BalloonTip
import net.java.balloontip.styles.EdgedBalloonStyle

/** Implements preferences panel for configuring Sequoia within Protégé.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class SequoiaPreferencesPanel extends OWLPreferencesPanel {
  self =>

  private[this] val errorColor = new Color(200, 0, 0)
  private[this] val borderColor = new Color(220, 220, 220)
  private[this] val insideEmptyBorder = BorderFactory.createEmptyBorder(2, 2, 2, 2)
  private[this] val normalBorder = BorderFactory.createCompoundBorder(
    BorderFactory.createMatteBorder(1, 1, 1, 1, borderColor), insideEmptyBorder
  )
  private[this] val errorBorder = BorderFactory.createCompoundBorder(
    BorderFactory.createMatteBorder(1, 1, 1, 1, errorColor), insideEmptyBorder
  )

  private[this] var timeoutMillis: Long = SequoiaReasonerPreferences.timeout
  private[this] var allowFreshEntitiesButton: JRadioButton = _
  private[this] var bySameAsButton: JRadioButton = _
  private[this] var checkBoxMultithreading: JCheckBox = _
  private[this] var checkBoxEqualityReasoning: JCheckBox = _
  private[this] var checkBoxSuppressOWLAPIMethodWarnings: JCheckBox = _

  override def initialise: Unit = {
    setLayout(new BorderLayout)
    val panel = new PreferencesLayoutPanel
    add(panel, BorderLayout.NORTH)

    val textFieldTimeout: JTextField = new JTextField(20)
    textFieldTimeout.setText(TimeUtils.millisToTimeOutString(timeoutMillis))
    textFieldTimeout.setToolTipText("The maximum amount of time before reasoning tasks are interrupted and cancelled.")
    textFieldTimeout.setBorder(normalBorder)

    val balloonTip = new BalloonTip(textFieldTimeout, new JLabel(),
      new EdgedBalloonStyle(Color.WHITE, errorColor),
      BalloonTip.Orientation.LEFT_ABOVE,
      BalloonTip.AttachLocation.ALIGNED,
      20, 10,
      false)
    balloonTip.setPadding(3)
    balloonTip.setVisible(false)

    val documentListener = new DocumentListener {
      def updateTimeout: Unit = TimeUtils.parseTimeOutStringToMillis(textFieldTimeout.getText) match {
        case Left(parseErrorMessage) =>
          balloonTip.setTextContents(parseErrorMessage)
          balloonTip.setVisible(true)
          textFieldTimeout.setBorder(errorBorder)
        case Right(timeOutMillis) =>
          self.timeoutMillis = timeOutMillis
          balloonTip.setVisible(false)
          textFieldTimeout.setBorder(normalBorder)
      }
      override def removeUpdate(e: DocumentEvent): Unit = updateTimeout
      override def changedUpdate(e: DocumentEvent): Unit = updateTimeout
      override def insertUpdate(e: DocumentEvent): Unit = updateTimeout
    }

    // ------ TIME OUT PREFERENCE --------------------------------------------
    panel.addGroup("Timeout")
    panel.addGroupComponent(textFieldTimeout)
    textFieldTimeout.getDocument.addDocumentListener(documentListener)

    panel.addSeparator

    // ------ ALLOW FRESH ENTITIES PREFERENCE --------------------------------
    panel.addGroup("Fresh Entities")
    allowFreshEntitiesButton = new JRadioButton("Allow", SequoiaReasonerPreferences.allowFreshEntities)
    allowFreshEntitiesButton.setToolTipText("If set, fresh entities are allowed in the signature of queries.")
    val disallowFreshEntitiesButton = new JRadioButton("Disallow", !SequoiaReasonerPreferences.allowFreshEntities)
    disallowFreshEntitiesButton.setToolTipText("If set, fresh entities are not allowed in the signature of queries.")
    val group1 = new ButtonGroup
    group1.add(allowFreshEntitiesButton)
    group1.add(disallowFreshEntitiesButton)
    panel.addGroupComponent(allowFreshEntitiesButton)
    panel.addGroupComponent(disallowFreshEntitiesButton)

    panel.addSeparator

    /*
    // ------ INDIVIDUAL NODE SET PREFERENCE ---------------------------------
    panel.addGroup("Individual Node Sets")
    bySameAsButton = new JRadioButton("By Same As", SequoiaReasonerPreferences.individualNodeSetSameAs)
    bySameAsButton.setToolTipText("If set, named individuals that are proven by the reasoner to be equal to each other will be grouped together.")
    val byNameButton = new JRadioButton("By Name", !SequoiaReasonerPreferences.individualNodeSetSameAs)
    byNameButton.setToolTipText("If set, named individuals will never be grouped together.")
    val group2 = new ButtonGroup
    group2.add(bySameAsButton)
    group2.add(byNameButton)
    panel.addGroupComponent(bySameAsButton)
    panel.addGroupComponent(byNameButton)

    panel.addSeparator
    */

    // ------ ENABLE MULTITHREADING PREFERENCE -------------------------------
    panel.addGroup("Reasoner Settings")
    checkBoxMultithreading = new JCheckBox("Enable Multithreading", SequoiaReasonerPreferences.enableMultithreading)
    checkBoxMultithreading.setToolTipText("If checked, Sequoia will use multiple threads to perform reasoning.")
    panel.addGroupComponent(checkBoxMultithreading)


    // ------ ENABLE EQUALITY REASONING PREFERENCE ---------------------------
    checkBoxEqualityReasoning = new JCheckBox("Enable Equality Reasoning", SequoiaReasonerPreferences.enableEqualityReasoning)
    checkBoxEqualityReasoning.setToolTipText("If checked, Sequoia will perform equality reasoning (the default).")
    panel.addGroupComponent(checkBoxEqualityReasoning)

    panel.addSeparator

    // ------ SUPPRESS ALL UNSUPPORTED METHOD WARNINGS -----------------------
    panel.addGroup("Suppressed Warnings")
    checkBoxSuppressOWLAPIMethodWarnings = new JCheckBox("Show Unsupported Inference Warnings", SequoiaReasonerPreferences.showUnsupportedOWLAPIMethodWarning)
    checkBoxSuppressOWLAPIMethodWarnings.setToolTipText("If checked, warnings about unsupported types of inference will be shown.")
    panel.addGroupComponent(checkBoxSuppressOWLAPIMethodWarnings)

  }

  override def applyChanges: Unit = {
    SequoiaReasonerPreferences.timeout = timeoutMillis
    SequoiaReasonerPreferences.allowFreshEntities = allowFreshEntitiesButton.isSelected
    //SequoiaReasonerPreferences.individualNodeSetSameAs = bySameAsButton.isSelected
    SequoiaReasonerPreferences.enableMultithreading = checkBoxMultithreading.isSelected
    SequoiaReasonerPreferences.enableEqualityReasoning = checkBoxEqualityReasoning.isSelected
    SequoiaReasonerPreferences.showUnsupportedOWLAPIMethodWarning = checkBoxSuppressOWLAPIMethodWarnings.isSelected
    SequoiaReasonerPreferences.save
  }

  override def dispose: Unit = {
    allowFreshEntitiesButton = null
    bySameAsButton = null
    checkBoxMultithreading = null
    checkBoxEqualityReasoning = null
    checkBoxSuppressOWLAPIMethodWarnings = null
  }

}
