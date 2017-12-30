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

import javax.swing._
import java.awt.{Color, Insets, BorderLayout}
import java.awt.event.ActionEvent

/** Panel displaying a sequence of information messages with line numbers.
  *
  * @author Andrew Bate <code@andrewbate.com>
  */
class EventTracePanel extends JPanel(new BorderLayout) {
  // Optimization: implement and set the model directly for efficiency.
  private[this] object EventListModel extends AbstractListModel[String] {
    private[this] var events = new Array[String](10000)
    private[this] var numElemsDisplayed = 0
    private[this] var numElemsStored = 0
    override def getElementAt(n: Int): String = events(n)
    override def getSize: Int = numElemsDisplayed
    /** Ensures that the all elements are set to be displayed.
      */
    def showAllSetElements: Unit = {
      // Elements corresponding to line numbers 1--numElemsStored will be shown in the displayed list on future renders.
      val oldNumElemsDisplayed = numElemsDisplayed
      numElemsDisplayed = numElemsStored
      fireIntervalAdded(this, oldNumElemsDisplayed, numElemsDisplayed - 1)
    }
    /** Set the value of an index that may or may not be shown on screen. Use `showAllSetElements` to ensure that the
      * element at index `n` is shown when the model is rendered. Index `n` can be greater than `getSize`.
      *
      * @param n  the index at which to add the element
      * @param v  the element to add
      */
    def setElementAt(n: Int, v: String): Unit = {
      val length = events.length
      // Ensure capacity.
      if (length < n + 1) {
        val newEvents = new Array[String](length << 1)
        Array.copy(events, 0, newEvents, 0, length)
        events = newEvents
      }
      events(n) = v
      numElemsStored = math.max(numElemsStored, n + 1)
    }
    /** Returns the String representation of this list model.
      *
      * In the returned String, each element is numbered, elements not shown when rendered are markted '*', and the
      * sequence is ordered as the events are displayed.
      *
      * @return the String representation of this list model.
      */
    override def toString: String = {
      val builder = new StringBuilder
      builder.append("Elements[")
      for (i <- 0 until numElemsStored) {
        builder.append(" " * (i/numElemsStored))
        builder.append(i)
        if (i >= numElemsDisplayed) builder.append('*')
        builder.append('.')
        builder.append(events(i))
        builder.append(',')
      }
      builder.append(']')
      builder.result
    }
  }
  // Necessary optimization: for efficiency implement and set the model directly.
  private[this] object LineNumberModel extends AbstractListModel[Int] {
    override def getElementAt(n: Int): Int = n + 1
    override def getSize: Int = EventListModel.getSize
  }

  /** JList that displays the events against their line number. */
  private val eventListView = new JList[String] {
    setModel(EventListModel)
    // Copy and cut support.
    getActionMap.put(TransferHandler.getCutAction.getValue(Action.NAME), TransferHandler.getCutAction)
    getActionMap.put(TransferHandler.getCopyAction.getValue(Action.NAME), TransferHandler.getCopyAction)
    getInputMap.put(KeyStroke.getKeyStroke("ctrl X"), TransferHandler.getCutAction.getValue(Action.NAME)) // TODO: test bindings on Mac.
    getInputMap.put(KeyStroke.getKeyStroke("ctrl C"), TransferHandler.getCopyAction.getValue(Action.NAME))
    // Cell renderer implementing the alternating colours of lines.
    setCellRenderer(new ListCellRenderer[String] {
      // For odd rows, set the background to be slightly darker; colour values must be at least zero.
      private[this] val r = math.max(getBackground.getRed - 7, 0)
      private[this] val g = math.max(getBackground.getGreen - 7, 0)
      private[this] val b = math.max(getBackground.getBlue - 7, 0)
      private[this] val rowBgColor = getBackground
      private[this] val rowAlternateBgColor = new Color(r,g,b)
      private[this] val rowFgColor = getForeground
      private[this] val rowSelectedBgColor = getSelectionBackground
      private[this] val rowSelectedFgColor = getSelectionForeground
      private[this] val lineString = new JLabel {
        setOpaque(true)
        setAlignmentX(SwingConstants.LEADING)
      }
      // The internal margin used by the labels representing events in the list view.
      private[this] val marginBorder = BorderFactory.createEmptyBorder(0, 5, 0, 5)
      private[this] val borderWidth = 1
      private[this] val focusBorder = BorderFactory.createCompoundBorder(BorderFactory.createLineBorder(Color.GRAY, borderWidth), marginBorder)
      private[this] val noFocusBorder = BorderFactory.createCompoundBorder(BorderFactory.createEmptyBorder(borderWidth,borderWidth,borderWidth,borderWidth), marginBorder)
      private[this] val rowFont = getFont
      override def getListCellRendererComponent(list: JList[_ <: String], a: String, index: Int, isSelected: Boolean, focused: Boolean): JLabel = {
        // Elements in the list view may be null; in this case we must display something for that line to correspond to the line number.
        lineString.setText(if (a ne null) a else " ")
        if (isSelected) {
          lineString.setBackground(rowSelectedBgColor)
          lineString.setForeground(rowSelectedFgColor)
        } else {
          lineString.setBackground(if (index % 2 == 0) rowBgColor else rowAlternateBgColor)
          lineString.setForeground(rowFgColor)
        }
        lineString.setBorder(if (focused) focusBorder else noFocusBorder)
        lineString.setEnabled(isEnabled)
        lineString.setFont(rowFont)
        // If the prototype cell value is too short, update it to a longer prototype, otherwise view will be truncated.
        if (lineString.getText.length > getPrototypeCellValue.length) setPrototypeCellValue(lineString.getText)
        lineString
      }
    })
    // We MUST set the prototype cell value, otherwise performance will be unacceptable.
    setPrototypeCellValue("123456789abcdefghijklmnopqrstuvwxyz")
  }

  /** JList whose sole purpose is to provide the line numbers on the left-hand side of the screen. */
  private val lineNumberListView = new JList[Int] {
    // PRECONDITION: Must be declared after eventListView has been constructed since this object assumes that variable is non-null.
    assert(eventListView != null, "PRECONDITION VIOLATED: eventListView is null, breaking the contract of this object's constructor")
    /* Define the border on the right-hand side between the line numbers and the
     * list of events, and also the internal margin for the line numbers.
     */
    setBorder(BorderFactory.createCompoundBorder(BorderFactory.createMatteBorder(0, 0, 0, 1, Color.lightGray), BorderFactory.createEmptyBorder(0, 10, 0, 8)))
    // Font colour of line numbers.
    private[this] val lineNumberColor = new Color(100,100,100)
    private[this] val lineNumberFont = eventListView.getFont
    // Set the background to be slightly darker than that of the events; values must be at least zero.
    private[this] val r = math.max(eventListView.getBackground.getRed - 15, 0)
    private[this] val g = math.max(eventListView.getBackground.getGreen - 15, 0)
    private[this] val b = math.max(eventListView.getBackground.getBlue - 15, 0)
    setBackground(new Color(r,g,b))
    setModel(LineNumberModel)

    // Cell renderer implementing the alternating colours of lines.
    setCellRenderer(new ListCellRenderer[Int] {
      // The internal margin used by the labels representing events in the list view.
      private[this] val borderWidth = 1
      private[this] val lineString = new JLabel {
        setAlignmentX(SwingConstants.TRAILING)
        setForeground(lineNumberColor)
        setBorder(BorderFactory.createEmptyBorder(borderWidth,0,borderWidth,0))
        setFont(lineNumberFont)
      }
      override def getListCellRendererComponent(list: JList[_ <: Int], a: Int, index: Int, isSelected: Boolean, focused: Boolean): JLabel = {
        // Elements in the list view may be null; in this case we must display something for that line to correspond to the line number.
        lineString.setText(a.toString)
        // If the prototype cell value is too short, update it to a longer prototype, otherwise line number will be truncated.
        if (a > getPrototypeCellValue) setPrototypeCellValue(10 * getPrototypeCellValue + 9)
        lineString
      }
    })
    // We MUST set the prototype cell value, otherwise performance will be unacceptable.
    setPrototypeCellValue(99)
  }

  private[this] val toolbar = new JToolBar {
    private[this] val pauseButton = new JToggleButton {
      setAction(new AbstractAction("Pause Updates") {
        override def actionPerformed(a: ActionEvent): Unit = displayManager.paused = !displayManager.paused
      })
      setSelected(displayManager.paused)
      //setIcon(IconHelper.getImageIcon(IconHelper.IconLib.PAUSE))
      setFocusPainted(false)
      setMargin(new Insets(4, 5, 5, 5))
    }
    private[this] val autoScrollButton = new JToggleButton {
      setAction(new AbstractAction("Scroll with Log") {
        override def actionPerformed(a: ActionEvent): Unit = displayManager.autoScroll = !displayManager.autoScroll
      })
      setSelected(displayManager.autoScroll)
      //setIcon(IconHelper.getImageIcon(IconHelper.IconLib.BOTTOM))
      setFocusPainted(false)
      setMargin(new Insets(4, 5, 5, 5))
    }
    setFloatable(false)
    add(pauseButton)
    add(autoScrollButton)
  }

  private val scrollPane = new JScrollPane {
    setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.lightGray))
    setViewportView(new JPanel(new BorderLayout) {
      add(lineNumberListView, BorderLayout.WEST)
      add(eventListView, BorderLayout.CENTER)
    })
  }

  // Add components to the panel.
  add(toolbar, BorderLayout.NORTH)
  add(scrollPane, BorderLayout.CENTER)

  private val scrollbarModel = scrollPane.getVerticalScrollBar.getModel
  // Setting setValueIsAdjusting(true) makes the scrolling smoother.
  scrollbarModel.setValueIsAdjusting(true)

  private object displayManager extends java.util.TimerTask {
    /** localPaused == true iff the display is not to be updated when new events are logged.
      * Allow dirty reads of paused for efficiency.
      */
    private var localPaused = false
    // Must additionally schedule a paint after un-setting the pause.
    def paused_=(b: Boolean) {
      synchronized { localPaused = b }
      if (!b) run
    }
    def paused = localPaused
    /** localAutoScroll == true iff the JList object should automatically scroll down
      * to the last logged event after each event is logged, even if the scroll bar
      * is not at its maximum position.
      *
      * This field has no effect on the scrolling behaviour when the scroll bar is in its maximum position.
      * In this case it will automatically scroll down as events are logged regardless.
      *
      * Allow dirty reads of localAutoScroll for efficiency
      */
    private[this] var localAutoScroll = true
    def autoScroll_=(b: Boolean) = synchronized { localAutoScroll = b }
    def autoScroll = localAutoScroll
    /** localDirty == true iff the underlying model has changed since the last repaint
      * was invoked (but not necessarily completed).
      *
      * Allow dirty reads of localDirty for efficiency.
      */
    private[this] var localDirty = false
    /** Must only be called from the event dispatch thread. */
    def dirty = localDirty
    /** Must only be called from the event dispatch thread. */
    def dirty_=(b: Boolean) = localDirty = b

    /* This runnable must ONLY be invoked from the event dispatching thread. */
    private val runner = new Runnable {
      override def run {
        dirty = false
        /* Determine whether the user has scrolled down to the bottom of the
         * scroll pane. If so then when a new event is added we automatically scroll down
         * as events are added in real time. If the user has not scrolled to the bottom then
         * we do not force the window to scroll down.
         * NOTE: This must be calculated *before* the new event is added.
         */
        val scrollToLast = autoScroll || (scrollbarModel.getMaximum - scrollbarModel.getExtent == scrollbarModel.getValue)
        EventListModel.showAllSetElements
        // Scroll down on the list of line numbers, otherwise the line numbers may not be shown in view.
        if (scrollToLast) lineNumberListView.ensureIndexIsVisible(LineNumberModel.getSize - 1)
      }
    }

    override def run = if (dirty && !paused) SwingUtilities.invokeLater(runner)

  }

  // Timer thread must be a daemon thread.
  private val timer = new java.util.Timer("Display Manager Thread", true)
  // Run the task every half second after no initial delay.
  timer.scheduleAtFixedRate(displayManager, 0, 500)

  private[this] var nextIndex = 0

  /** Adds a new element to the collection of elements to be displayed.
    *
    * @param message  the element to be added to the list.
    */
  def addEventToDisplay(message: String): Unit =
    SwingUtilities.invokeLater { () =>
      // Update the model model from the event dispatch thread.
      EventListModel.setElementAt(nextIndex, message)
      nextIndex += 1
      displayManager.dirty = true
    }

}
