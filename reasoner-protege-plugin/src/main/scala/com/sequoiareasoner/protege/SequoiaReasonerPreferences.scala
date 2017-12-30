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

package com.sequoiareasoner.protege

import com.sequoiareasoner.owlapi.SequoiaReasonerConfiguration
import org.protege.editor.core.prefs.Preferences
import org.protege.editor.core.prefs.PreferencesManager
import org.semanticweb.owlapi.reasoner.{FreshEntityPolicy, IndividualNodeSetPolicy, OWLReasonerConfiguration}

/** Loads and stores the Sequoia OWL Reasoner configuration to the Protégé registry.
  *
  * If the registry does not contain any saved reasoner configuration settings, then the default Sequoia OWL reasoner
  * configuration is used.
  *
  * Can obtain an OWL reasoner configuration with the settings stored here.
  */
object SequoiaReasonerPreferences {
  private[this] val idSequoiaPreferences                  = "com.sequoiareasoner"
  private[this] val keyAllowFreshEntities                 = "com.sequoiareasoner.AllowFreshEntities"
  private[this] val keyTimeout                            = "com.sequoiareasoner.Timeout"
  private[this] val keyIndividualNodeSetSameAs            = "com.sequoiareasoner.IndividualNodeSetSameAs"
  private[this] val keyEnableMultithreading               = "com.sequoiareasoner.EnableMultithreading"
  private[this] val keyEnableEqualityReasoning            = "com.sequoiareasoner.EnableEqualityReasoning"
  private[this] val keyShowUnsupportedOWLAPIMethodWarning = "com.sequoiareasoner.ShowUnsupportedOWLAPIMethodWarning"

  /** The default preferences are set to be identical to the default reasoner configuration. */
  private[this] val defaults = SequoiaReasonerConfiguration.getDefaultOWLReasonerConfiguration()
  val defaultAllowFreshEntities: Boolean = defaults.freshEntityPolicy == FreshEntityPolicy.ALLOW
  /** The default timeout to use for reasoning tasks. Set to 10 hours.
    * Note that this timeout for Protégé is different to that supplied by the default reasoner configuration.
    */
  val defaultTimeout: Long = 600000
  val defaultIndividualNodeSetSameAs: Boolean = defaults.individualNodeSetPolicy == IndividualNodeSetPolicy.BY_SAME_AS
  val defaultEnableMultithreading: Boolean = defaults.enableMultithreading
  val defaultEnableEqualityReasoning: Boolean = defaults.enableEqualityReasoning

  private[this] def getPreferences: Preferences = {
    val prefMan: PreferencesManager = PreferencesManager.getInstance
    prefMan.getPreferencesForSet(idSequoiaPreferences, SequoiaReasonerPreferences.getClass)
  }

  /** Policy for fresh entities: `true` if fresh entities are allowed, `false` otherwise. */
  protected[protege] var allowFreshEntities: Boolean = defaultAllowFreshEntities

  /** Timeout in milliseconds. */
  protected[protege] var timeout: Long = defaultTimeout

  /** Policy for individual nodes: `true` if individuals are grouped by name, `false` if grouped by name only. */
  protected[protege] var individualNodeSetSameAs: Boolean = defaultIndividualNodeSetSameAs

  /** `true` if multithreading should be enabled, otherwise `false`. */
  protected[protege] var enableMultithreading: Boolean = defaultEnableMultithreading

  /** `true` if equality reasoning should be disabled (which may lead to incompleteness), `false` otherwise. */
  protected[protege] var enableEqualityReasoning: Boolean = defaultEnableEqualityReasoning

  /** `true` if information dialog about unsupported OWL API method call should be shown. */
  protected[protege] var showUnsupportedOWLAPIMethodWarning: Boolean = true

  /** Returns an OWL reasoner configuration with the preferences set here. The preferences which are not specified here
    * (i.e., those that cannot be configured from within Protégé, including the progress monitor and unsupported method
    * handler) are set to their defaults as provided by
    * {{{SequoiaReasonerConfiguration.getDefaultOWLReasonerConfiguration()}}}.
    *
    * @return the reasoner configuration.
    */
  def reasonerConfiguration: SequoiaReasonerConfiguration =
    SequoiaReasonerConfiguration(
      defaults.progressMonitor,
      if (allowFreshEntities) FreshEntityPolicy.ALLOW else FreshEntityPolicy.DISALLOW,
      timeout,
      if (defaultIndividualNodeSetSameAs) IndividualNodeSetPolicy.BY_SAME_AS else IndividualNodeSetPolicy.BY_NAME,
      defaults.unsupportedFeatureObserver,
      defaults.unsupportedAPIMethodHandler,
      enableMultithreading,
      defaults.enableEqualitySimplifyReflect,
      defaults.enableTrieRedundancyIndex,
      enableEqualityReasoning,
    )

  /**
    * Load preferences for Sequoia from the Protégé registry.
    */
  def load: Unit = {
    val preferences: Preferences = getPreferences
    allowFreshEntities = preferences.getBoolean(keyAllowFreshEntities, defaultAllowFreshEntities)
    timeout = preferences.getLong(keyTimeout, defaultTimeout)
    individualNodeSetSameAs = preferences.getBoolean(keyIndividualNodeSetSameAs, defaultIndividualNodeSetSameAs)
    enableMultithreading = preferences.getBoolean(keyEnableMultithreading, defaultEnableMultithreading)
    enableEqualityReasoning = preferences.getBoolean(keyEnableEqualityReasoning, defaultEnableEqualityReasoning)
    showUnsupportedOWLAPIMethodWarning = preferences.getBoolean(keyShowUnsupportedOWLAPIMethodWarning, true)
  }

  /**
    * Save preferences for Sequoia to the Protégé registry.
    */
  def save: Unit = {
    val preferences: Preferences = getPreferences
    preferences.putBoolean(keyAllowFreshEntities, allowFreshEntities)
    preferences.putLong(keyTimeout, timeout)
    preferences.putBoolean(keyIndividualNodeSetSameAs, individualNodeSetSameAs)
    preferences.putBoolean(keyEnableMultithreading, enableMultithreading)
    preferences.putBoolean(keyEnableEqualityReasoning, enableEqualityReasoning)
    preferences.putBoolean(keyShowUnsupportedOWLAPIMethodWarning, showUnsupportedOWLAPIMethodWarning)
  }

}
