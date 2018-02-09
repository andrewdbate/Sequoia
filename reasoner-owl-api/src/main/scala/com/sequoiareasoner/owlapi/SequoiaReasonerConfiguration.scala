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

import com.sequoiareasoner.kernel.logging.Logger
import com.sequoiareasoner.kernel.reasoner.ReasonerConfiguration
import com.sequoiareasoner.kernel.structural.{UnsupportedFeatureObserver, UnsupportedFeatureObserverIgnore, UnsupportedFeatureObserverThrowException}
import org.semanticweb.owlapi.reasoner._

object SequoiaReasonerConfiguration {
  def getDefaultOWLReasonerConfiguration(): SequoiaReasonerConfiguration =
    new SequoiaReasonerConfiguration()

  def getDefaultOWLReasonerConfiguration(progressMonitor: ReasonerProgressMonitor): SequoiaReasonerConfiguration =
    new SequoiaReasonerConfiguration(progressMonitor = new SequoiaReasonerProgressMonitor(progressMonitor))

  def getSequoiaReasonerConfiguration(config: OWLReasonerConfiguration): SequoiaReasonerConfiguration = config match {
    case config: SequoiaReasonerConfiguration => config
    case _ => new SequoiaReasonerConfiguration(new SequoiaReasonerProgressMonitor(config.getProgressMonitor), config.getFreshEntityPolicy, config.getTimeOut, config.getIndividualNodeSetPolicy)
  }

  private def mkString(b: Boolean): String = if (b) "ON" else "OFF"
}

/**
  * @author Andrew Bate <code@andrewbate.com>
  *
  * @param progressMonitor                the progress monitor to use.
  * @param freshEntityPolicy              the policy for fresh entities.
  * @param timeOut                        the timeout in milliseconds.
  * @param individualNodeSetPolicy        the policy for individual nodes.
  * @param unsupportedAPIMethodHandler    the handler to call when unsupported API methods are called on the reasoner
  * @param enableMultithreading           `true` if multithreading should be enabled, otherwise `false`.
  * @param enableEqualitySimplifyReflect  `true` if the simplify-reflect optimisation for equality should be enabled, `false` otherwise.
  * @param enableTrieRedundancyIndex      `true` if the trie index for redundancy elimination should be used, `false` otherwise for naive redundancy indexing.
  * @param enableEqualityReasoning        `true` if equality reasoning should be enabled (required for completeness guarantees), `false` otherwise.
  */
class SequoiaReasonerConfiguration(progressMonitor: SequoiaReasonerProgressMonitor = new SequoiaReasonerProgressMonitor,
                                   freshEntityPolicy: FreshEntityPolicy = FreshEntityPolicy.ALLOW,
                                   timeOut: Long = Long.MaxValue,
                                   individualNodeSetPolicy: IndividualNodeSetPolicy = IndividualNodeSetPolicy.BY_NAME,
                                   unsupportedFeatureTreatment: UnsupportedFeatureTreatment = UnsupportedFeatureTreatment.THROW_EXCEPTION,
                                   unsupportedAPIMethodHandler: UnsupportedAPIMethodHandler = new DefaultUnsupportedAPIMethodHandler,
                                   enableMultithreading: Boolean = true,
                                   enableEqualitySimplifyReflect: Boolean = true,
                                   enableTrieRedundancyIndex: Boolean = true,
                                   enableEqualityReasoning: Boolean = true) extends OWLReasonerConfiguration with Serializable {
  import SequoiaReasonerConfiguration._

  override def getProgressMonitor: ReasonerProgressMonitor = progressMonitor.getReasonerProgressMonitor

  def getSequoiaProgressMonitor: SequoiaReasonerProgressMonitor = progressMonitor

  override def getFreshEntityPolicy: FreshEntityPolicy = freshEntityPolicy

  override def getTimeOut: Long = timeOut

  override def getIndividualNodeSetPolicy: IndividualNodeSetPolicy = individualNodeSetPolicy

  def getUnsupportedFeatureTreatment: UnsupportedFeatureTreatment = unsupportedFeatureTreatment

  def getUnsupportedAPIMethodHandler: UnsupportedAPIMethodHandler = unsupportedAPIMethodHandler

  /** Returns `true` if the reasoner should attempt to use multiple threads during the saturation phase.
    *
    * @return
    */
  def isMultithreadingEnabled: Boolean = enableMultithreading

  /** Returns `true` if the simplify-reflect optimisation for equality is enabled.
    *
    * @return `true` if the simplify-reflect optimisation for equality is enabled.
    */
  def isEqualitySimplifyReflectEnabled: Boolean = enableEqualitySimplifyReflect

  /** Returns `true` if the trie-based redundancy index should be used. If `false`, then the fallback redundancy index
    * will be maintained and used instead.
    *
    * @return
    */
  def isTrieRedundancyIndexEnabled: Boolean = enableTrieRedundancyIndex

  /** Returns `true` if equality reasoning is disabled, indicating that the reasoner may return incomplete results.
    *
    * @return `true` if equality reasoning is disabled.
    */
  def isEqualityReasoningEnabled: Boolean = enableEqualityReasoning

  /** Returns a [[ReasonerConfiguration]] object that provides the configuration for the internal reasoner.
    *
    * @return the configuration for the internal reasoner.
    */
  protected[owlapi] def getConfiguration =
    ReasonerConfiguration(progressMonitor, enableMultithreading, enableEqualitySimplifyReflect, enableTrieRedundancyIndex, enableEqualityReasoning, freshEntityPolicy == FreshEntityPolicy.ALLOW)

  protected[owlapi] def getUnsupportedFeatureObserver(logger: Logger): UnsupportedFeatureObserver = unsupportedFeatureTreatment match {
    case UnsupportedFeatureTreatment.THROW_EXCEPTION =>
      new UnsupportedFeatureObserverThrowException(logger)
    case UnsupportedFeatureTreatment.IGNORE =>
      new UnsupportedFeatureObserverIgnore(logger)
  }

  protected[owlapi] def getUnsupportedSWRLRuleHandler(logger: Logger): UnsupportedSWRLRuleHandler = unsupportedFeatureTreatment match {
    case UnsupportedFeatureTreatment.THROW_EXCEPTION =>
      new UnsupportedSWRLRuleHandlerThrowException(logger)
    case UnsupportedFeatureTreatment.IGNORE =>
      new UnsupportedSWRLRuleHandlerIgnore(logger)
  }

  override def toString: String =
    s"""|SequoiaReasonerConfiguration[
        |  progressMonitor: $progressMonitor;
        |  freshEntityPolicy: $freshEntityPolicy;
        |  timeOut: $timeOut;
        |  individualNodeSetPolicy: $individualNodeSetPolicy;
        |  unsupportedFeatureTreatment: $unsupportedFeatureTreatment;
        |  unsupportedAPIMethodHandler: ${unsupportedAPIMethodHandler.getClass.getSimpleName};
        |  Multithreading: ${mkString(enableMultithreading)};
        |  EqualitySimplifyReflect: ${mkString(enableEqualitySimplifyReflect)};
        |  TrieRedundancyIndex: ${mkString(enableTrieRedundancyIndex)};
        |  EqualityReasoning: ${mkString(enableEqualityReasoning)};
        |]""".stripMargin

}
