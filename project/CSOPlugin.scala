package io.cso.compiler

import io.cso.compiler.util.CSOLogger

import sbt.Keys._
import sbt._
import sbt.inc._
import java.nio.file.Files

// Import classes from the Zinc incremental compiler.
import xsbti.compile.CompileResult
import xsbti.compile.CompileAnalysis
import xsbti.compile.analysis.ReadStamps
import xsbti.compile.analysis.Stamp

// Import required internal SBT clases.
import sbt.internal.inc.{Analysis => InternalAnalysis}
import sbt.internal.inc.{Stamps => InternalStamps}
import sbt.internal.inc.Stamper


object CSOPlugin extends AutoPlugin {

  // Note: to show the classpath passed to the CPS transform tool, use "set logLevel in Global := Level.Debug".

  private [this] def doRecompile(logger: sbt.Logger,
                                 classpath: Classpath,
                                 classDirectory: java.io.File): Boolean = {

    val classpathSearchDirs: Array[File] = (classpath map { _.data }).toSet.toArray // This removes duplicates.

    object LocalLogger extends CSOLogger {
      override def error(msg: => String): Unit = logger.error(msg)
      override def info(msg: => String): Unit = logger.info(msg)
      override def verbose(msg: => String): Unit = logger.verbose(msg)
      override def debug(msg: => String): Unit = logger.debug(msg)
    }
    RewriterEntryPoint.recompile(LocalLogger)(classDirectory, Array(classDirectory), classpathSearchDirs, true)
    true
  }

  def recompileHook: Def.Initialize[Task[CompileResult]] = Def.task {
    val result: CompileResult = manipulateBytecode.value
    val classpath: Classpath = dependencyClasspath.value
    val theClassDirectory: java.io.File = classDirectory.value
    val logger: sbt.Logger = streams.value.log
    if (result.hasModified) {
      // Manipulate bytecode.
      val success = doRecompile(logger, classpath, theClassDirectory)
      if (!success) sys.error("Recompilation failed for compile.")
    }

    // Since we may have modified some of the products of the incremental compiler through bytecode transformation
    // we need to update their timestamps in the incremental compiler, otherwise the incremental compiler will see
    // that they have changed since it last compiled them, and recompile them.
    val analysis /*: CompileAnalysis*/ = result.analysis.asInstanceOf[InternalAnalysis]
    var newStamps/*: ReadStamps*/ = analysis.readStamps.asInstanceOf[InternalStamps]
    val productStamps: java.util.Map[File, Stamp] = newStamps.getAllProductStamps
    productStamps forEach {
      (product: java.io.File, stamp: Stamp) => {
        newStamps = newStamps.markProduct(product, Stamper.forLastModified(product))
      }
    }
    val updateAnalysis = analysis.copy(stamps = newStamps)

    result.withAnalysis(updateAnalysis)
  }

}
