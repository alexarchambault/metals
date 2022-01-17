package scala.meta.internal.metals

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference
import java.{util => ju}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.control.NonFatal

import scala.meta.internal.bsp.BspSession
import scala.meta.internal.bsp.BuildChange
import scala.meta.internal.builds.BuildTool
import scala.meta.internal.builds.BuildTools
import scala.meta.internal.builds.Digest.Status
import scala.meta.internal.builds.WorkspaceReload
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ammonite.Ammonite
import scala.meta.internal.metals.clients.language.DelegatingLanguageClient
import scala.meta.internal.metals.clients.language.ForwardingMetalsBuildClient
import scala.meta.internal.metals.debug.BuildTargetClasses
import scala.meta.internal.metals.testProvider.TestSuitesProvider
import scala.meta.internal.metals.watcher.FileWatcher
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.tvp.TreeViewProvider
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.io.AbsolutePath

import ch.epfl.scala.{bsp4j => b}

final case class Indexer(
    workspaceReload: () => WorkspaceReload,
    doctor: () => Doctor,
    languageClient: DelegatingLanguageClient,
    bspSession: () => Option[BspSession],
    executionContext: ExecutionContextExecutorService,
    tables: () => Tables,
    statusBar: () => StatusBar,
    timerProvider: TimerProvider,
    scalafixProvider: () => ScalafixProvider,
    indexingPromise: Promise[Unit],
    ammonite: () => Ammonite,
    lastImportedBuilds: () => List[ImportedBuild],
    clientConfig: ClientConfiguration,
    definitionIndex: OnDemandSymbolIndex,
    referencesProvider: () => ReferenceProvider,
    workspaceSymbols: () => WorkspaceSymbolProvider,
    buildTargets: BuildTargets,
    interactiveSemanticdbs: () => InteractiveSemanticdbs,
    buildClient: () => ForwardingMetalsBuildClient,
    semanticDBIndexer: () => SemanticdbIndexer,
    treeView: () => TreeViewProvider,
    worksheetProvider: () => WorksheetProvider,
    symbolSearch: () => MetalsSymbolSearch,
    buildTools: () => BuildTools,
    formattingProvider: () => FormattingProvider,
    fileWatcher: FileWatcher,
    focusedDocument: () => Option[AbsolutePath],
    focusedDocumentBuildTarget: AtomicReference[b.BuildTargetIdentifier],
    buildTargetClasses: BuildTargetClasses,
    userConfig: () => UserConfiguration,
    sh: ScheduledExecutorService,
    symbolDocs: Docstrings,
    scalaVersionSelector: ScalaVersionSelector,
    testProvider: () => TestSuitesProvider,
    buildTargetsData: BuildTargets.WritableData,
    sourceMapper: SourceMapper
) {

  private implicit def ec: ExecutionContextExecutorService = executionContext

  def reloadWorkspaceAndIndex(
      forceRefresh: Boolean,
      buildTool: BuildTool,
      checksum: String
  ): Future[BuildChange] = {
    def reloadAndIndex(session: BspSession): Future[BuildChange] = {
      workspaceReload().persistChecksumStatus(Status.Started, buildTool)

      session
        .workspaceReload()
        .map { _ =>
          scribe.info("Correctly reloaded workspace")
          profiledIndexWorkspace(() => doctor().check())
          workspaceReload().persistChecksumStatus(Status.Installed, buildTool)
          BuildChange.Reloaded
        }
        .recoverWith { case NonFatal(e) =>
          scribe.error(s"Unable to reload workspace: ${e.getMessage()}")
          workspaceReload().persistChecksumStatus(Status.Failed, buildTool)
          languageClient.showMessage(Messages.ReloadProjectFailed)
          Future.successful(BuildChange.Failed)
        }
    }

    bspSession() match {
      case None =>
        scribe.warn("No build session currently active to reload.")
        Future.successful(BuildChange.None)
      case Some(session) if forceRefresh => reloadAndIndex(session)
      case Some(session) =>
        workspaceReload().oldReloadResult(checksum) match {
          case Some(status) =>
            scribe.info(s"Skipping reload with status '${status.name}'")
            Future.successful(BuildChange.None)
          case None =>
            synchronized {
              for {
                userResponse <- workspaceReload().requestReload(
                  buildTool,
                  checksum
                )
                installResult <- {
                  if (userResponse.isYes) {
                    reloadAndIndex(session)
                  } else {
                    tables().dismissedNotifications.ImportChanges
                      .dismiss(2, TimeUnit.MINUTES)
                    Future.successful(BuildChange.None)
                  }
                }
              } yield installResult
            }
        }
    }
  }

  def profiledIndexWorkspace(check: () => Unit): Future[Unit] = {
    val tracked = statusBar().trackFuture(
      s"Indexing",
      Future {
        timerProvider.timedThunk("indexed workspace", onlyIf = true) {
          try indexWorkspace(check)
          finally {
            Future(scalafixProvider().load())
            indexingPromise.trySuccess(())
          }
        }
      }
    )
    tracked.foreach { _ =>
      statusBar().addMessage(
        s"${clientConfig.icons.rocket}Indexing complete!"
      )
      if (clientConfig.initialConfig.statistics.isMemory) {
        logMemory(
          "definition index",
          definitionIndex
        )
        logMemory(
          "references index",
          referencesProvider().index
        )
        logMemory(
          "workspace symbol index",
          workspaceSymbols().inWorkspace
        )
        logMemory(
          "classpath symbol index",
          workspaceSymbols().inDependencies.packages
        )
        logMemory(
          "build targets",
          buildTargets
        )
      }
    }
    tracked
  }

  private def logMemory(name: String, index: Object): Unit = {
    val footprint = Memory.footprint(name, index)
    scribe.info(s"memory: $footprint")
  }

  private def indexWorkspace(check: () => Unit): Unit = {
    val lastImportedBuilds0 = lastImportedBuilds()
    timerProvider.timedThunk(
      "reset stuff",
      clientConfig.initialConfig.statistics.isIndex
    ) {
      buildTargetsData.reset()
      interactiveSemanticdbs().reset()
      buildClient().reset()
      semanticDBIndexer().reset()
      treeView().reset()
      worksheetProvider().reset()
      symbolSearch().reset()
    }
    val allBuildTargetsData = Seq(
      (
        "main",
        buildTargetsData,
        if (lastImportedBuilds0.isEmpty) ImportedBuild.empty
        else lastImportedBuilds0.reduce(_ ++ _)
      )
    )
    for ((name, data, importedBuild) <- allBuildTargetsData)
      timerProvider.timedThunk(
        s"updated $name build targets",
        clientConfig.initialConfig.statistics.isIndex
      ) {
        data.reset()
        data.addWorkspaceBuildTargets(importedBuild.workspaceBuildTargets)
        data.addScalacOptions(importedBuild.scalacOptions)
        data.addJavacOptions(importedBuild.javacOptions)
        for {
          item <- importedBuild.sources.getItems.asScala
          source <- item.getSources.asScala
        } {
          val sourceItemPath =
            source.getUri.toAbsolutePath(followSymlink = false)
          data.addSourceItem(sourceItemPath, item.getTarget)
        }
      }
    timerProvider.timedThunk(
      "post update build targets stuff",
      clientConfig.initialConfig.statistics.isIndex
    ) {
      check()
      buildTools()
        .loadSupported()
      formattingProvider().validateWorkspace()
    }
    timerProvider.timedThunk(
      "started file watcher",
      clientConfig.initialConfig.statistics.isIndex
    ) {
      try {
        fileWatcher.restart()
      } catch {
        // note(@tgodzik) This is needed in case of ammonite
        // where it can rarely deletes directories while we are trying to watch them
        case NonFatal(e) =>
          scribe.warn("File watching failed, indexes will not be updated.", e)
      }
    }
    timerProvider.timedThunk(
      "indexed library classpath",
      clientConfig.initialConfig.statistics.isIndex
    ) {
      workspaceSymbols().indexClasspath()
    }
    timerProvider.timedThunk(
      "indexed workspace SemanticDBs",
      clientConfig.initialConfig.statistics.isIndex
    ) {
      semanticDBIndexer().onTargetRoots()
    }
    for ((name, data, _) <- allBuildTargetsData)
      timerProvider.timedThunk(
        s"indexed workspace $name sources",
        clientConfig.initialConfig.statistics.isIndex
      ) {
        indexWorkspaceSources(data)
      }
    var usedJars = Set.empty[AbsolutePath]
    for ((name, data, importedBuild) <- allBuildTargetsData)
      timerProvider.timedThunk(
        "indexed library sources",
        clientConfig.initialConfig.statistics.isIndex
      ) {
        usedJars ++= indexJdkSources(data, importedBuild.dependencySources)
        usedJars ++= indexDependencySources(
          data,
          importedBuild.dependencySources
        )
      }
    // Schedule removal of unused toplevel symbols from cache
    if (usedJars.nonEmpty)
      sh.schedule(
        new Runnable {
          override def run(): Unit = {
            tables().jarSymbols.deleteNotUsedTopLevels(usedJars.toArray)
          }
        },
        2,
        TimeUnit.SECONDS
      )

    focusedDocument().foreach { doc =>
      buildTargets
        .inverseSources(doc)
        .foreach(focusedDocumentBuildTarget.set)
    }

    val targets = buildTargets.allBuildTargetIds
    buildTargetClasses
      .rebuildIndex(targets)
      .foreach { _ =>
        testProvider().refreshTestSuites()
        languageClient.refreshModel()
      }
  }

  def indexWorkspaceSources(data: Seq[BuildTargets.WritableData]): Unit = {
    for (data0 <- data.iterator)
      indexWorkspaceSources(data)
  }
  def indexWorkspaceSources(data: BuildTargets.WritableData): Unit = {
    case class SourceToIndex(
        source: AbsolutePath,
        sourceItem: AbsolutePath,
        targets: Iterable[b.BuildTargetIdentifier]
    )
    val sourcesToIndex = mutable.ListBuffer.empty[SourceToIndex]
    for {
      (sourceItem, targets) <- data.sourceItemsToBuildTarget
      source <- sourceItem.listRecursive
      if source.isScalaOrJava
    } {
      targets.asScala.foreach { target =>
        data.linkSourceFile(target, source)
      }
      sourcesToIndex += SourceToIndex(source, sourceItem, targets.asScala)
    }
    val threadPool = new ForkJoinPool(
      Runtime.getRuntime().availableProcessors() match {
        case 1 => 1
        case f => f / 2
      }
    )
    try {
      val parSourcesToIndex = sourcesToIndex.par
      parSourcesToIndex.tasksupport = new ForkJoinTaskSupport(threadPool)
      parSourcesToIndex.foreach(f =>
        indexSourceFile(
          f.source,
          Some(f.sourceItem),
          f.targets.headOption,
          Seq(data)
        )
      )
    } finally threadPool.shutdown()
  }

  private def indexDependencySources(
      data: BuildTargets.WritableData,
      dependencySources: b.DependencySourcesResult
  ): Set[AbsolutePath] = {
    // Track used Jars so that we can
    // remove cached symbols from Jars
    // that are not used
    val usedJars = mutable.HashSet.empty[AbsolutePath]
    val isVisited = new ju.HashSet[String]()
    for {
      item <- dependencySources.getItems.asScala
      scalaTarget <- data.scalaTarget(item.getTarget)
      sourceUri <- Option(item.getSources).toList.flatMap(_.asScala)
      path = sourceUri.toAbsolutePath
      _ = data.addDependencySource(path, item.getTarget)
      if !isVisited.contains(sourceUri)
    } {
      isVisited.add(sourceUri)
      try {
        if (path.isJar) {
          usedJars += path
          addSourceJarSymbols(path)
        } else if (path.isDirectory) {
          val dialect =
            ScalaVersions.dialectForScalaVersion(
              scalaTarget.scalaVersion,
              includeSource3 = true
            )
          definitionIndex.addSourceDirectory(path, dialect)
        } else {
          scribe.warn(s"unexpected dependency: $path")
        }
      } catch {
        case NonFatal(e) =>
          scribe.error(s"error processing $sourceUri", e)
      }
    }
    usedJars.toSet
  }

  private def indexJdkSources(
      data: BuildTargets.WritableData,
      dependencySources: b.DependencySourcesResult
  ): Set[AbsolutePath] = {
    // Track used Jars so that we can
    // remove cached symbols from Jars
    // that are not used
    val usedJars = mutable.HashSet.empty[AbsolutePath]
    val jdkSources = JdkSources(userConfig().javaHome)
    jdkSources match {
      case Some(zip) =>
        usedJars += zip
        addSourceJarSymbols(zip)
      case None =>
        scribe.warn(
          s"Could not find java sources in ${userConfig().javaHome}. Java symbols will not be available."
        )
    }
    for {
      item <- dependencySources.getItems.asScala
    } {
      jdkSources.foreach(source =>
        data.addDependencySource(source, item.getTarget)
      )
    }
    usedJars.toSet
  }

  private def indexSourceFile(
      source: AbsolutePath,
      sourceItem: Option[AbsolutePath],
      targetOpt: Option[b.BuildTargetIdentifier],
      data: Seq[BuildTargets.Data]
  ): Unit = {

    try {
      val sourceToIndex0 = sourceMapper.actualSource(source, targetOpt)
      if (sourceToIndex0.exists) {
        val dialect = {
          val scalaVersion =
            targetOpt
              .flatMap(id =>
                data.iterator
                  .flatMap(_.buildTargetInfo.get(id).iterator)
                  .take(1)
                  .toList
                  .headOption
                  .flatMap(_.asScalaBuildTarget)
              )
              .map(_.getScalaVersion())
              .getOrElse(
                scalaVersionSelector.fallbackScalaVersion(
                  source.isAmmoniteScript
                )
              )
          ScalaVersions.dialectForScalaVersion(
            scalaVersion,
            includeSource3 = true
          )
        }
        val reluri = source.toIdeallyRelativeURI(sourceItem)
        val input = sourceToIndex0.toInput
        val symbols = ArrayBuffer.empty[WorkspaceSymbolInformation]
        SemanticdbDefinition.foreach(input, dialect) {
          case SemanticdbDefinition(info, occ, owner) =>
            if (WorkspaceSymbolProvider.isRelevantKind(info.kind)) {
              occ.range.foreach { range =>
                symbols += WorkspaceSymbolInformation(
                  info.symbol,
                  info.kind,
                  range.toLSP
                )
              }
            }
            if (
              sourceItem.isDefined &&
              !info.symbol.isPackage &&
              (owner.isPackage || source.isAmmoniteScript)
            ) {
              definitionIndex.addToplevelSymbol(
                reluri,
                source,
                info.symbol,
                dialect
              )
            }
        }
        workspaceSymbols().didChange(source, symbols)

        // Since the `symbols` here are toplevel symbols,
        // we cannot use `symbols` for expiring the cache for all symbols in the source.
        symbolDocs.expireSymbolDefinition(sourceToIndex0, dialect)
      }
    } catch {
      case NonFatal(e) =>
        scribe.error(source.toString(), e)
    }
  }

  /**
   * Add top level Scala symbols from source JAR into symbol index
   * Uses H2 cache for symbols
   *
   * @param path JAR path
   */
  private def addSourceJarSymbols(path: AbsolutePath): Unit = {
    tables().jarSymbols.getTopLevels(path) match {
      case Some(toplevels) =>
        val dialect = ScalaVersions.dialectForDependencyJar(path.filename)
        definitionIndex.addIndexedSourceJar(path, toplevels, dialect)
      case None =>
        val dialect = ScalaVersions.dialectForDependencyJar(path.filename)
        val toplevels = definitionIndex.addSourceJar(path, dialect)
        tables().jarSymbols.putTopLevels(path, toplevels)
    }
  }

  def reindexWorkspaceSources(
      paths: Seq[AbsolutePath]
  ): Unit = {
    for {
      path <- paths.iterator
      if path.isScalaOrJava
    } {
      indexSourceFile(
        path,
        buildTargets.inverseSourceItem(path),
        None,
        Seq(buildTargetsData)
      )
    }
  }
}
