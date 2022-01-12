package scala.meta.internal.metals

import java.lang.{Iterable => JIterable}
import java.net.URLClassLoader
import java.util
import java.util.concurrent.ConcurrentLinkedQueue
import java.{util => ju}

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

import scala.meta.internal.io.PathIO
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ammonite.Ammonite
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTarget
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import ch.epfl.scala.bsp4j.JavacOptionsResult
import ch.epfl.scala.bsp4j.ScalacOptionsResult
import ch.epfl.scala.bsp4j.WorkspaceBuildTargetsResult

/**
 * In-memory cache for looking up build server metadata.
 */
final class BuildTargets(
    ammoniteBuildServer: BuildTargetIdentifier => Option[BuildServerConnection]
) {
  private var workspace = PathIO.workingDirectory
  def setWorkspaceDirectory(newWorkspace: AbsolutePath): Unit = {
    workspace = newWorkspace
  }
  private var tables: Option[Tables] = None
  private var data: BuildTargets.DataSeq =
    BuildTargets.DataSeq(BuildTargets.Data.create(), Nil)
  def allWritableData = data.writableDataIterator.toSeq

  val buildTargetsOrder: BuildTargetIdentifier => Int = {
    (t: BuildTargetIdentifier) =>
      var score = 1

      val isSupportedScalaVersion = scalaTarget(t).exists(t =>
        ScalaVersions.isSupportedAtReleaseMomentScalaVersion(
          t.scalaVersion
        )
      )
      if (isSupportedScalaVersion) score <<= 2

      val usesJavac = javaTarget(t).nonEmpty
      val isJVM = scalaTarget(t).exists(_.scalac.isJVM)
      if (usesJavac) score <<= 1
      else if (isJVM) score <<= 1

      // note(@tgodzik) once the support for Scala 3 is on par with Scala 2 this can be removed
      val isScala2 = scalaTarget(t).exists(info =>
        !ScalaVersions.isScala3Version(info.scalaVersion)
      )
      if (isScala2) score <<= 1

      score
  }

  def setTables(newTables: Tables): Unit = {
    tables = Some(newTables)
  }
  def sourceItems: Iterable[AbsolutePath] =
    data.tail.foldLeft(data.head.sourceItemsToBuildTarget.keys)(
      _ ++ _.sourceItemsToBuildTarget.keys
    )
  def sourceItemsToBuildTargets
      : Iterator[(AbsolutePath, JIterable[BuildTargetIdentifier])] =
    data.iterator.flatMap(_.sourceItemsToBuildTarget.iterator)
  private def allBuildTargetIdsInternal: Iterator[(BuildTargets.Data, BuildTargetIdentifier)] =
    data.iterator.flatMap(d => d.allBuildTargetIds.map((d, _)))
  def allBuildTargetIds: Seq[BuildTargetIdentifier] =
    allBuildTargetIdsInternal.map(_._2).toVector

  def allTargetRoots: Iterator[AbsolutePath] =
    data.iterator.flatMap(_.allTargetRoots)

  def all: Iterator[BuildTarget] =
    data.iterator.flatMap(_.all)

  def allScala: Iterator[ScalaTarget] =
    data.iterator.flatMap(_.allScala)

  def allJava: Iterator[JavaTarget] =
    data.iterator.flatMap(_.allJava)

  def info(id: BuildTargetIdentifier): Option[BuildTarget] =
    data.iterator.flatMap(_.info(id).iterator).take(1).toList.headOption

  def scalaTarget(id: BuildTargetIdentifier): Option[ScalaTarget] =
    data.iterator.flatMap(_.scalaTarget(id).iterator).toStream.headOption

  def javaTarget(id: BuildTargetIdentifier): Option[JavaTarget] =
    data.iterator.flatMap(_.javaTarget(id).iterator).toStream.headOption

  def targetJarClasspath(
      id: BuildTargetIdentifier
  ): Option[List[AbsolutePath]] =
    data.iterator.flatMap(_.targetJarClasspath(id).iterator).take(1).toList.headOption

  def targetClassDirectories(
      id: BuildTargetIdentifier
  ): List[String] =
    data.iterator.flatMap(_.targetClassDirectories(id).iterator).toList

  def allWorkspaceJars: Iterator[AbsolutePath] =
    data.iterator.flatMap(_.allWorkspaceJars)

  def allSourceJars: Iterator[AbsolutePath] =
    data.iterator
      .flatMap(_.inverseDependencySources.keysIterator)

  def buildTargetSources(
      id: BuildTargetIdentifier
  ): Iterable[AbsolutePath] =
    data.iterator
      .flatMap(_.buildTargetSources.get(id).iterator)
      .toStream
      .headOption
      .toSeq
      .flatMap(_.asScala)

  def buildTargetTransitiveSources(
      id: BuildTargetIdentifier
  ): Iterator[AbsolutePath] = {
    for {
      dependency <- buildTargetTransitiveDependencies(id).iterator
      sources <- data.iterator
        .flatMap(_.buildTargetSources.get(dependency).iterator)
        .take(1)
      source <- sources.asScala.iterator
    } yield source
  }

  def buildTargetTransitiveDependencies(
      id: BuildTargetIdentifier
  ): Iterable[BuildTargetIdentifier] = {
    val isVisited = mutable.Set.empty[BuildTargetIdentifier]
    val toVisit = new java.util.ArrayDeque[BuildTargetIdentifier]
    toVisit.add(id)
    while (!toVisit.isEmpty) {
      val next = toVisit.pop()
      if (!isVisited(next)) {
        isVisited.add(next)
        for {
          info <- info(next).iterator
          dependency <- info.getDependencies.asScala.iterator
        } {
          toVisit.add(dependency)
        }
      }
    }
    isVisited
  }

  def targetRoots(
      buildTarget: BuildTargetIdentifier
  ): List[AbsolutePath] = {
    val javaRoot = javaTargetRoot(buildTarget).toList
    val scalaRoot = scalaTargetRoot(buildTarget).toList
    (javaRoot ++ scalaRoot).distinct
  }

  def javaTargetRoot(
      buildTarget: BuildTargetIdentifier
  ): Option[AbsolutePath] =
    data.iterator.flatMap(_.javaTargetRoot(buildTarget).iterator).take(1).toList.headOption

  def scalaTargetRoot(
      buildTarget: BuildTargetIdentifier
  ): Option[AbsolutePath] =
    data.iterator.flatMap(_.scalaTargetRoot(buildTarget).iterator).take(1).toList.headOption

  def workspaceDirectory(
      buildTarget: BuildTargetIdentifier
  ): Option[AbsolutePath] =
    buildServerOf(buildTarget).map(_.workspaceDirectory)

  /**
   * Returns the first build target containing this source file.
   */
  def inverseSources(
      source: AbsolutePath
  ): Option[BuildTargetIdentifier] = {
    val buildTargets = sourceBuildTargets(source)
    val orSbtBuildTarget =
      buildTargets.getOrElse(sbtBuildScalaTarget(source).toIterable)
    if (orSbtBuildTarget.isEmpty) {
      tables
        .flatMap(_.dependencySources.getBuildTarget(source))
        .orElse(inferBuildTarget(source))
    } else {
      Some(orSbtBuildTarget.maxBy(buildTargetsOrder))
    }
  }

  def scalaVersion(source: AbsolutePath): Option[String] = {
    for {
      id <- inverseSources(source)
      target <- scalaTarget(id)
    } yield target.scalaVersion
  }

  /**
   * Resolves sbt auto imports if a file belongs to a Sbt build target.
   */
  def sbtAutoImports(path: AbsolutePath): Option[Seq[String]] =
    for {
      targetId <- inverseSources(path)
      target <- scalaTarget(targetId)
      imports <- target.autoImports
    } yield imports

  /**
   * Tries to guess what build target this readonly file belongs to from the symbols it defines.
   *
   * By default, we rely on carefully recording what build target produced what
   * files in the `.metals/readonly/` directory. This approach has the problem
   * that navigation failed to work in `readonly/` sources if
   *
   * - a new metals feature forgot to record the build target
   * - a user removes `.metals/metals.h2.db`
   *
   * When encountering an unknown `readonly/` file we do the following steps to
   * infer what build target it belongs to:
   *
   * - check if file is in `.metals/readonly/dependencies/${source-jar-name}`
   * - find the build targets that have a sourceDependency with that name
   *
   * Otherwise if it's a jar file we find a build target it belongs to.
   *
   * This approach is not glamorous but it seems to work reasonably well.
   */
  def inferBuildTarget(
      source: AbsolutePath
  ): Option[BuildTargetIdentifier] = {
    val readonly = workspace.resolve(Directories.readonly)
    source.toRelativeInside(readonly) match {
      case Some(rel) =>
        val names = rel.toNIO.iterator().asScala.toList.map(_.filename)
        names match {
          case Directories.dependenciesName :: jarName :: _ =>
            // match build target by source jar name
            sourceJarFile(jarName)
              .flatMap(inverseDependencySource(_).headOption)
          case _ =>
            None
        }
      case None =>
        // else it can be a source file inside a jar
        val fromJar = jarPath(source)
          .flatMap { jar =>
            allBuildTargetIdsInternal.find { case (_, id) =>
              targetJarClasspath(id).exists(_.contains(jar))
            }
          }
        for ((data0, id) <- fromJar)
          data0.addSourceItem(source, id)
        fromJar.map(_._2)
    }
  }

  def findByDisplayName(name: String): Option[BuildTarget] = {
    data.iterator
      .flatMap(_.buildTargetInfo.valuesIterator)
      .find(_.getDisplayName() == name)
      .toStream
      .headOption
  }

  private def jarPath(source: AbsolutePath): Option[AbsolutePath] = {
    source.jarPath.map { sourceJarPath =>
      sourceJarPath.parent.resolve(
        source.filename.replace("-sources.jar", ".jar")
      )
    }
  }

  /**
   * Returns meta build target for `*.sbt` or `*.scala`  files.
   * It selects build target by directory of its connection
   *   because `*.sbt` and `*.scala` aren't included in `sourceFiles` set
   */
  def sbtBuildScalaTarget(
      file: AbsolutePath
  ): Option[BuildTargetIdentifier] = {
    val targetMetaBuildDir =
      if (file.isSbt) file.parent.resolve("project") else file.parent
    data.iterator
      .flatMap(_.buildTargetInfo.valuesIterator)
      .find { target =>
        val isMetaBuild = target.isSbtBuild
        if (isMetaBuild) {
          workspaceDirectory(target.getId)
            .map(_ == targetMetaBuildDir)
            .getOrElse(false)
        } else {
          false
        }
      }
      .map(_.getId())
      .toStream
      .headOption
  }

  case class InferredBuildTarget(
      jar: AbsolutePath,
      symbol: String,
      id: BuildTargetIdentifier
  )
  def inferBuildTarget(
      toplevels: Iterable[Symbol]
  ): Option[InferredBuildTarget] = {
    val classloader = new URLClassLoader(
      allWorkspaceJars.map(_.toNIO.toUri().toURL()).toArray,
      null
    )
    lazy val classpaths: Seq[(BuildTargetIdentifier, Seq[AbsolutePath])] =
      allBuildTargetIdsInternal.toVector.map { case (data, id) =>
        id -> data.targetClasspath(id)
          .map(_.toAbsoluteClasspath.toSeq)
          .getOrElse(Seq.empty)
      }

    try {
      toplevels.foldLeft(Option.empty[InferredBuildTarget]) {
        case (Some(x), _) => Some(x)
        case (None, toplevel) =>
          val classfile = toplevel.owner.value + toplevel.displayName + ".class"
          val resource = classloader
            .findResource(classfile)
            .toURI()
            .toString()
            .replaceFirst("!/.*", "")
            .stripPrefix("jar:")
          val path = resource.toAbsolutePath
          classpaths.collectFirst {
            case (id, classpath) if classpath.contains(path) =>
              InferredBuildTarget(path, toplevel.value, id)
          }
      }
    } catch {
      case NonFatal(_) =>
        None
    } finally {
      classloader.close()
    }
  }

  def sourceBuildTargets(
      sourceItem: AbsolutePath
  ): Option[Iterable[BuildTargetIdentifier]] =
    data.iterator
      .flatMap(_.sourceBuildTargets(sourceItem).iterator)
      .toStream
      .headOption

  def inverseSourceItem(source: AbsolutePath): Option[AbsolutePath] =
    sourceItems.find(item => source.toNIO.startsWith(item.toNIO))

  def originalInverseSourceItem(source: AbsolutePath): Option[AbsolutePath] =
    data.iterator
      .flatMap(_.originalSourceItems.asScala.iterator)
      .find(item => source.toNIO.startsWith(item.dealias.toNIO))

  def isInverseDependency(
      query: BuildTargetIdentifier,
      roots: List[BuildTargetIdentifier]
  ): Boolean = {
    BuildTargets.isInverseDependency(
      query,
      roots,
      id =>
        data.iterator
          .flatMap(_.inverseDependencies.get(id).iterator)
          .toStream
          .headOption
    )
  }
  def inverseDependencyLeaves(
      target: BuildTargetIdentifier
  ): collection.Set[BuildTargetIdentifier] = {
    computeInverseDependencies(target).leaves
  }
  def allInverseDependencies(
      target: BuildTargetIdentifier
  ): collection.Set[BuildTargetIdentifier] = {
    computeInverseDependencies(target).visited
  }
  private def computeInverseDependencies(
      target: BuildTargetIdentifier
  ): BuildTargets.InverseDependencies = {
    BuildTargets.inverseDependencies(
      List(target),
      id =>
        data.iterator
          .flatMap(_.inverseDependencies.get(id).iterator)
          .toStream
          .headOption
    )
  }

  def sourceJarFile(sourceJarName: String): Option[AbsolutePath] =
    data.iterator
      .flatMap(_.sourceJarNameToJarFile.get(sourceJarName).iterator)
      .toStream
      .headOption

  def inverseDependencySource(
      sourceJar: AbsolutePath
  ): collection.Set[BuildTargetIdentifier] = {
    data.iterator
      .flatMap(_.inverseDependencySources.get(sourceJar).iterator)
      .toStream
      .headOption
      .getOrElse(Set.empty)
  }

  def sourceRoots: Iterable[AbsolutePath] = {
    data.iterable.flatMap(_.isSourceRoot.asScala)
  }

  def isInsideSourceRoot(path: AbsolutePath): Boolean = {
    data.iterator.exists(_.isSourceRoot.contains(path)) &&
    data.iterator.flatMap(_.isSourceRoot.asScala.iterator).exists { root =>
      path.toNIO.startsWith(root.toNIO)
    }
  }

  def buildServerOf(
      id: BuildTargetIdentifier
  ): Option[BuildServerConnection] = {
    ammoniteBuildServer(id).orElse(
      data.iterator
        .flatMap(_.targetToConnection.get(id).iterator)
        .toStream
        .headOption
    )
  }

  def addData(data: BuildTargets.WritableData): Unit =
    this.data = BuildTargets.DataSeq(data, this.data.head :: this.data.tail)
}

object BuildTargets {

  def withAmmonite(
      ammonite: () => Ammonite
  ): BuildTargets = {
    val ammoniteBuildServerF =
      (id: BuildTargetIdentifier) =>
        if (Ammonite.isAmmBuildTarget(id)) ammonite().buildServer
        else None

    new BuildTargets(ammoniteBuildServerF)
  }

  def withoutAmmonite(): BuildTargets =
    new BuildTargets(_ => None)

  def isInverseDependency(
      query: BuildTargetIdentifier,
      roots: List[BuildTargetIdentifier],
      inverseDeps: BuildTargetIdentifier => Option[Seq[BuildTargetIdentifier]]
  ): Boolean = {
    val isVisited = mutable.Set.empty[BuildTargetIdentifier]
    @tailrec
    def loop(toVisit: List[BuildTargetIdentifier]): Boolean =
      toVisit match {
        case Nil => false
        case head :: tail =>
          if (head == query) true
          else if (isVisited(head)) false
          else {
            isVisited += head
            inverseDeps(head) match {
              case Some(next) =>
                loop(next.toList ++ tail)
              case None =>
                loop(tail)
            }
          }
      }
    loop(roots)
  }

  /**
   * Given an acyclic graph and a root target, returns the leaf nodes that depend on the root target.
   *
   * For example, returns `[D, E, C]` given the following graph with root A: {{{
   *      A
   *    ^   ^
   *    |   |
   *    B   C
   *   ^ ^
   *   | |
   *   D E
   * }}}
   */
  def inverseDependencies(
      root: List[BuildTargetIdentifier],
      inverseDeps: BuildTargetIdentifier => Option[Seq[BuildTargetIdentifier]]
  ): InverseDependencies = {
    val isVisited = mutable.Set.empty[BuildTargetIdentifier]
    val leaves = mutable.Set.empty[BuildTargetIdentifier]
    def loop(toVisit: List[BuildTargetIdentifier]): Unit =
      toVisit match {
        case Nil => ()
        case head :: tail =>
          if (!isVisited(head)) {
            isVisited += head
            inverseDeps(head) match {
              case Some(next) =>
                loop(next.toList)
              case None =>
                // Only add leaves of the tree to the result to minimize the number
                // of targets that we compile. If `B` depends on `A`, it's faster
                // in Bloop to compile only `B` than `A+B`.
                leaves += head
            }
            loop(tail)
          }
      }
    loop(root)
    InverseDependencies(isVisited, leaves)
  }

  case class InverseDependencies(
      visited: collection.Set[BuildTargetIdentifier],
      leaves: collection.Set[BuildTargetIdentifier]
  )

  trait Data {
    def sourceItemsToBuildTarget
        : scala.collection.Map[AbsolutePath, ConcurrentLinkedQueue[
          BuildTargetIdentifier
        ]]
    def buildTargetInfo
        : scala.collection.Map[BuildTargetIdentifier, BuildTarget]
    def javaTargetInfo
        : scala.collection.Map[BuildTargetIdentifier, JavaTarget]
    def scalaTargetInfo
        : scala.collection.Map[BuildTargetIdentifier, ScalaTarget]
    def inverseDependencies
        : scala.collection.Map[BuildTargetIdentifier, ListBuffer[
          BuildTargetIdentifier
        ]]
    def buildTargetSources
        : scala.collection.Map[BuildTargetIdentifier, util.Set[AbsolutePath]]
    def inverseDependencySources
        : scala.collection.Map[AbsolutePath, Set[BuildTargetIdentifier]]
    def sourceJarNameToJarFile
        : scala.collection.Map[String, AbsolutePath]
    def isSourceRoot: java.util.Set[AbsolutePath]
    // if workspace contains symlinks, original source items are kept here and source items dealiased
    def originalSourceItems: java.util.Set[AbsolutePath]

    def targetToConnection
        : scala.collection.Map[BuildTargetIdentifier, BuildServerConnection]
    def sourceBuildTargets(
        sourceItem: AbsolutePath
    ): Option[Iterable[BuildTargetIdentifier]]

    def allTargetRoots: Iterator[AbsolutePath]
    def all: Iterator[BuildTarget]
    def allBuildTargetIds: Seq[BuildTargetIdentifier]
    def allScala: Iterator[ScalaTarget]
    def allJava: Iterator[JavaTarget]
    def scalaTarget(id: BuildTargetIdentifier): Option[ScalaTarget]
    def javaTarget(id: BuildTargetIdentifier): Option[JavaTarget]

    def targetRoots(buildTarget: BuildTargetIdentifier): List[AbsolutePath]
    def javaTargetRoot(buildTarget: BuildTargetIdentifier): Option[AbsolutePath]
    def scalaTargetRoot(buildTarget: BuildTargetIdentifier): Option[AbsolutePath]

    def info(id: BuildTargetIdentifier): Option[BuildTarget]
    def targetJarClasspath(id: BuildTargetIdentifier): Option[List[AbsolutePath]]
    def targetClasspath(id: BuildTargetIdentifier): Option[List[String]]
    def targetClassDirectories(id: BuildTargetIdentifier): List[String]

    def allWorkspaceJars: Iterator[AbsolutePath]

    def addSourceItem(
        sourceItem: AbsolutePath,
        buildTarget: BuildTargetIdentifier
    ): Unit
  }

  final case class DataSeq(head: WritableData, tail: List[WritableData]) {
    private lazy val list: List[Data] = head :: tail
    def iterator: Iterator[Data] = list.iterator
    def writableDataIterator: Iterator[WritableData] = (head :: tail).iterator
    def iterable: Iterable[Data] = list.toIterable
  }

  trait WritableData extends Data {
    def linkSourceFile(id: BuildTargetIdentifier, source: AbsolutePath): Unit
    def reset(): Unit
    def addWorkspaceBuildTargets(result: WorkspaceBuildTargetsResult): Unit
    def addScalacOptions(result: ScalacOptionsResult): Unit
    def addJavacOptions(result: JavacOptionsResult): Unit
    def addDependencySource(
        sourcesJar: AbsolutePath,
        target: BuildTargetIdentifier
    ): Unit
    def resetConnections(
        idToConn: List[(BuildTargetIdentifier, BuildServerConnection)]
    ): Unit
    def onCreate(source: AbsolutePath): Unit
  }

  object Data {
    def create(): WritableData =
      new DataImpl
  }

  private final class DataImpl extends WritableData {

    val sourceItemsToBuildTarget =
      TrieMap.empty[AbsolutePath, ConcurrentLinkedQueue[BuildTargetIdentifier]]
    val buildTargetInfo =
      TrieMap.empty[BuildTargetIdentifier, BuildTarget]
    val javaTargetInfo =
      TrieMap.empty[BuildTargetIdentifier, JavaTarget]
    val scalaTargetInfo =
      TrieMap.empty[BuildTargetIdentifier, ScalaTarget]
    val inverseDependencies =
      TrieMap.empty[BuildTargetIdentifier, ListBuffer[BuildTargetIdentifier]]
    val buildTargetSources =
      TrieMap.empty[BuildTargetIdentifier, util.Set[AbsolutePath]]
    val inverseDependencySources =
      TrieMap.empty[AbsolutePath, Set[BuildTargetIdentifier]]
    val sourceJarNameToJarFile = TrieMap.empty[String, AbsolutePath]
    val isSourceRoot =
      ConcurrentHashSet.empty[AbsolutePath]
    // if workspace contains symlinks, original source items are kept here and source items dealiased
    val originalSourceItems = ConcurrentHashSet.empty[AbsolutePath]

    val targetToConnection =
      new mutable.HashMap[BuildTargetIdentifier, BuildServerConnection]

    def reset(): Unit = {
      sourceItemsToBuildTarget.values.foreach(_.clear())
      sourceItemsToBuildTarget.clear()
      buildTargetInfo.clear()
      javaTargetInfo.clear()
      scalaTargetInfo.clear()
      inverseDependencies.clear()
      buildTargetSources.clear()
      inverseDependencySources.clear()
      sourceJarNameToJarFile.clear()
      isSourceRoot.clear()
    }

    def addSourceItem(
        sourceItem: AbsolutePath,
        buildTarget: BuildTargetIdentifier
    ): Unit = {
      val dealiased = sourceItem.dealias
      if (dealiased != sourceItem)
        originalSourceItems.add(sourceItem)

      val queue = sourceItemsToBuildTarget.getOrElseUpdate(
        dealiased,
        new ConcurrentLinkedQueue()
      )
      queue.add(buildTarget)
    }

    def linkSourceFile(id: BuildTargetIdentifier, source: AbsolutePath): Unit = {
      val set = buildTargetSources.getOrElseUpdate(id, ConcurrentHashSet.empty)
      set.add(source)
    }

    def addWorkspaceBuildTargets(result: WorkspaceBuildTargetsResult): Unit = {
      result.getTargets.asScala.foreach { target =>
        buildTargetInfo(target.getId) = target
        target.getDependencies.asScala.foreach { dependency =>
          val buf =
            inverseDependencies.getOrElseUpdate(dependency, ListBuffer.empty)
          buf += target.getId
        }
      }
    }

    def addScalacOptions(result: ScalacOptionsResult): Unit = {
      result.getItems.asScala.foreach { scalac =>
        info(scalac.getTarget()).foreach { info =>
          info.asScalaBuildTarget.foreach { scalaBuildTarget =>
            val sbtTarget = info.asSbtBuildTarget
            val autoImports = sbtTarget.map(_.getAutoImports.asScala)
            scalaTargetInfo(scalac.getTarget) = ScalaTarget(
              info,
              scalaBuildTarget,
              scalac,
              autoImports,
              sbtTarget.map(_.getSbtVersion())
            )
          }
        }
      }
    }

    def addJavacOptions(result: JavacOptionsResult): Unit = {
      result.getItems.asScala.foreach { javac =>
        info(javac.getTarget()).foreach { info =>
          javaTargetInfo(javac.getTarget) = JavaTarget(info, javac)
        }
      }
    }

    def addDependencySource(
        sourcesJar: AbsolutePath,
        target: BuildTargetIdentifier
    ): Unit = {
      val acc = inverseDependencySources.getOrElse(sourcesJar, Set.empty)
      inverseDependencySources(sourcesJar) = acc + target
    }

    def resetConnections(
        idToConn: List[(BuildTargetIdentifier, BuildServerConnection)]
    ): Unit = {
      targetToConnection.clear()
      idToConn.foreach { case (id, conn) => targetToConnection.put(id, conn) }
    }

    def onCreate(source: AbsolutePath): Unit =
      for (targets <- sourceBuildTargets(source); buildTarget <- targets)
        linkSourceFile(buildTarget, source)

    def sourceBuildTargets(
        sourceItem: AbsolutePath
    ): Option[Iterable[BuildTargetIdentifier]] =
      sourceItemsToBuildTarget.collectFirst {
        case (source, buildTargets)
            if sourceItem.toNIO.getFileSystem == source.toNIO.getFileSystem &&
              sourceItem.toNIO.startsWith(source.toNIO) =>
          buildTargets.asScala
      }

    def allTargetRoots: Iterator[AbsolutePath] = {
      val scalaTargetRoots = scalaTargetInfo.map(_._2.targetroot)
      val javaTargetRoots = javaTargetInfo.map(_._2.targetroot)
      val allTargetRoots = scalaTargetRoots.toSet ++ javaTargetRoots.toSet
      allTargetRoots.iterator
    }
    def all: Iterator[BuildTarget] =
      buildTargetInfo.values.toIterator
    def allBuildTargetIds: Seq[BuildTargetIdentifier] =
      buildTargetInfo.keys.toSeq
    def allScala: Iterator[ScalaTarget] =
      scalaTargetInfo.values.toIterator
    def allJava: Iterator[JavaTarget] =
      javaTargetInfo.values.toIterator
    def scalaTarget(id: BuildTargetIdentifier): Option[ScalaTarget] =
      scalaTargetInfo.get(id)
    def javaTarget(id: BuildTargetIdentifier): Option[JavaTarget] =
      javaTargetInfo.get(id)

    def targetRoots(
        buildTarget: BuildTargetIdentifier
    ): List[AbsolutePath] = {
      val javaRoot = javaTargetRoot(buildTarget).toList
      val scalaRoot = scalaTargetRoot(buildTarget).toList
      (javaRoot ++ scalaRoot).distinct
    }

    def javaTargetRoot(
        buildTarget: BuildTargetIdentifier
    ): Option[AbsolutePath] =
      javaTarget(buildTarget).map(_.targetroot)

    def scalaTargetRoot(
        buildTarget: BuildTargetIdentifier
    ): Option[AbsolutePath] =
      scalaTarget(buildTarget).map(_.targetroot)

    def info(id: BuildTargetIdentifier): Option[BuildTarget] =
      buildTargetInfo.get(id)

    def targetJarClasspath(id: BuildTargetIdentifier): Option[List[AbsolutePath]] = {
      val scalacData = scalaTarget(id).map(_.scalac.jarClasspath)
      val javacData = javaTarget(id).map(_.javac.jarClasspath)
      scalacData
        .flatMap(s => javacData.map(j => (s ::: j).distinct).orElse(scalacData))
        .orElse(javacData)
    }

    def targetClasspath(
        id: BuildTargetIdentifier
    ): Option[List[String]] = {
      val scalacData = scalaTarget(id).map(_.scalac.classpath)
      val javacData = javaTarget(id).map(_.javac.classpath)
      scalacData
        .flatMap(s => javacData.map(j => (s ::: j).distinct).orElse(scalacData))
        .orElse(javacData)
    }

    def targetClassDirectories(id: BuildTargetIdentifier): List[String] = {
      val scalacData = scalaTarget(id).map(_.scalac.getClassDirectory).toList
      val javacData = javaTarget(id).map(_.javac.getClassDirectory).toList
      (scalacData ++ javacData).distinct
    }

    def allWorkspaceJars: Iterator[AbsolutePath] = {
      val isVisited = new ju.HashSet[AbsolutePath]()

      Iterator(
        for {
          targetId <- allBuildTargetIds
          classpathEntries <- targetJarClasspath(targetId).toList
          classpathEntry <- classpathEntries
          if isVisited.add(classpathEntry)
        } yield classpathEntry,
        PackageIndex.bootClasspath.iterator
      ).flatten
    }
  }

}
