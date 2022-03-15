package scala.meta.internal.metals.scalacli

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.InputStream
import java.net.URI
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.NoSuchElementException
import java.util.Scanner
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContextExecutorService
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Failure
import scala.util.Properties
import scala.util.Success
import scala.util.control.NonFatal

import scala.meta.internal.bsp.BuildChange
import scala.meta.internal.metals.Buffers
import scala.meta.internal.metals.BuildServerConnection
import scala.meta.internal.metals.Cancelable
import scala.meta.internal.metals.ClosableOutputStream
import scala.meta.internal.metals.Compilations
import scala.meta.internal.metals.Compilers
import scala.meta.internal.metals.Diagnostics
import scala.meta.internal.metals.ImportedBuild
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.MetalsBuildClient
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.MetalsServerConfig
import scala.meta.internal.metals.MutableCancelable
import scala.meta.internal.metals.SocketConnection
import scala.meta.internal.metals.StatusBar
import scala.meta.internal.metals.Tables
import scala.meta.internal.metals.TargetData
import scala.meta.io.AbsolutePath

import coursier.version.Version
import org.eclipse.lsp4j.services.LanguageClient

class ScalaCli(
    compilers: () => Compilers,
    compilations: Compilations,
    statusBar: () => StatusBar,
    buffers: Buffers,
    indexWorkspace: () => Future[Unit],
    diagnostics: () => Diagnostics,
    workspace: () => AbsolutePath,
    tables: () => Tables,
    buildClient: () => MetalsBuildClient,
    languageClient: LanguageClient,
    config: () => MetalsServerConfig
)(implicit ec: ExecutionContextExecutorService)
    extends Cancelable {

  private val cancelables = new MutableCancelable
  private val isCancelled = new AtomicBoolean(false)
  def cancel(): Unit =
    if (isCancelled.compareAndSet(false, true))
      try cancelables.cancel()
      catch {
        case NonFatal(_) =>
      }

  private var buildServer0 = Option.empty[BuildServerConnection]
  private var lastImportedBuild0 = ImportedBuild.empty
  private var roots0 = Seq.empty[AbsolutePath]

  private def allSources(): Seq[AbsolutePath] = {
    val sourceItems = lastImportedBuild0.sources.getItems.asScala.toVector
    val sources = sourceItems.flatMap(_.getSources.asScala.map(_.getUri))
    val roots = sourceItems
      .flatMap(item => Option(item.getRoots).toSeq)
      .flatMap(_.asScala)
    (sources ++ roots).map(new URI(_)).map(AbsolutePath.fromAbsoluteUri(_))
  }

  val buildTargetsData = new TargetData
  def lastImportedBuild: ImportedBuild =
    lastImportedBuild0
  def buildServer: Option[BuildServerConnection] =
    buildServer0
  def roots: Seq[AbsolutePath] =
    roots0

  def importBuild(): Future[Unit] =
    buildServer0 match {
      case None =>
        Future.failed(new Exception("No Scala CLI server running"))
      case Some(conn) =>
        compilers().cancel()

        for {
          build0 <- statusBar().trackFuture(
            "Importing Scala CLI sources",
            ImportedBuild.fromConnection(conn)
          )
          _ = {
            lastImportedBuild0 = build0
            val targets = build0.workspaceBuildTargets.getTargets.asScala
            val connections =
              targets.iterator.map(_.getId).map((_, conn)).toList
            buildTargetsData.resetConnections(connections)
          }
          _ <- indexWorkspace()
          allSources0 = allSources()
          toCompile = buffers.open.toSeq.filter(p =>
            allSources0.exists(root =>
              root == p || p.toNIO.startsWith(root.toNIO)
            )
          )
          _ <- Future.sequence(
            compilations
              .cascadeCompileFiles(toCompile) ::
              compilers().load(toCompile) ::
              Nil
          )
        } yield ()
    }

  private def disconnectOldBuildServer(): Future[Unit] = {
    if (buildServer0.isDefined)
      scribe.info("disconnected: Scala CLI server")
    buildServer0 match {
      case None => Future.unit
      case Some(value) =>
        val allSources0 = allSources()
        buildServer0 = None
        lastImportedBuild0 = ImportedBuild.empty
        roots0 = Nil
        cancelables.cancel()
        diagnostics().reset(allSources0)
        value.shutdown()
    }
  }

  private lazy val javaCommand = {

    val defaultJvmId = "temurin:17"

    val majorVersion = sys.props
      .get("java.version")
      .map(_.takeWhile(_.isDigit))
      .filter(_.nonEmpty)
      .flatMap(_.toIntOption)
      .getOrElse(0)

    if (majorVersion >= 17)
      Paths
        .get(sys.props("java.home"))
        .resolve("bin/java")
        .toAbsolutePath
        .toString
    else {
      scribe.info(
        s"Found Java version $majorVersion. " +
          s"Scala CLI requires at least Java 17, getting a $defaultJvmId JVM via coursier-jvm."
      )
      val jvmManager = coursierapi.JvmManager.create()
      val javaHome = jvmManager.get(defaultJvmId)
      new File(javaHome, "bin/java").getAbsolutePath
    }
  }

  private def scalaCliClassPath(): Seq[String] =
    coursierapi.Fetch
      .create()
      .addDependencies(
        coursierapi.Dependency
          .of("org.virtuslab.scala-cli", "cli_2.12", "0.1.2")
      )
      .addRepositories(
        // for the using_directives dependency of Scala CLI…
        coursierapi.MavenRepository.of(
          "https://oss.sonatype.org/content/repositories/snapshots"
        )
      )
      .fetch()
      .asScala
      .toSeq
      .map(_.getAbsolutePath)

  private lazy val baseCommand = {

    def endsWithCaseInsensitive(s: String, suffix: String): Boolean =
      s.length >= suffix.length &&
        s.regionMatches(
          true,
          s.length - suffix.length,
          suffix,
          0,
          suffix.length
        )

    def findInPath(app: String): Option[Path] = {
      val asIs = Paths.get(app)
      if (Paths.get(app).getNameCount >= 2) Some(asIs)
      else {
        def pathEntries =
          Option(System.getenv("PATH")).iterator
            .flatMap(_.split(File.pathSeparator).iterator)
        def pathSep =
          if (Properties.isWin)
            Option(System.getenv("PATHEXT")).iterator
              .flatMap(_.split(File.pathSeparator).iterator)
          else Iterator("")
        def matches = for {
          dir <- pathEntries
          ext <- pathSep
          app0 = if (endsWithCaseInsensitive(app, ext)) app else app + ext
          path = Paths.get(dir).resolve(app0)
          if Files.isExecutable(path)
        } yield path
        matches.toStream.headOption
      }
    }

    def readFully(is: InputStream): Array[Byte] = {
      val b = new ByteArrayOutputStream
      val buf = Array.ofDim[Byte](64)
      var read = -1
      while ({
        read = is.read(buf)
        read >= 0
      })
        if (read > 0)
          b.write(buf, 0, read)
      b.toByteArray
    }

    def requireMinVersion(executable: Path, minVersion: String): Boolean = {
      val process =
        new ProcessBuilder(executable.toAbsolutePath.toString, "version")
          .redirectError(ProcessBuilder.Redirect.INHERIT)
          .redirectOutput(ProcessBuilder.Redirect.PIPE)
          .redirectInput(ProcessBuilder.Redirect.PIPE)
          .start()

      val b = readFully(process.getInputStream())
      val version = Version(new String(b, "UTF-8").trim())
      val minVersion0 = Version(minVersion)
      minVersion0.compareTo(version) <= 0
    }

    val cliCommand = sys.props
      .get("metals.scala-cli.launcher")
      .map(Seq(_))
      .orElse {
        findInPath("scala-cli")
          .filter(requireMinVersion(_, "0.1.3"))
          .map(p => Seq(p.toString))
      }
      .getOrElse {
        scribe.warn(
          "scala-cli not found in PATH, fetching and starting a JVM-based Scala CLI"
        )
        val cp = scalaCliClassPath()
        Seq(
          javaCommand,
          "-cp",
          cp.mkString(File.pathSeparator),
          "scala.cli.ScalaCli"
        )
      }
    cliCommand ++ Seq("bsp")
  }

  private def loaded(path: AbsolutePath): Boolean =
    roots0.contains(path)

  def maybeImport(path: AbsolutePath): Option[Future[Unit]] = {
    val directory = path.parent
    if (loaded(directory))
      None
    else {
      def doImport(): Unit =
        start(roots0 :+ directory).onComplete({
          case Failure(e) =>
            languageClient.showMessage(
              Messages.ImportScalaCliProject.ImportFailed(path.toString)
            )
            scribe.warn(s"Error importing Scala CLI project $directory", e)
          case Success(_) =>
            languageClient.showMessage(
              Messages.ImportScalaCliProject.Imported
            )
            scribe.info(s"Imported Scala CLI project $directory")
        })(ec)

      val autoImport =
        tables().dismissedNotifications.ScalaCliImportAuto.isDismissed

      val futureRes =
        if (autoImport) {
          doImport()
          Future.unit
        } else {
          val futureResp = languageClient
            .showMessageRequest(Messages.ImportScalaCliProject.params())
            .asScala
          futureResp.onComplete({
            case Failure(e) =>
              scribe.warn("Error requesting Scala CLI project import", e)
            case Success(null) =>
              scribe.warn("Scala CLI project import cancelled by user")
            case Success(resp) =>
              resp.getTitle match {
                case Messages.ImportScalaCliProject.importAll =>
                  tables().dismissedNotifications.ScalaCliImportAuto
                    .dismissForever()
                  doImport()
                case Messages.ImportScalaCliProject.doImport =>
                  doImport()
                case _ =>
              }
          })(ec)
          futureResp.ignoreValue(ec)
        }
      Some(futureRes)
    }
  }

  def start(roots: Seq[AbsolutePath]): Future[Unit] = {

    disconnectOldBuildServer().onComplete({
      case Failure(e) =>
        scribe.warn("Error disconnecting old Scala CLI server", e)
      case Success(()) =>
    })(ec)

    val command =
      baseCommand ++ Seq(roots.head.toString) ++ roots.tail.flatMap(p =>
        Seq(";", p.toString)
      )

    val futureConn = BuildServerConnection.fromSockets(
      workspace(),
      buildClient(),
      languageClient,
      () => ScalaCli.socketConn(command, workspace())(ec),
      tables().dismissedNotifications.ReconnectScalaCli,
      config(),
      "Scala CLI"
    )

    val f = for {
      conn <- futureConn
      _ <- connectToNewBuildServer(conn, roots)
    } yield ()

    f.transform {
      case Failure(ex) =>
        scribe.error("Error starting Scala CLI", ex)
        Success(())
      case Success(other) =>
        scribe.info("Scala CLI started")
        Success(other)
    }
  }

  def stop(): CompletableFuture[Object] =
    disconnectOldBuildServer().asJavaObject

  private def connectToNewBuildServer(
      build: BuildServerConnection,
      roots: Seq[AbsolutePath]
  ): Future[BuildChange] = {
    scribe.info(s"Connected to Scala CLI server v${build.version}")
    cancelables.add(build)
    buildServer0 = Some(build)
    roots0 = roots
    for {
      _ <- importBuild()
    } yield BuildChange.Reconnected
  }

}

object ScalaCli {

  private def socketConn(
      command: Seq[String],
      workspace: AbsolutePath
  )(implicit ec: ExecutionContext): Future[SocketConnection] =
    // meh, blocks on random ec
    Future {
      scribe.info(s"Running $command")
      val b = new ProcessBuilder(command: _*)
        .redirectInput(ProcessBuilder.Redirect.PIPE)
        .redirectOutput(ProcessBuilder.Redirect.PIPE)
        .redirectError(ProcessBuilder.Redirect.PIPE)
        .directory(workspace.toFile)
      val proc = b.start()
      val os = new ClosableOutputStream(proc.getOutputStream, "Scala CLI")
      var stopSendingOutput = false
      val sendOutput = logOutputThread(proc.getErrorStream, stopSendingOutput)
      sendOutput.start()
      val finished = Promise[Unit]()
      Future {
        proc.waitFor()
        finished.success(())
        ()
      }.onComplete {
        case Success(()) =>
        case f @ Failure(_) => finished.tryComplete(f)
      }
      SocketConnection(
        "Ammonite",
        os,
        proc.getInputStream,
        List(
          Cancelable { () => proc.destroyForcibly() },
          Cancelable { () => stopSendingOutput = true }
        ),
        finished
      )
    }

  private val threadCounter = new AtomicInteger
  private def logOutputThread(
      is: InputStream,
      stopSendingOutput: => Boolean
  ): Thread =
    new Thread(
      s"scala-cli-bsp-stderr-to-metals-log-${threadCounter.incrementAndGet()}"
    ) {
      setDaemon(true)
      override def run(): Unit = {
        val reader = new Scanner(is)
        var line: String = null
        while (
          !stopSendingOutput && {
            line =
              try reader.nextLine()
              catch {
                case _: NoSuchElementException =>
                  null
              }
            line != null
          }
        )
          scribe.info("Scala CLI: " + line)
      }
    }

}
