package tests

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.IOException
import java.io.InputStream
import java.nio.file.Files
import java.util.Locale
import java.util.UUID
import java.util.zip.GZIPInputStream
import java.util.zip.ZipFile

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Properties

import scala.meta.internal.builds.BuildTool
import scala.meta.internal.metals.Messages.ImportBuild
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.ServerCommands
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BspConnectionDetails
import com.google.gson.Gson
import coursier.cache.FileCache
import coursier.util.Artifact
import coursier.util.Task
import org.eclipse.lsp4j.MessageActionItem

sealed trait BuildServerInitializer {
  def initialize(
      workspace: AbsolutePath,
      server: TestingServer,
      client: TestingClient,
      layout: String,
      expectError: Boolean
  )(implicit ec: ExecutionContext): Future[Unit]
}

/**
 * Set up your workspace using QuickBuild as your build tool.
 * This will take your `metals.json` file and quickly produce `.bloop/` files from it.
 */
object QuickBuildInitializer extends BuildServerInitializer {
  this: BaseLspSuite =>
  override def initialize(
      workspace: AbsolutePath,
      server: TestingServer,
      client: TestingClient,
      layout: String,
      expectError: Boolean
  )(implicit ec: ExecutionContext): Future[Unit] = {
    QuickBuild.bloopInstall(workspace)
    for {
      _ <- server.initialize()
      _ <- server.initialized()
    } yield {
      if (!expectError) {
        server.assertBuildServerConnection()
      }
    }
  }
}

/**
 * Set up your workspace by responding to an Import Build request which will
 * run Bloop Install via the build tool being used.
 */
object BloopImportInitializer extends BuildServerInitializer {
  this: BaseLspSuite =>
  override def initialize(
      workspace: AbsolutePath,
      server: TestingServer,
      client: TestingClient,
      layout: String,
      expectError: Boolean
  )(implicit ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- server.initialize()
      // Import build using Bloop
      _ = client.importBuild = ImportBuild.yes
      _ <- server.initialized()
    } yield {
      if (!expectError) {
        server.assertBuildServerConnection()
      }
    }
  }
}

/**
 * Assumes sbt is being used as a build tool and also for your BSP server.
 * This generates the .bsp/sbt.json file and invoke the BSP switch command
 * with sbt as the build server.
 */
object SbtServerInitializer extends BuildServerInitializer {
  this: BaseLspSuite =>
  override def initialize(
      workspace: AbsolutePath,
      server: TestingServer,
      client: TestingClient,
      layout: String,
      expectError: Boolean
  )(implicit ec: ExecutionContext): Future[Unit] = {
    generateBspConfig(workspace)
    for {
      _ <- server.initialize()
      _ <- server.initialized()
      // choose sbt as the Bsp Server
      _ = client.selectBspServer = { _ => new MessageActionItem("sbt") }
      _ <- server.executeCommand(ServerCommands.BspSwitch.id)
    } yield {
      if (!expectError) {
        server.assertBuildServerConnection()
      }
    }
  }

  private def generateBspConfig(workspace: AbsolutePath): Unit = {
    val bspFolder = workspace.resolve(".bsp")
    val sbtJson = bspFolder.resolve("sbt.json")
    // don't overwrite existing BSP config
    if (!sbtJson.isFile) {
      // we create bsp/sbt.json file manually because `sbt bspConfig` takes too long
      val sbtLaunchJar =
        BuildTool.copyFromResource(bspFolder.toNIO, "sbt-launch.jar")
      val argv = List(
        s"${Properties.javaHome}/bin/java",
        "-Xms100m",
        "-Xmx100m",
        "-classpath",
        sbtLaunchJar.toString,
        "xsbt.boot.Boot",
        "-bsp",
        s"--sbt-launch-jar=$sbtLaunchJar"
      )
      val connectionDetails = new BspConnectionDetails(
        "sbt",
        argv.asJava,
        "1.5.5",
        "2.0.0-M5",
        List("scala").asJava
      )
      val gson = new Gson()
      sbtJson.writeText(gson.toJson(connectionDetails))
    }
  }
}

/**
 * Set up your workspace using Scala CLI as your build tool.
 * This generates the .bsp/scala-cli.json file and invoke the BSP switch command
 * with scala-cli as the build server.
 */
object ScalaCliBuildInitializer extends BuildServerInitializer {
  this: BaseLspSuite =>
  override def initialize(
      workspace: AbsolutePath,
      server: TestingServer,
      client: TestingClient,
      layout: String,
      expectError: Boolean
  )(implicit ec: ExecutionContext): Future[Unit] = {
    generateBspConfig(workspace)
    for {
      _ <- server.initialize()
      _ <- server.initialized()
      // choose scala-cli as the Bsp Server
      _ = client.selectBspServer = { _ => new MessageActionItem("scala-cli") }
      _ <- server.executeCommand(ServerCommands.BspSwitch.id)
    } yield {
      if (!expectError) {
        server.assertBuildServerConnection()
      }
    }
  }

  private def scalaCliLauncher(): File = {

    // coursier/coursier#2188 should allow to get rid of the unzip / gunzip stuff here.
    // And the next release of Scala CLI should be published to Maven Central, so we could
    // also just fetch its class path, and run it with Java.
    def fetch(url: String, launcherPrefix: String): File = {

      val cache = FileCache()

      val f = cache.logger.use {
        cache
          .file(Artifact(url))
          .run
          .flatMap {
            case Left(e) => Task.fail(e)
            case Right(f) => Task.point(f)
          }
          .unsafeRun()(cache.ec)
      }

      val launcher =
        // FIXME Once coursier has proper support for extracted archives in cache, use it instead of those hacks
        if (f.getName.endsWith(".zip")) {
          val baseDir = f.getParentFile
          val dir =
            new File(baseDir, s".${f.getName.stripSuffix(".zip")}-content")
          if (!dir.exists()) {
            val tmpDir = new File(
              baseDir,
              s".${f.getName.stripSuffix(".zip")}-content-${UUID.randomUUID()}"
            )
            try {
              import scala.collection.JavaConverters._
              var zf: ZipFile = null
              try {
                zf = new ZipFile(f)
                for {
                  ent <- zf.entries().asScala
                  if ent.getName.startsWith(launcherPrefix)
                } {
                  val dest = new File(tmpDir, ent.getName)
                  var is: InputStream = null
                  var fos: FileOutputStream = null
                  try {
                    is = zf.getInputStream(ent)
                    fos = new FileOutputStream(dest)
                    val buf = Array.ofDim[Byte](16 * 1024)
                    var read = 0
                    while ({
                      read = is.read(buf)
                      read >= 0
                    }) {
                      if (read > 0)
                        fos.write(buf, 0, read)
                    }
                  } finally {
                    if (is != null)
                      is.close()
                    if (fos != null)
                      fos.close()
                  }
                }
              } finally {
                if (zf != null)
                  zf.close()
              }
              if (!dir.exists()) {
                try Files.move(tmpDir.toPath, dir.toPath) //, atomicMove = true)
                catch {
                  case ex: IOException =>
                    if (!dir.exists())
                      throw new Exception(ex)
                }
              }
            } finally {
              try {
                // os.remove.all(tmpDir)
              } catch {
                case _: IOException if Properties.isWin =>
              }
            }
          }

          val dirContent = dir.listFiles()
          if (dirContent.length == 1) dirContent.head
          else dirContent.filter(_.getName.startsWith(launcherPrefix)).head
        } else if (f.getName.endsWith(".gz")) {
          val dest =
            new File(f.getParentFile, s".${f.getName.stripSuffix(".gz")}")
          if (!dest.exists()) {
            var fis: FileInputStream = null
            var fos: FileOutputStream = null
            var gzis: GZIPInputStream = null
            try {
              fis = new FileInputStream(f)
              gzis = new GZIPInputStream(fis)
              fos = new FileOutputStream(dest)

              val buf = Array.ofDim[Byte](16 * 1024)
              var read = -1
              while ({
                read = gzis.read(buf)
                read >= 0
              }) {
                if (read > 0)
                  fos.write(buf, 0, read)
              }
              fos.flush()
            } finally {
              if (gzis != null) gzis.close()
              if (fos != null) fos.close()
              if (fis != null) fis.close()
            }
          }
          dest
        } else
          f

      if (!Properties.isWin) {
        import java.nio.file.attribute.PosixFilePermission._
        Files.setPosixFilePermissions(
          launcher.toPath,
          Set(
            OWNER_READ,
            OWNER_WRITE,
            OWNER_EXECUTE,
            GROUP_READ,
            GROUP_EXECUTE,
            OTHERS_READ,
            OTHERS_EXECUTE
          ).asJava
        )
      }

      launcher
    }

    val platformSuffix = {
      val arch = sys.props("os.arch").toLowerCase(Locale.ROOT) match {
        case "amd64" => "x86_64"
        case other => other
      }
      val os =
        if (Properties.isWin) "pc-win32"
        else if (Properties.isLinux) "pc-linux"
        else if (Properties.isMac) "apple-darwin"
        else sys.error(s"Unrecognized OS: ${sys.props("os.name")}")
      s"$arch-$os"
    }

    val extension = if (Properties.isWin) ".zip" else ".gz"

    fetch(
      s"https://github.com/VirtuslabRnD/scala-cli/releases/download/v0.0.1/scala-cli-$platformSuffix$extension",
      "scala-cli"
    )
  }

  private def generateBspConfig(workspace: AbsolutePath): Unit = {
    val bspJson = workspace.resolve(".bsp/scala-cli.json")
    if (!bspJson.isFile) {
      val scalaCli = scalaCliLauncher()
      val retCode =
        new ProcessBuilder(scalaCli.getAbsolutePath, "setup-ide", ".")
          .directory(workspace.toFile)
          .inheritIO()
          .start()
          .waitFor()
      if (retCode != 0)
        sys.error(
          s"Command 'scala-cli setup-ide .' failed (return code: $retCode)."
        )
    }
  }
}
