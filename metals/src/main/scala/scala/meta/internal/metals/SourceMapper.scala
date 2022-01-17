package scala.meta.internal.metals

import scala.meta.inputs.Input
import scala.meta.internal.builds.SbtBuildTool
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.worksheets.WorksheetProvider
import scala.meta.io.AbsolutePath

import org.eclipse.{lsp4j => l}

final case class SourceMapper(
    buildTargets: BuildTargets,
    buffers: Buffers,
    workspace: () => AbsolutePath
) {

  def actualSource(path: AbsolutePath): AbsolutePath =
    buildTargets.actualSource(path).map(_.path).getOrElse(path)

  def actualSourceForPc(
      path: AbsolutePath,
      scalaVersion: String
  ): (Input.VirtualFile, l.Position => l.Position, AdjustLspData) = {

    def input = path.toInputFromBuffers(buffers)
    def default = {
      val viaBuildTargets =
        buildTargets.actualSource(path).map(_.update(input.value))
      viaBuildTargets.getOrElse(
        (input, identity[l.Position] _, AdjustedLspData.default)
      )
    }

    val forScripts =
      if (path.isSbt) {
        buildTargets
          .sbtAutoImports(path)
          .map(
            SbtBuildTool.sbtInputPosAdjustment(input, _)
          )
      } else if (
        path.isWorksheet && ScalaVersions.isScala3Version(scalaVersion)
      ) {
        WorksheetProvider.worksheetScala3Adjustments(
          input,
          path.toURI.toASCIIString
        )
      } else None

    forScripts.getOrElse(default)
  }
}
