package scala.meta.internal.metals

import java.util.concurrent.CompletableFuture

import scala.build.bsp.ScalaScriptBuildServer

import ch.epfl.scala.bsp4j.DebugSessionParams
import ch.epfl.scala.{bsp4j => b}
import org.eclipse.lsp4j.jsonrpc.services.JsonRequest

trait MetalsBuildServer
    extends b.BuildServer
    with b.ScalaBuildServer
    with b.JavaBuildServer
    with ScalaScriptBuildServer {
  @JsonRequest("debugSession/start")
  def startDebugSession(
      params: DebugSessionParams
  ): CompletableFuture[b.DebugSessionAddress]
}
