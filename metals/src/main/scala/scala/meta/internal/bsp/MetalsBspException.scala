package scala.meta.internal.metals

class MetalsBspException(
    tryingToGet: String,
    cause: Throwable
) extends Exception(
      s"BSP connection failed in the attempt to get: $tryingToGet.  ${cause.getMessage}",
      cause
    )
