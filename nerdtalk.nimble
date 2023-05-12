# Package

version       = "0.1.0"
author        = "Mark Leyva"
description   = "XML-RPC library"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.10"

task docs,"Generate documentation":
    exec "nim doc --project --index:on --outdir:htmldocs src/nerdtalk.nim"
