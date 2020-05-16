import webasmio

proc add(lhs, rhs: int32): int32 {.wasm, exportwasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i32.add)
  """.}

webasmio.compileDefinedFunctions()