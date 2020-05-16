import gen

type
  int32* {.magic: "Int32".}   ## Signed 32 bit integer type.
  int64* {.magic: "Int64".}
  float32* {.magic: Float32.} ## 32 bit floating point type.
  float64* {.magic: Float.}   ## 64 bit floating point type.

proc `+`*(lhs, rhs: int32): int32 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i32.add)
  """.}

proc `-`*(lhs, rhs: int32): int32 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i32.sub)
  """.}

proc `*`*(lhs, rhs: int32): int32 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i32.mul)
  """.}