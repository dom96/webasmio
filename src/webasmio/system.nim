import gen

type
  int32* {.magic: "Int32".}   ## Signed 32 bit integer type.
  int64* {.magic: "Int64".}
  float32* {.magic: Float32.} ## 32 bit floating point type.
  float64* {.magic: Float.}   ## 64 bit floating point type.

# TODO: Get rid of the repetition in this code.

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

proc `+`*(lhs, rhs: int64): int64 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i64.add)
  """.}

proc `-`*(lhs, rhs: int64): int64 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i64.sub)
  """.}

proc `*`*(lhs, rhs: int64): int64 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (i64.mul)
  """.}

proc `+`*(lhs, rhs: float64): float64 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (f64.add)
  """.}

proc `-`*(lhs, rhs: float64): float64 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (f64.sub)
  """.}

proc `*`*(lhs, rhs: float64): float64 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (f64.mul)
  """.}

proc `+`*(lhs, rhs: float32): float32 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (f32.add)
  """.}

proc `-`*(lhs, rhs: float32): float32 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (f32.sub)
  """.}

proc `*`*(lhs, rhs: float32): float32 {.wasm.} =
  {.emit: """
    (local.get $lhs)
    (local.get $rhs)
    (f32.mul)
  """.}