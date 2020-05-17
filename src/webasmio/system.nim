import gen

type
  int32* {.magic: "Int32".}   ## Signed 32 bit integer type.
  int64* {.magic: "Int64".}
  float32* {.magic: Float32.} ## 32 bit floating point type.
  float64* {.magic: Float.}   ## 64 bit floating point type.

# TODO: Get rid of the repetition in this code.

proc `+`*(lhs, rhs: int32): int32 {.wasm.} =
  {.emit: """
    (i32.add (local.get $lhs) (local.get $rhs))
  """.}

proc `-`*(lhs, rhs: int32): int32 {.wasm.} =
  {.emit: """
    (i32.sub (local.get $lhs) (local.get $rhs))
  """.}

proc `*`*(lhs, rhs: int32): int32 {.wasm.} =
  {.emit: """
    (i32.mul (local.get $lhs) (local.get $rhs))
  """.}

proc `+`*(lhs, rhs: int64): int64 {.wasm.} =
  {.emit: """
    (i64.add (local.get $lhs) (local.get $rhs))
  """.}

proc `-`*(lhs, rhs: int64): int64 {.wasm.} =
  {.emit: """
    (i64.sub (local.get $lhs) (local.get $rhs))
  """.}

proc `*`*(lhs, rhs: int64): int64 {.wasm.} =
  {.emit: """
    (i64.mul (local.get $lhs) (local.get $rhs))
  """.}

proc `+`*(lhs, rhs: float64): float64 {.wasm.} =
  {.emit: """
    (f64.add (local.get $lhs) (local.get $rhs))
  """.}

proc `-`*(lhs, rhs: float64): float64 {.wasm.} =
  {.emit: """
    (f64.sub (local.get $lhs) (local.get $rhs))
  """.}

proc `*`*(lhs, rhs: float64): float64 {.wasm.} =
  {.emit: """
    (f64.mul (local.get $lhs) (local.get $rhs))
  """.}

proc `+`*(lhs, rhs: float32): float32 {.wasm.} =
  {.emit: """
    (f32.add (local.get $lhs) (local.get $rhs))
  """.}

proc `-`*(lhs, rhs: float32): float32 {.wasm.} =
  {.emit: """
    (f32.sub (local.get $lhs) (local.get $rhs))
  """.}

proc `*`*(lhs, rhs: float32): float32 {.wasm.} =
  {.emit: """
    (f32.mul (local.get $lhs) (local.get $rhs))
  """.}