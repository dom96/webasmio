from system import bool
import gen

type
  int32* {.magic: "Int32".}   ## Signed 32 bit integer type.
  int64* {.magic: "Int64".}
  float32* {.magic: Float32.} ## 32 bit floating point type.
  float64* {.magic: Float.}   ## 64 bit floating point type.

# TODO: Get rid of the repetition in this code.

# Reference: https://webassembly.github.io/spec/core/appendix/index-instructions.html

# Functions dealing with numbers.

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

# Conditions

proc `<`*(lhs, rhs: int32): bool {.wasm.} =
  {.emit: """
    (i32.lt_s (local.get $lhs) (local.get $rhs))
  """.}

proc `>`*(lhs, rhs: int32): bool {.wasm.} =
  {.emit: """
    (i32.gt_s (local.get $lhs) (local.get $rhs))
  """.}

proc `<=`*(lhs, rhs: int32): bool {.wasm.} =
  {.emit: """
    (i32.le_s (local.get $lhs) (local.get $rhs))
  """.}

proc `>=`*(lhs, rhs: int32): bool {.wasm.} =
  {.emit: """
    (i32.ge_s (local.get $lhs) (local.get $rhs))
  """.}

proc `==`*(lhs, rhs: int32): bool {.wasm.} =
  {.emit: """
    (i32.eq (local.get $lhs) (local.get $rhs))
  """.}

proc `!=`*(lhs, rhs: int32): bool {.wasm.} =
  {.emit: """
    (i32.ne (local.get $lhs) (local.get $rhs))
  """.}

proc `<`*(lhs, rhs: int64): bool {.wasm.} =
  {.emit: """
    (i64.lt_s (local.get $lhs) (local.get $rhs))
  """.}

# Iterators.
iterator `..<`*(a, b: int64): int64 {.inline, wasm.} =
  var i = a
  while i < b:
    yield i
    i = sys.`+`(i, 1)