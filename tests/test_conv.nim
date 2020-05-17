# nim js --out:.\tests\test1.js .\tests\test_vars

from system import nil
import webasmio

proc test(lhs, rhs: int32): float32 {.wasm, exportwasm.} =
  var
    x = 52'i32
    z = 12_345'i64
  return float32(x + z) + 1_000.5

webasmio.compileDefinedFunctions()