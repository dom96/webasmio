# nim js --out:.\tests\test1.js .\tests\test_vars

from system import nil
import webasmio

proc test(lhs, rhs: int32): int32 {.wasm, exportwasm.} =
  var
    x = 52'i32
    z = 12345678'i32
  return x + z

webasmio.compileDefinedFunctions()