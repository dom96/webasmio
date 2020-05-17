# nim js --out:.\tests\test1.js .\tests\test_vars

from system import nil
import webasmio

proc test(lhs, rhs: int32): int32 {.wasm, exportwasm.} =
  var
    x = 52'i32
    y = 1.42
    z = 123'i32#45678'i64
  return x + z# + y

webasmio.compileDefinedFunctions()