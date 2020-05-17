# nim js --out:.\tests\test1.js .\tests\test_for

from system import nil
import webasmio

proc test(lhs, rhs: int32): float32 {.wasm, exportwasm.} =
  var i = 0'i32
  while i < 10'i32:
    result = result + 5
    i = i + 1

webasmio.compileDefinedFunctions()