# nim js --out:.\tests\fib.js .\tests\fib

from system import nil
import webasmio

proc fib_webasm(n: int32): int32 {.wasm, exportwasm.} =
  var
    a = 0'i32
    b = 1'i32
    i = 0'i32
  while i < n:
    let t = a + b
    a = b
    b = t
    i = i + 1

  return b

proc fib_bench(count, n: int32) {.wasm, exportwasm.} =
  var i = 0'i32
  while i < count:
    let _ = fib_webasm(n)
    i = i + 1

webasmio.compileDefinedFunctions()