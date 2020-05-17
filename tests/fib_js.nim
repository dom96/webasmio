# nim js -d:release --out:.\tests\fib_js.js .\tests\fib_js

proc fib_js(n: int32): int32 {.exportc.} =
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