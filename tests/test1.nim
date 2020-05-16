from system import nil
import webasmio

proc add(lhs, rhs: int32): int32 {.wasm, exportwasm.} =
  lhs + rhs # Infix test case

webasmio.compileDefinedFunctions()