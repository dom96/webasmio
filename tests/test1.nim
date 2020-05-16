from system import nil
import webasmio

proc addEx(lhs, rhs: int32): int32 {.wasm, exportwasm.} =
  lhs + rhs # Infix test case

proc add(lhs, rhs: int32): int32 {.wasm, exportwasm.} =
  addEx(lhs, rhs) # Normal call

webasmio.compileDefinedFunctions()