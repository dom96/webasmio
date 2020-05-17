# Part of webasmio - WebAssembly JS wrapper.

type
  WebAssemblyModule* = ref object
  WebAssemblyInstance* = ref object
  # TODO: Implement ability to pass user-defined value types between functions.
  # TODO: Allocate them on a virtual stack that we set up in WASM's memory.
  WebAssemblyMemory* = ref object

