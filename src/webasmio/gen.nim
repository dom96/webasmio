
import macros, options, sequtils, sugar, strutils, strformat
import macrocache

import webassembly

type
  # https://www.w3.org/TR/wasm-core-1/#text-id
  Indice = string

  ValueType {.pure.} = enum
    i32, i64, f32, f64

  WatKind {.pure.} = enum
    Module,
    # https://www.w3.org/TR/wasm-core-1/#functions%E2%91%A7
    Func, # useful search: "(func $add (export "add")"
    Export,
    Param,
    Drop,
    Emit,
    Call,
    Block,
    Loop,
    Br,
    Br_if,
    Return
  WatNode = ref object
    case kind: WatKind
    of Drop, Module, Loop, Return: discard
    of Export:
      name: string
    of Param:
      paramId: Option[Indice]
      paramType: ValueType
    of Func:
      funcId: Option[Indice]
      `export`: Option[WatNode]
      params: seq[WatNode]
      result: Option[ValueType]
      locals: seq[WatNode]
    of Emit:
      wat: string
    of Call:
      callId: Indice
    of Block:
      blockId: Option[Indice]
    of Br:
      brId: Indice
    of Br_if:
      brIfId: Indice
    children: seq[WatNode]

proc toWAT(node: WatNode, result: var string, indent = 0, newline=false) =
  result.add repeat(" ", indent)
  case node.kind
  of Drop:
    result.add "(drop "
  of Module:
    result.add "(module "
  of Export:
    result.add "(export "
    result.add "\""
    result.add node.name
    result.add "\""
  of Param:
    result.add "(param "
    if node.paramId.isSome():
      result.add "$"
      result.add $node.paramId.get()
      result.add " "
    result.add $node.paramType
  of Func:
    result.add "(func "
    if node.funcId.isSome():
      result.add "$"
      result.add $node.funcId.get()
      result.add " "
    if node.`export`.isSome():
      toWAT(node.`export`.get(), result)
      result.add " "
    for param in node.params:
      toWAT(param, result)
      result.add " "
    if node.result.isSome():
      result.add "(result "
      result.add $node.result.get()
      result.add ")"
    for local in node.locals:
      toWAT(local, result, indent + 2, newline=true)
  of Emit:
    for line in node.wat.splitLines:
      result.add repeat(" ", indent)
      result.add line.strip()
      result.add("\n")
  of Call:
    result.add "(call "
    result.add "$"
    result.add node.callId
  of Block:
    result.add "(block "
    if node.blockId.isSome():
      result.add "$"
      result.add node.blockId.get()
  of Loop:
    result.add "(loop "
  of Br_if:
    result.add "(br_if "
  of Br:
    result.add "(br "
  of Return:
    result.add "(return "

  if newline and node.kind notin {Emit}:
    result.add("\n")
  for child in node.children:
    toWAT(child, result, indent + 2, newline=true)

  if node.kind notin {Emit}:
    result.add ")"

proc getExportWasm(pragma: NimNode, procName: string): Option[WatNode] =
  for child in pragma:
    if child.kind == nnkExprColonExpr:
      if child[0].strVal.normalize() == "exportwasm":
        return some(WatNode(kind: Export, name: child[1].strVal))
    else:
      if child.strVal.normalize() == "exportwasm":
        return some(WatNode(kind: Export, name: procName))

proc toValueType(node: NimNode): ValueType =
  if node.kind == nnkIdent:
    case node.strVal.normalize()
    of "int32":
      ValueType.i32
    of "int64":
      ValueType.i64
    of "float64":
      ValueType.f64
    of "float32":
      ValueType.f32
    else:
      assert false; ValueType.i32
  else:
    case node.kind
    of nnkInt32Lit:
      ValueType.i32
    of nnkIntLit, nnkInt64Lit:
      ValueType.i64
    of nnkFloatLit, nnkFloat64Lit:
      ValueType.f64
    of nnkFloat32Lit:
      ValueType.f32
    of nnkInt8Lit, nnkInt16Lit, nnkUIntLit, nnkUInt8Lit, nnkUInt16Lit,
      nnkUInt32Lit, nnkUInt64Lit, nnkFloat128Lit:
      error("These integer types are not supported by WASM."); ValueType.i32
    else:
      assert false; ValueType.i32

proc toValueType(typeKind: NimTypeKind): ValueType =
  case typeKind
  of ntyInt32:
    ValueType.i32
  of ntyInt, ntyInt64:
    ValueType.i64
  of ntyFloat, ntyFloat64:
    ValueType.f64
  of ntyFloat32:
    ValueType.f32
  of ntyInt8, ntyInt16, ntyFloat128, ntyUInt, ntyUInt8,
     ntyUInt16, ntyUInt32, ntyUInt64:
    error("These integer types are not supported by WASM."); ValueType.i32
  else:
    assert false; ValueType.i32

proc processParams(params: NimNode): (seq[WatNode], Option[ValueType]) =
  assert params.kind == nnkFormalParams

  # Return value
  let retType = params[0]
  assert retType.kind in {nnkIdent, nnkEmpty}
  if retType.kind != nnkEmpty:
    result[1] = some(toValueType(retType))

  # Parameters
  for i in 1 ..< params.len:
    let defs = params[i]
    assert defs.kind == nnkIdentDefs
    let idents = defs[0 .. ^3]
    let identType = defs[^2]
    for ident in idents:
      result[0].add(
        WatNode(
          kind: Param,
          paramId: some(ident.strVal),
          paramType: toValueType(identType)
        )
      )

proc mangleName(name: string): string =
  # https://www.w3.org/TR/wasm-core-1/#text-id
  for c in name:
    if c in {' ', '\'', '"', ',', ';', '[', ']', '{', '}'}:
      result.add($c.int)
    else:
      result.add(c)

proc initLocalsGet(varName: string): WatNode =
  WatNode(
    kind: Emit,
    wat: "(local.get $" & mangleName(varName) & ")"
  )

proc initLocalsSet(varName: string): WatNode =
  WatNode(
    kind: Emit,
    wat: "(local.set $" & mangleName(varName) & ")"
  )

proc initLocal(identDefs: NimNode): WatNode =
  assert identDefs.kind == nnkIdentDefs
  var valueType: ValueType
  if identDefs[1].kind != nnkEmpty:
    valueType = toValueType(identDefs[1])
  else:
    valueType = toValueType(identDefs[2])
  return WatNode(
    kind: Emit,
    wat: fmt"(local ${mangleName(identDefs[0].strVal)} {$valueType})"
  )

proc processBody(node: NimNode, locals: var seq[WatNode]): seq[WatNode] =
  case node.kind
  of nnkIdent:
    result.add(initLocalsGet(node.strVal))
  of nnkStmtList:
    for child in node.children:
      result.add processBody(child, locals)
  of nnkPragma:
    assert node[0].kind == nnkExprColonExpr and node[0][0].strVal == "emit"
    result.add(
      WatNode(
        kind: Emit,
        wat: node[0][1].strVal
      )
    )
  of nnkInfix, nnkCall:
    let name = mangleName(node[0].strVal)
    for i in 1 ..< node.len:
      result.add(processBody(node[i], locals))
    result.add(
      WatNode(
        kind: Call,
        callId: name
      )
    )
  of nnkVarSection:
    for identDefs in node:
      assert identDefs.kind == nnkIdentDefs
      locals.add(initLocal(identDefs))
      result.add processBody(identDefs[2], locals)
      result.add initLocalsSet(identDefs[0].strVal)
  of nnkIntLit, nnkInt64Lit:
    result.add(
      WatNode(
        kind: Emit,
        wat: "(i64.const " & $node.intVal & ")"
      )
    )
  of nnkInt32Lit:
    result.add(
      WatNode(
        kind: Emit,
        wat: "(i32.const " & $node.intVal & ")"
      )
    )
  of nnkFloatLit:
    result.add(
      WatNode(
        kind: Emit,
        wat: "(f64.const " & $node.floatVal & ")"
      )
    )
  of nnkReturnStmt:
    var retNode = WatNode(kind: Return)
    retNode.children.add processBody(node[0], locals)
    result.add(retNode)
  # of nnkForStmt:
  #   assert node[0].kind == nnkIdent
  #   let iterVarName = node[0].strVal
  #   if node[1].kind == nnkInfix:
  #     case node[1][0].strVal
  #     of "..<":
  #       # Init start of loop variable.
  #       result.add(processBody(node[1][1]))
  #       result.add initLocalsSet(iterVarName)
  else:
    assert false, $node.kind

proc generateJsWasmCall(name: string, params: NimNode): NimNode =
  assert params.kind == nnkFormalParams
  var javascript = ""
  javascript.add fmt"""
    `result` = gWebasmioInstance.exports.{name}(
  """
  # TODO: Refactor this code, similar one is used above.
  for i in 1 ..< params.len:
    let defs = params[i]
    assert defs.kind == nnkIdentDefs
    let idents = defs[0 .. ^3]
    for ident in idents:
      javascript.add "`"
      javascript.add ident.strVal
      javascript.add "`,"
      javascript.add "\n"

  javascript.add(");")
  return quote do:
    {.emit: `javascript`.}

const definedFunctions = CacheSeq"webasmio.funcs"
var gWebasmioInstance {.exportc.}: WebAssemblyInstance
macro wasm*(node: untyped): untyped =
  if node.kind != nnkProcDef:
    error("{.wasm.} can only be applied to procedures.")

  definedFunctions.add(node.copyNimTree)

  let name = $node.name
  let exported = getExportWasm(node.pragma, name)

  # If this has been exported then we create a JS stub.
  if exported.isSome():
    result = node
    result.body = generateJsWasmCall(name, node.params)
    let pragmas = result.pragma
    result.pragma = newEmptyNode() # TODO: Remove exportwasm only.
    for pragma in pragmas:
      if pragma.kind == nnkIdent and pragma.strVal != "exportwasm":
        result.pragma.add(pragma)
    result.addPragma(
      newIdentNode("exportc")
    )
    hint("Webasmio: Generated stub " & name)
  else:
    result = newEmptyNode()
  echo(result.toStrLit)

macro compileDefinedFunctions*(): untyped =
  var watModule = WatNode(
    kind: Module,
  )
  for node in definedFunctions:
    let name = $node.name
    hint("Webasmio: Compiling " & name)
    echo treeRepr(node)
    let exported = getExportWasm(node.pragma, name)
    let (params, retType) = processParams(node.params)
    var locals: seq[WatNode]
    let children = processBody(node.body, locals)
    var watNode = WatNode(
      kind: Func,
      funcId: some(name),
      `export`: exported,
      params: params,
      result: retType,
      locals: locals,
      children: children,
    )

    watModule.children.add(watNode)
  var text = ""
  toWAT(watModule, text, newline=true)
  echo(text)

  let jsToEmit = """
    // Create a module from a WebAssembly Text format: https://stackoverflow.com/a/60921153/492186
    var myModule = WabtModule().parseWat("nim.wat", ``
      $#
    ``, {});

    // Emit module in a binary format
    var wasmData = myModule.toBinary({}).buffer;

    // Use WebAssembly API to instantiate a compiled module
    var compiled = new WebAssembly.Module(wasmData);
    gWebasmioInstance = new WebAssembly.Instance(compiled, {});
  """ % [text]
  let strNode = newNimNode(nnkTripleStrLit)
  strNode.strVal = jsToEmit
  result = newTree(
    nnkPragma,
    newTree(
      nnkExprColonExpr,
      newIdentNode("emit"),
      strNode
    )
  )
  echo(result.toStrLit)