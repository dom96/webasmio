# when not defined(js):
#   {.error: "Webasmio is only support via the JS backend.".}

import macros, options, sequtils, sugar, strutils

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
  WatNode = ref object
    case kind: WatKind
    of Drop, Module: discard
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
    of Emit:
      wat: string
    children: seq[WatNode]

proc toWAT(node: WatNode, result: var string) =
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
    for param in node.params:
      toWAT(param, result)
      result.add " "
    if node.result.isSome():
      result.add "(result "
      result.add $node.result.get()
      result.add ")"
  of Emit:
    result.add node.wat

  for child in node.children:
    toWAT(child, result)

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

proc toValueType(ident: NimNode): ValueType =
  case ident.strVal.normalize()
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



proc processBody(node: NimNode): seq[WatNode] =
  case node.kind
  of nnkStmtList:
    for child in node.children:
      result.add processBody(child)
  of nnkPragma:
    assert node[0].kind == nnkExprColonExpr and node[0][0].strVal == "emit"
    result.add(
      WatNode(
        kind: Emit,
        wat: node[0][1].strVal
      )
    )
  else:
    assert false

macro wasm*(node: untyped): untyped =
  if node.kind != nnkProcDef:
    error("{.wasm.} can only be applied to procedures.")
  echo treeRepr(node)

  let name = $node.name
  let exported = getExportWasm(node.pragma, name)
  let (params, retType) = processParams(node.params)
  let children = processBody(node.body)
  var watNode = WatNode(
    kind: Func,
    funcId: some(name),
    `export`: exported,
    params: params,
    result: retType,
    children: children,
  )

  var text = ""
  toWAT(watNode, text)
  echo(text)