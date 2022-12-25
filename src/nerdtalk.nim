import std/macros
import std/strutils
import std/times
import std/xmlparser
import std/xmltree
import std/sequtils
import std/typetraits
from std/os import fileExists
from std/math import almostEqual
import std/base64

# -- XML-RPC --

## `XML-RPC<http://xmlrpc.com/spec.md>`_ implementation in pure Nim.
##
## Basic usage::
##  type
##    Acccount = object
##      id: int
##      name: string
##      birthday: DateTime
##  xmlRpcSpec:
##    name: "update_account"
##    params:
##      account: Account
##  update_account(Account(1001, "John", now()))

type
  XmlRpcDecodingException* = object of ValueError ## \
  ## Is thrown when an XML-RPC method response cannot be decoded
  XmlRpcItemKind* = enum
    xmlRpcInteger,
    xmlRpcBoolean,
    xmlRpcString,
    xmlRpcFloat,
    xmlRpcDateTime
    xmlRpcBase64,
    xmlRpcStruct,
    xmlRpcArray
  XmlRpcResponseKind* = enum ## \
  ## Discrimantor kind for XML-RPC responses
  ## 
  ## According to the XML-RPC spec, an XML-RPC response
  ## is either a response with a sole `<params>` node or
  ## a fault with a sole `<struct>` node with exactly 2
  ## members.
  ##
    methodResponse, fault
  XmlRpcResponse* {.final.} = object
    case k*: XmlRpcResponseKind
      of methodResponse:
        response*: XmlRpcType
      of fault:
        code*: int
        str*: string
  XmlRpcType* {.acyclic, final.} = object
    ## Base type for all XML-RPC types
    ##
    case k*: XmlRpcItemKind
      of xmlRpcInteger:
        fInt*: BiggestInt
      of xmlRpcBoolean:
        fBool*: bool
      of xmlRpcString, xmlRpcBase64:
        fString*: string
      of xmlRpcFloat:
        fFloat*: BiggestFloat
      of xmlRpcDateTime:
        fDateTime*: DateTime
      of xmlRpcArray:
        fArray*: seq[XmlRpcType]
      of xmlRpcStruct:
        fStruct*: seq[(string, XmlRpcType)]

proc `[]`*(this: XmlRpcType, key: string): XmlRpcType {.raises: [KeyError].} =
  ## Access the member `key` in `this`
  ##
  ## **Note**: Only works on `xmlRpcStruct`s
  ##
  ## Raises a `KeyError` if `key` does not exist, else
  ## returns the `XmlRcpType` associated with `key`
  ##
  doAssert this.k == xmlRpcStruct, "`this` is not an xmlRpcStruct"
  for element in this.fStruct:
    let name = element[0]
    if key == name:
      return element[1]
  raise newException(KeyError, key & " does not exist")

proc `~=`*(lhs, rhs: XmlRpcType): bool {.inline.} =
  ## Checks if `lhs` and `rhs` are the same kind of
  ## `XmlRpcItemKind`
  ##
  return lhs.k == rhs.k

proc `==`*(lhs, rhs: XmlRpcType): bool =
  ## Checks if `lhs` is field equivalent with
  ## `rhs`
  ##
  if lhs ~= rhs:
    case lhs.k:
      of xmlRpcInteger:
        return lhs.fInt == rhs.fInt
      of xmlRpcBoolean:
        return lhs.fBool == rhs.fBool
      of xmlRpcString, xmlRpcBase64:
        return lhs.fString == rhs.fString
      of xmlRpcFloat:
        # Ensure we are approximately equal to the other
        return almostEqual(lhs.fFloat, rhs.fFloat)
      of xmlRpcDateTime:
        return lhs.fDateTime == rhs.fDateTime
      of xmlRpcArray:
        for element in zip(lhs.fArray, rhs.fArray):
          if not (element[0] == element[1]):
            return false
        return true
      of xmlRpcStruct:
        for element in zip(lhs.fStruct, rhs.fStruct):
          let first = element[0]
          let second = element[1]
          # Check member names
          if not (first[0] == second[0]):
            return false
          # Check member values
          if not (first[1] == second[1]):
            return false
        return true
  return false

proc `==`*(lhs, rhs: XmlRpcResponse): bool =
  ## Checks if `lhs` is field equivalent to `rhs`
  ##
  if lhs.k == rhs.k:
    case lhs.k:
      of methodResponse:
        return lhs.response == rhs.response
      of fault:
        return (lhs.code == rhs.code) and (lhs.str == rhs.str)
  return false

template xrarray*() {.pragma.} ## \
## Used to discriminate between XML-RPC structs
## and XML-RPC arrays. The presence of this pragma
## on an object type causes the `to` macro to
## generate an XML-RPC array.
## 
## Example::
##  type
##    Persons {.xrarray.} = object
##      p: seq[string]
## 
## **Note**: The same warning applies for objects inheriting
## from `RootObj`.
## 
## **Note**: `seq[T]` and `arr[T]` types also map to
## `<array>` where very element in the array is of type `T` which
## differs from the case where every element in `<array>` could be
## of different types `A`, `B`, etc...
##

proc `?:`*[T](encode: T): XmlRpcType =
  ## Generic constructor for `XmlRpcType` of `Base64`.
  ##
  ## Note that `T` must have the proc `$` defined for it
  ##
  result = XmlRpcType(k: xmlRpcBase64, fString: encode($encode))

# Compile-time object helper types
type
  xKind = enum
    strKind, seqKind
  rType = enum
    structType, arrayType
  xType {.acyclic.} = object
    case k: xKind
      of strKind:
        s: string
      of seqKind:
        t: rType
        q: seq[(string, xType)]

proc isXmlRpcArray(t: NimNode): bool =
  ## Checks if a type definition has the {.xrarray.}
  ## pragma
  t.expectKind nnkTypeDef
  let q = t.findChild(it.kind == nnkPragmaExpr)
  if q.isNil():
    return false
  else:
    return q[1][0].strVal == "xrarray"

proc getMembers(recList: NimNode): seq[(string, xType)] =
  ## Get the name and type members of the NimNode
  result = newSeq[(string, xType)]()
  for item in recList:
    var name = ""
    for idx in 0 ..< item.len:
      let kn = item[idx]
      case kn.kind:
        of nnkSym:
          if idx mod 2 == 0:
            # Name
            name = kn.strVal
          else:
            # Member type
            var memberType = kn.getTypeImpl
            case memberType.typeKind:
              of ntyObject:
                for m in memberType:
                  case m.kind:
                    # Inheritance is not allowed
                    of nnkOfInherit:
                      error("Inheritance is not allowed. All lookups must be done at compile-time", m)
                    of nnkRecList:
                      # Call recursively on object types
                      if kn.getImpl.isXmlRpcArray():
                        # Object had {.xrarray.} pragma
                        # Construct XmlRpcArrayType
                        result.add((name, xType(k: seqKind, t: arrayType,
                            q: getMembers(m))))
                      else:
                        result.add((name, xType(k: seqKind, t: structType,
                            q: getMembers(m))))
                    else:
                      # nnkEmptyNodes
                      discard
              else:
                result.add((name, xType(k: strKind, s: kn.strVal)))
        of nnkBracketExpr:
          let memberType = kn.getTypeImpl
          #let arraySeqType = memberType[0]
          #let elementType = memberType[1]
          error("Sequences and arrays are currently not supported as member types", recList)
        of nnkEmpty:
          discard
        else:
          error("Unsupported member type", recList)

proc constructRpcType(members: seq[(string, xType)], objct: NimNode): seq[NimNode] =
  ## `members` is a sequence of tuples that contain
  ## the name of the member of `objct` and the respective `XmlRpcType`
  ## 
  result = newSeq[NimNode]()
  for t in members:
    let memberName = t[0]
    let memberType = t[1] # object variant
    var mem = newIdentNode(memberName)
    var obj = newTree(nnkObjConstr)
    obj.add(newIdentNode("XmlRpcType"))
    var typeNode, xmlRpcNodeKind: NimNode
    case memberType.k:
      of strKind:
        case memberType.s:
          of "string":
            typeNode = ident("fString")
            xmlRpcNodeKind = ident("xmlRpcString")
          of "float":
            typeNode = ident("fFloat")
            xmlRpcNodeKind = ident("xmlRpcFloat")
          of "int", "Natural":
            typeNode = ident("fInt")
            xmlRpcNodeKind = ident("xmlRpcInteger")
          of "bool":
            typeNode = ident("fBool")
            xmlRpcNodeKind = ident("xmlRpcBoolean")
          of "DateTime":
            typeNode = ident("fDateTime")
            xmlRpcNodeKind = ident("xmlRpcDateTime")
          of "XmlRpcType":
            error("Inclusion of XmlRpcTypes is not supported. Use non-XmlRpcTypes")
          else:
            error("Unspported type: " & memberType.s, objct)
        # Construct the proper XmlRpcType
        obj.add(newColonExpr(ident("k"), xmlRpcNodeKind))
        # `objct`.`mem`
        let value = newDotExpr(objct, mem)
        obj.add(newColonExpr(typeNode, value))
        result.add(obj)
      of seqKind:
        case memberType.t:
          of structType:
            typeNode = ident("fStruct")
            xmlRpcNodeKind = ident("xmlRpcStruct")
            obj.add(newColonExpr(ident("k"), xmlRpcNodeKind))
            let value = newDotExpr(objct, mem)
            # Recursively constuct the element types
            let nestedValues = constructRpcType(memberType.q, value)
            var tupleValues = newSeq[NimNode]()
            var mNames = newSeq[string]()
            for names in memberType.q:
              mNames.add(names[0])
            for v in zip(mNames, nestedValues):
              tupleValues.add(newTree(nnkTupleConstr, newLit(v[0]), v[1]))
            let actValue = newTree(nnkBracket, tupleValues)
            let children = newTree(nnkPrefix, ident("@"), actValue)
            obj.add(newColonExpr(typeNode, children))
            result.add(obj)
          of arrayType:
            typeNode = ident("fArray")
            xmlRpcNodeKind = ident("xmlRpcArray")
            obj.add(newColonExpr(ident("k"), xmlRpcNodeKind))
            let value = newDotExpr(objct, mem)
            let nestedValues = constructRpcType(memberType.q, value)
            let actValue = newTree(nnkBracket, nestedValues)
            let children = newTree(nnkPrefix, ident("@"), actValue)
            obj.add(newColonExpr(typeNode, children))
            result.add(obj)

proc xmlRpcObjectConstruction(body: NimNode): NimNode =
  ## XML-RPC struct type
  ## are defined as a tuple[string, XmlRpcType]
  ## The first item is the member name (i.e. the field name)
  ## The second item is the value which is itself an XmlRpcType
  let typeImpl = body.getTypeImpl
  let recList = typeImpl[2]

  # Get members (name,type)
  var members = getMembers(recList)

  # Construct a dot expression with the above member information
  var memberValues = constructRpcType(members, body)

  # Get all the names of the object members
  var names = newSeq[string]()
  for v in members:
    let memberName = v[0]
    names.add(memberName)

  # Map mamber names to member values
  var mems = zip(names, memberValues)

  # Finished XmlRpcStruct type
  result = quote do:
    let q = `mems`
    XmlRpcType(k: xmlRpcStruct, fStruct: @q)

proc xmlRpcArrayContsruction(body: NimNode): NimNode =
  let typeImpl = body.getTypeImpl
  let recList = typeImpl[2]

  # Get members (name,type)
  var members = getMembers(recList)

  # Construct a dot expression with the above member information
  var memberValues = constructRpcType(members, body)

  result = quote do:
    let q = `memberValues`
    XmlRpcType(k: xmlRpcArray, fArray: @q)

macro to*(body: typed): untyped =
  ## Construct `XmlRpcType`s from
  ## - `int`
  ## - `float`
  ## - `bool`
  ## - `string`
  ## - `char`
  ## - `seq[T]`
  ## - `array[T]`
  ## - `object`
  ##
  ## Usage::
  ##  let temp = to 2.0
  ##  let result = getMethodCall("set_temp", temp)
  ##
  ## **Warning**: The library is limited to `object`
  ## types that **do not** inhereit from `RootObj`.
  ##
  ## The reason for this is because `to` constructs
  ## an `XmlRpcType` by traversing the type implementation
  ## at compile time. All fields are treated as public, breaking
  ## basic OOP encapsulation.
  ##
  let node = body.getTypeImpl
  case node.typeKind:
    of ntyInt, ntyInt8, ntyInt16, ntyInt32, ntyInt64, ntyRange:
      result = quote do:
        XmlRpcType(k: xmlRpcInteger, fInt: `body`)
    of ntyFloat, ntyFloat32, ntyFloat64, ntyFloat128:
      result = quote do:
        XmlRpcType(k: xmlRpcFloat, fFloat: `body`)
    of ntyBool:
      result = quote do:
        XmlRpcType(k: xmlRpcBoolean, fBool: `body`)
    of ntyString, ntyChar, ntyCString:
      result = quote do:
        # non-string types must be converted to string type with `$`
        XmlRpcType(k: xmlRpcString, fString: $`body`)
    of ntySequence, ntyArray:
      result = quote do:
        var xRpcElements = newSeq[XmlRpcType]()
        for element in `body`:
          xRpcElements.add(to(element))
        XmlRpcType(k: xmlRpcArray, fArray: xRpcElements)
    of ntyObject:
      let ntype = node.getTypeInst()
      # DateTime objects map directly to XmlRpcType `DateTime`
      if ntype.strVal == "DateTime":
        result = quote do:
          XmlRpcType(k: xmlRpcDateTime, fDateTime: `body`)
      # Generalized object types
      else:
        case body.kind:
          of nnkObjConstr, nnkSym:
            let t = body.getTypeInst().getImpl()
            if t.isXmlRpcArray():
              # Construct XML-RPC array type
              result = xmlRpcArrayContsruction(body)
            else:
              # Construct XML-RPC struct type
              result = xmlRpcObjectConstruction(body)
          else:
            error("Invalid body", body)
    else:
      error("Unspported type", body)

proc to*[T](this: XmlRpcType, t: typedesc[T]): T =
  result

proc allDigits(str: string): bool =
  ## Return `true` if all `c` in `str`
  ## are in {'0'..'9'}
  for i in str:
    if not i.isDigit():
      return false
  return true

proc deserialize(value: XmlNode): XmlRpcType =
  ## Helper proc for deserialization of XML-RPC value type
  var xmlType = value.tag
  # Handle all <iN> cases by changing tag to <int>
  if xmlType[0].toLowerAscii() == 'i' and xmlType[1..^1].allDigits():
    xmlType = "int"
  var r: XmlRpcType
  case xmlType:
    of "int":
      try:
        r = to parseInt(value.innerText)
      except ValueError as e:
        raise newException(XmlRpcDecodingException, e.msg)
    of "double":
      try:
        r = to parseFloat(value.innerText)
      except ValueError as e:
        raise newException(XmlRpcDecodingException, e.msg)
    of "string":
      r = to value.innerText
    of "boolean":
      try:
        r = to parseBool(value.innerText)
      except ValueError as e:
        raise newException(XmlRpcDecodingException, e.msg)
    of "dateTime.iso8601":
      try:
        # Year-Month-Day Hour:minutes:seconds.milliseconds
        r = to parse(value.innerText, "YYYY-MM-dd HH:mm:ss:fff")
      except TimeParseError as e:
        raise newException(XmlRpcDecodingException, e.msg)
    of "base64":
      r = XmlRpcType(k: xmlRpcBase64, fString: value.innerText)
    of "struct":
      var elements = newSeq[(string, XmlRpcType)]()
      for child in value:
        let mName = child[0].innerText
        let mValue = child[1][0]
        elements.add((mName, deserialize(mValue)))
      r = XmlRpcType(k: xmlRpcStruct, fStruct: elements)
    of "array":
      var elements = newSeq[XmlRpcType]()
      for child in value[0]:
        let aValue = child[0]
        let aType = aValue
        elements.add(deserialize(aType))
      r = XmlRpcType(k: xmlRpcArray, fArray: elements)
    else:
      raise newException(XmlRpcDecodingException,
          "Received an invalid XMl-RPC response. <" & xmlType & "> is not a valid XML-RPC type.")
  return r

proc `:!`*(body: string): XmlRpcResponse {.raises: [XmlRpcDecodingException].} =
  ## Deserialization proc for XML-RPC responses.
  ##
  var response: XmlNode
  try:
    response = parseXml(body)
  except Exception as err:
    raise newException(XmlRpcDecodingException, err.msg)
  let faults = response.findAll("fault")
  let params = response.findAll("params")
  if faults.len == 1:
    let fault = faults[0]
    var struct: XmlNode
    try:
      struct = fault[0][0]
    except IndexDefect:
      raise newException(XmlRpcDecodingException, "Expected a <value> tag")
    var
      code: int
      reason: string
    # Iterate through each `<member>`
    for child in struct:
      var name: XmlNode
      try:
        name = child[0]
      except IndexDefect:
        raise newException(XmlRpcDecodingException, "Expected a <name> tag")
      if name.innerText == "faultCode":
        try:
          code = parseInt(child[1][0].innerText)
        except ValueError as e:
          raise newException(XmlRpcDecodingException,
              "Unable to parse integer: " & e.msg)
        except IndexDefect:
          raise newException(XmlRpcDecodingException, "Expected a <value> tag")
      elif name.innerText == "faultString":
        try:
          reason = child[1][0].innerText
        except IndexDefect:
          raise newException(XmlRpcDecodingException, "Expected a <string> tag")
    return XmlRpcResponse(k: XmlRpcResponseKind.fault, code: code, str: reason)
  elif params.len == 1:
    let param = params[0]
    var value: XmlNode
    try:
      # <param><value><T>...</T></value></param>
      #                |
      #                |-- Attempting to access this
      value = param[0][0][0]
    except IndexDefect:
      raise newException(XmlRpcDecodingException, "Expected tag <T> where T is an XMl-RPC type")
    let r = deserialize(value)
    return XmlRpcResponse(k: XmlRpcResponseKind.methodResponse, response: r)
  else:
    raise newException(XmlRpcDecodingException, "Received an invalid XML-RPC response type. Expected a <fault> or <params>")

proc getFuncName(name: NimNode): string =
  ## Get the function name
  ##
  var funcName = name.repr.replace(".", "_").replace("\"", "")
  return funcName.strip(chars = {'\r', '\n', '\t'})

proc getFuncName(name: string) : string =
  var funcName = name.replace(".", "_").replace("\"", "")
  return funcName.strip(chars = {'\r', '\n', '\t'})

proc generateFuncWithNoParams(f: NimNode, name: string): NimNode =
  ## Generate a function with no parameters
  ##
  let paramsNode = newNimNode(nnkFormalParams)
  paramsNode.add(ident("string"))
  f.add(paramsNode)
  f.add(newEmptyNode(), newEmptyNode())
  let funcBody = newStmtList(newAssignment(ident("result"), newCall(ident(
      "getMethodCall"), newLit(name))))
  f.add(funcBody)
  return f

macro xmlRpcSpecFromFile*(spec: static[string]): untyped =
  ## Compile time code generation from a XMl spec file.
  ##
  ## The cousin of `xmlRpcSpec`, this macro generates code
  ## from a file at compile time.
  ##
  if not fileExists(spec):
    error(spec & " does not exist")

  var 
    specSource = staticRead(spec)
    specXml: XmlNode

  try:
    specXml = parseXml(specSource)
  except Exception as e:
    error("Unable to parse file " & e.msg)

  var specs = newSeq[NimNode]()
  for meth in specXml:
    let mName = meth[0]
    let mParams = meth[1]

    var specFunc = newNimNode(nnkProcDef)
    specFunc.add(ident(getFuncName(mName.innerText)))
    specFunc.add(newEmptyNode(), newEmptyNode())
    let paramsNode = newNimNode(nnkFormalParams)
    paramsNode.add(ident("string"))

    var paramsBody = newSeq[NimNode]()
    paramsBody.add(newLit(mName.innerText))
    for param in mParams:
      let paramName = param[0]
      let paramType = param[1]
      let paramNameNode = ident(paramName.innerText)
      let paramTypeNode = ident(paramType.innerText)
      let paramNode = newTree(nnkIdentDefs, paramNameNode, paramTypeNode,
          newEmptyNode())
      paramsNode.add(paramNode)
      paramsBody.add(newTree(nnkPrefix, ident("to"), paramNameNode))
    specFunc.add(paramsNode)
    specFunc.add(newEmptyNode(), newEmptyNode())
    let funcbody = newStmtList()
    funcBody.add(newAssignment(ident("result"), newCall(ident("getMethodCall"), paramsBody)))
    specFunc.add(funcBody)
    specs.add(specFunc)

  # Pack all the generated code into a statement list
  result = newStmtList()
  for item in specs:
    result.add(item)

macro xmlRpcSpec*(body: untyped): untyped =
  ## DSL macro for XML-RPC specification.
  ##
  ## Consumers of this library should use this or
  ## its cousin `xmlRpcSpecFromFile` as it generates
  ## code so it makes it easy to get XML-RPC calls
  ## using basic Nim types.
  ##
  ## The DSL is defined as follows::
  ##  xmlRpcSpec:
  ##   name: <string>
  ##   [params:
  ##     <string_wo_quotes>:<basic Nim type>
  ##     ...
  ##   ]
  ##
  ## where `<string>` is a double-quoted string and
  ## `<string_wo_quotes>` is a string without any quotes.
  ##
  ## This macro generates code according to
  ## the specifiction given.
  ##
  ## Example::
  ##  xmlRpcSpec:
  ##   name: "download_list"
  ##   name: "d.name"
  ##   params:
  ##     hash: string
  ##
  ## The above example generates 2 functions with the names
  ## `download_list` and `d_name` (illegal characters within the
  ## function name are replaced with '_'). The first parameter
  ## does not take any parameters so it's prototype is::
  ##  proc download_list() : string
  ##
  ## `d_name` does and thus it's prototype is::
  ##  proc d_name(hash: string) : string
  ##
  ## You can then use the following functions in Nim code::
  ##  discard download_list()
  ##  discard d_name("2FDSF33DFDSJKB32423")
  ##
  body.expectKind nnkStmtList
  var spec = newSeq[NimNode]()
  var funcs = newSeq[(string, NimNode)]()
  for call in body:
    call.expectKind nnkCall
    let ident = call[0]
    if ident.repr == "name":
      let name = call[1]
      # Parse the function name
      let funcName = getFuncName(name)
      # Create the AST
      let funcNode = newNimNode(nnkProcDef)
      let funcNameNode = ident(funcName)
      funcNode.add(funcNameNode)
      funcNode.add(newEmptyNode(), newEmptyNode())

      if funcs.len != 0:
        let pastFunc = funcs.pop()
        let name = pastFunc[0]
        let pFunc = pastFunc[1]
        # All XML-RPC calls return XmlNode
        # This specific case creates a proc with zero parameters
        spec.add(generateFuncWithNoParams(pFunc, name))
      funcs.add((name.repr.replace("\"", ""), funcNode))
    elif ident.repr == "params":
      call[1].expectKind nnkStmtList
      let params = call[1]
      let prev = funcs.pop()
      let name = prev[0]
      let currentFunc = prev[1]
      # All XML-RPC calls return XmlNode
      let paramsNode = newNimNode(nnkFormalParams)
      paramsNode.add(ident("string"))

      var paramsBody = newSeq[NimNode]()
      # XML-RPC function name is the first parameter
      paramsBody.add(newLit(name))

      # Generate the procedure prototype and params in single loop
      for param in params:
        let paramName = param[0]
        let paramNameNode = ident(paramName.repr)
        let formalType = param[1][0]
        let formalTypeNode = ident(formalType.repr)
        let param = newTree(nnkIdentDefs, paramNameNode, formalTypeNode,
            newEmptyNode())
        paramsNode.add(param)
      # Params get converted into XmlRpc type by using macro `to`
        paramsBody.add(newTree(nnkPrefix, ident("to"), paramNameNode))
      currentFunc.add(paramsNode)
      # Function body
      currentFunc.add(newEmptyNode(), newEmptyNode())
      let funcBody = newStmtList()
      funcbody.add(newAssignment(ident("result"), newCall(ident(
          "getMethodCall"), paramsBody)))
      currentFunc.add(funcBody)
      spec.add(currentFunc)
    else:
      error("Invalid identifier", ident)

  # Handle any leftover that have no params
  if funcs.len != 0:
    let prev = funcs.pop()
    let name = prev[0]
    let pastFunc = prev[1]
    spec.add(generateFuncWithNoParams(pastFunc, name))

  # Pack all the generated code into a statement list
  result = newStmtList()
  for item in spec:
    result.add(item)

proc getRpcType(this: XmlRpcType): string =
  ## Get the RPC type of `this` as `string`
  ##
  case this.k:
    of xmlRpcInteger:
      result = "i" & $sizeof(this.fInt)
    of xmlRpcFloat:
      result = "double"
    of xmlRpcArray:
      result = "array"
    of xmlRpcBase64:
      result = "base64"
    of xmlRpcString:
      result = "string"
    of xmlRpcStruct:
      result = "struct"
    of xmlRpcBoolean:
      result = "boolean"
    of xmlRpcDateTime:
      result = "dateTime.iso8601"

proc serialize(this: XmlRpcType): XmlNode =
  ## Gives an XML representation of `this` as:
  ## <value><T>VALUE</T></value>
  ##
  case this.k:
    of xmlRpcInteger:
      result = newElement(this.getRpcType)
      result.add newText($this.fInt)
    of xmlRpcBoolean:
      result = newElement(this.getRpcType)
      result.add newText(if this.fBool: "1" else: "0")
    of xmlRpcString, xmlRpcBase64:
      result = newElement(getRpcType(this))
      result.add newText(this.fString)
    of xmlRpcFloat:
      result = newElement(getRpcType(this))
      result.add newText($this.fFloat)
    of xmlRpcDateTime:
      result = newElement(getRpcType(this))
      result.add newText($this.fDateTime)
    of xmlRpcStruct:
      result = newElement(getRpcType(this))
      # (name: string, field: XmlRpcType)
      for member in this.fStruct:
        let memberNode = newElement("member")

        let name = newElement("name")
        let innerName = newText(member[0])
        name.add innerName
        memberNode.add name

        let value = newElement("value")
        # Recursively call XmlRpcType elements
        value.add member[1].serialize
        memberNode.add value

        result.add memberNode
    of xmlRpcArray:
      result = newElement(getRpcType(this))
      let data = newElement("data")

      for item in this.fArray:
        let value = newElement("value")
        value.add item.serialize
        data.add value

      result.add data

proc `$`*(this: XmlRpcType): string =
  ## String representation
  ##
  result = $serialize(this)

proc getMethodCall*(name: string, params: varargs[XmlRpcType]): string =
  ## Generate a `methodCall` given `name` and a list of `XmlRpcType`s.
  ##
  ## Returns a `string` representation of the XML-RPC call.
  ##
  ## Consumers of this library shouldn't need to use this but is provided
  ## for completeness. The macros `xmlRpcSpec` and `xmlRpcSpecFromFile`
  ## both construct `procs` that have Nim types as parameters with
  ## this method generating the XML-RPC method call string in the body. Use
  ## that instead of this method directly as it involves writing less code.
  ##
  ## Example usage::
  ##  getMethodCall("download_item")
  ##
  let root = newElement("methodCall")

  let methodName = newElement("methodName")
  methodName.add newText(name)

  let paramsNode = newElement("params")

  for param in params:
    let paramNode = newElement("param")
    let value = newElement("value")
    # Since <array>'s and <struct>'s are recursive
    # we need to serialize them
    value.add param.serialize()
    paramNode.add value
    paramsNode.add paramNode

  root.add methodName
  root.add paramsNode
  return $root

when isMainModule:
  type
    A = object
      a: seq[string]

  # to(A(a: @["Mark", "Park", "Shark"]))

  # echo XmlRpcType(k: xmlRpcStruct, fStruct: @[
  #   ("a", XmlRpcType(k: xmlRpcArray, fArray: @[
  #     XmlRpcType(k: xmlRpcString, fString: "Mark"),
  #     XmlRpcType(k: xmlRpcString, fString: "Park"),
  #     XmlRpcType(k: xmlRpcString, fString: "Shark")
  #   ]))
  # ])
