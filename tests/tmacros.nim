import std/unittest
import std/random
import std/times
import std/base64

import nerdtalk

randomize(now().second)

suite "XMl-RPC Serialization Macro":
  test "Integers":
    let q = rand(0..1_000_000)
    let i = `from` q
    let j = XmlRpcType(k: xmlRpcInteger, fInt: q)
    check i == j
  test "Strings":
    let i = `from` "Hello World"
    let j = XmlRpcType(k: xmlRpcString, fString: "Hello World")
    check i == j
  test "Booleans":
    let i = `from` true
    let j = XmlRpcType(k: xmlRpcBoolean, fBool: true)
    check i == j
  test "Float":
    let q = rand(1_000_000.00)
    let i = `from` q
    let j = XmlRpcType(k: xmlRpcFloat, fFloat: q)
    check i == j
  test "Datetime":
    let q = now()
    let i = `from` q
    let j = XmlRpcType(k: xmlRpcDateTime, fDateTime: q)
    check i == j
  test "Base64":
    let q = rand(0..1_000_000)
    let i = ?:q
    let j = XmlRpcType(k: xmlRpcBase64, fString: encode($q))
    check i == j
  test "Struct":
    type
      User = object
        name: string
        id: int
    let i = `from` User(name: "John Doe", id: 1001)
    let j = XmlRpcType(k: xmlRpcStruct, fStruct: @[
      ("name", XmlRpcType(k: xmlRpcString, fString: "John Doe")),
      ("id", XmlRpcType(k: xmlRpcInteger, fInt: 1001))
      ])
    check i == j
  test "Struct 2":
    type
      User = object
        name: string
        id: Natural
    let i = `from` User(name: "Steve Wonder", id: 1001)
    let j = XmlRpcType(k: xmlRpcStruct, fStruct: @[
      ("name", XmlRpcType(k: xmlRpcString, fString: "Steve Wonder")),
      ("id", XmlRpcType(k: xmlRpcInteger, fInt: 1001))
    ])
    check i == j
  test "Complex objects":
    type
      Person = object
        name: string
        age: int
      Dog = object
        name: string
        age: int
        owner: Person
    let o = Person(name: "Hank Hill", age: 43)
    let i = `from` Dog(name: "Ladybird", age: 4, owner: o)
    let j = XmlRpcType(k: xmlRpcStruct, fStruct: @[
      ("name", XmlRpcType(k: xmlRpcString, fString: "Ladybird")),
      ("age", XmlRpcType(k: xmlRpcInteger, fInt: 4)),
      ("owner", XmlRpcType(k: xmlRpcStruct, fStruct: @[
        ("name", XmlRpcType(k: xmlRpcString, fString: "Hank Hill")),
        ("age", XmlRpcType(k: xmlRpcInteger, fInt: 43))
      ]))
    ])
    check i == j
  test "Arrays":
    type
      Users {.xrarray.} = object
        names: string
        ids: int
    let i = `from` Users(names: "John Doe", ids: 1001)
    let j = XmlRpcType(k: xmlRpcArray, fArray: @[
      XmlRpcType(k: xmlRpcString, fString: "John Doe"),
      XmlRpcType(k: xmlRpcInteger, fInt: 1001)
      ])
    check i == j
  #test "Complex Arrays":