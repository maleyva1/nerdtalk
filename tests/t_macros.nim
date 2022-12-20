# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import std/unittest
import std/random
import std/times
import std/base64

import nerdtalk

randomize()

suite "XMl-RPC Serialization Macro":
  test "Integers":
    let q = rand(0..1_000_000)
    let i = !:q
    let j = XmlRpcType(k: xmlRpcInteger, fInt: q)
    check i.k == j.k
    check i.fInt == j.fInt
  test "Strings":
    let i = !:"Hello World"
    let j = XmlRpcType(k: xmlRpcString, fString: "Hello World")
    check i.k == j.k
    check i.fString == j.fString
  test "Booleans":
    let i = !:true
    let j = XmlRpcType(k: xmlRpcBoolean, fBool: true)
    check i.k == j.k
    check i.fBool == j.fBool
  test "Float":
    let q = rand(1_000_000.00)
    let i = !:q
    let j = XmlRpcType(k: xmlRpcFloat, fFloat: q)
    check i.k == j.k
    check i.fFloat == j.fFloat
  test "Datetime":
    let q = now()
    let i = !:q
    let j = XmlRpcType(k: xmlRpcDateTime, fDateTime: q)
    check i.k == j.k
    check i.fDateTime == j.fDateTime
  test "Base64":
    let q = rand(0..1_000_000)
    let i = ?:q
    let j = XmlRpcType(k: xmlRpcBase64, fString: encode($q))
    check i.k == j.k
    check i.fString == j.fString