import std/unittest
import std/random
import std/times
import std/strformat

import nerdtalk

randomize(now().second)

proc generateMethodResponse(response: XmlRpcType) : string =
    return fmt"""
    <methodResponse>
        <params>
            <param>
                <value>
                    {response}
                </value>
            </param>
        </params>
    </methodResponse>
    """

suite "XML-RPC Response deserialization":
    test "Fault":
        let faultResponse = """
        <?xml version="1.0"?>
        <methodResponse>
            <fault>
                <value>
                    <struct>
                        <member>
                            <name>faultCode</name>
                            <value>
                                <int>4</int>
                            </value>
                        </member>
                        <member>
                            <name>faultString</name>
                            <value>
                                <string>Too many parameters.</string>
                            </value>
                        </member>
                        </struct>
                    </value>
                </fault>
            </methodResponse>
        """
        let i = :!faultResponse
        check i == XmlRpcResponse(k: XmlRpcResponseKind.fault, code: 4, str: "Too many parameters.")
    test "Simple method response":
        let payload = "South Dakota"
        let response = generateMethodResponse(to payload)
        let i = :!response
        let j = to payload
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response == j
    test "Complex method response (<struct>)":
        type
            Payload = object
                Name: string
        let payload = Payload(Name: "John")
        let response = generateMethodResponse(to payload)
        let i = :!response
        let j = to payload
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response == j
    test "Complex method response (<array>)":
        type
            Payload {.xrarray.} = object
                a: int
                b: float
                c: string
        let payload = Payload(a: 1, b: 1.0, c: "Hello World")
        let response = generateMethodResponse(to payload)
        let i = :!response
        let j = to payload
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response == j
    test "Ill-formed XML":
        let response = """
        <methodResponse>
            <params>
            </params>
        </methodResponse>
        """
        try:
            discard :!response
        except XmlRpcDecodingException as err:
            check err is ref XmlRpcDecodingException
    test "Integer deserialization":
        let randInt = rand(0..1_000_000)
        let response = generateMethodResponse(to randInt)
        let i = :!response
        let j = to randInt
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response == j
    test "Float deserialization":
        let randFloat = rand(1_000_000.00)
        let response = generateMethodResponse(to randFloat)
        let i = :!response
        let j = to randFloat
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response == j