import std/unittest

import nerdtalk

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
        let response = """
        <?xml version="1.0"?>
        <methodResponse>
            <params>
                <param>
                    <value>
                        <string>South Dakota</string>
                    </value>
                </param>
            </params>
        </methodResponse>
        """
        let i = :!response
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response.k == xmlRpcString
        check i.response.fString == "South Dakota"
    test "Complex method response (<struct>)":
        let response = """
        <methodResponse>
            <params>
                <param>
                    <value>
                        <struct>
                            <member>
                                <name>Name</name>
                                <value>
                                    <string>John</string>
                                </value>
                            </member>
                        </struct>
                    </value>
                </param>
            </params>
        </methodResponse>
        """
        type
            Temp = object
                Name: string
        let i = :!response
        let j = !:Temp(Name: "John")
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response == j
    test "Complex method response (<array>)":
        let response = """
        <methodResponse>
            <params>
                <param>
                    <value>
                        <array>
                            <data>
                                <value>
                                    <int>1</int>
                                </value>
                                <value>
                                    <double>1.0</double>
                                </value>
                                <value>
                                    <string>Hello World</string>
                                </value>
                            </data>
                        </array>
                    </value>
                </param>
            </params>
        </methodResponse>
        """
        let i = :!response
        check i.k == XmlRpcResponseKind.methodResponse
        check i.response.k == xmlRpcArray
    test "Ill-formed XML":
        let response = """
        <methodResponse>
            <params>
            </params>
        </methodResponse>
        """
