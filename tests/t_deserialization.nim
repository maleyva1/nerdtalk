import std/unittest

import nerdtalk

suite "XML-RPC Response deserialization":
    test "Faults":
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
        check i.k == XmlRpcResponseKind.fault
        check i.code == 4
        check i.str == "Too many parameters."
    test "MethodResponse":
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