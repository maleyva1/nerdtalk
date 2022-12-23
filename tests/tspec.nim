import std/unittest
import std/xmlparser
import std/xmltree
import std/times

import nerdtalk

suite "XMl-RPC Spec":
    test "Spec":
        type
            User = object
                name: string
                id: int
        xmlRpcSpec:
            name: "get_users"
            name: "update_user"
            params:
                user: User
            name: "delete_user"
            params:
                user: User
            name: "change_dob"
            params:
                user: User
                oldDt: DateTime
                newDt: DateTime
        let response = """
        <methodCall>
            <methodName>get_users</methodName>
            <params></params>
        </methodCall>
        """
        let i = User(name: "Amy Lee", id: 1001)
        let j = parseXml(get_users())
        let k = parseXml(response)
        check $j == $k

        let response2 = """
        <methodCall>
            <methodName>delete_user</methodName>
            <params>
                <param>
                    <value>
                        <struct>
                            <member>
                                <name>name</name>
                                <value>
                                    <string>Amy Lee</string>
                                </value>
                            </member>
                            <member>
                                <name>id</name>
                                <value>
                                    <i8>1001</i8>
                                </value>
                            </member>
                        </struct>
                    </value>
                </param>
            </params>
        </methodCall>
        """
        let a = parseXml(delete_user(i))
        let b = parseXml(response2)
        check $a == $b
