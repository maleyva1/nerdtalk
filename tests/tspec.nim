import std/unittest
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
        let j = User(name: "Amy Lee", id: 1001)
