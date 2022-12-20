# nerdtalk

`nerdtalk` is an XML-RPC implementation in pure Nim.

```nim
#                                                                          (object)
# Method Name -------|                                                    structure --|           (object)
#                    | string ----| int --|   base64 -| bool -- |    float -|         |             Array --|
#                    |            |       |           |         |           |         |                     |
# getMethodCall("update_account", !:"mark", !:2, ?:"password", !:true, !:3.1456, !:payload, !:now(), !:arr)
#                                                                                                 |
#                                                                                    DateTime ----|
```

## Example Usage

Here is an example `nerdtalk` using the `xmlRpcSpec` macro:

```nim
import nerdtalk

xmlRpcSpec:
    name: "download_list"
    name: "d.name"
    params:
        hash: string

when isMainModule:
    echo download_list()
    echo d_name("ABDCEFGH1234567890")
```

For de-serialization:

```nim
import nerdtalk

# ...

socket.read(buffer, 1023)
var result = :!buffer
case result:
  of xmlRpcString:
    echo result.fString
  else:
    discard
```

## Implementation

In order to increase ease of use, `nerdtalk` maps Nim's type system
to XML-RPC "types".

An `int` maps to `<int>` or `<iN>` where `N` is the byte-width of the integer

A `float` maps to `<double>`

A `string` maps to `<string>`

A `DateTime` object maps to `<datetime.iso8601>`

A `bool` maps to `<boolean>`

An `object` maps to `<struct>`

An `object` marked `{.xrarray.}` maps to `<array>`

Any type `T` can map to `<base64>` iff `T` implements `$`

## TODO

- ~~Allow recursive definition of struct types~~
- ~~Implement serialization of XML-RPC arrays~~
- ~~Implement recursive XMl-RPC array definitions?~~
- ~~Allow code generation through DSL~~
- ~~Implement byte-width specific integer types~~
- ~~Implement de-serialization of XML-RPC responses~~
- ~~Allow compile time reading of XML spec~~