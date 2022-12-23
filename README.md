# nerdtalk

`nerdtalk` is an XML-RPC implementation in pure Nim.

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

For deserialization (unmarshalling):

```nim
import nerdtalk

# ...

socket.read(buffer, 1023)
var response = :!buffer
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

An `object` marked with  `{.xrarray.}` maps to `<array>`

Any type `T` can map to `<base64>` iff `T` implements `$`

## TODO

- Possible serialization of generic sequences and arrays to heterogeneous `<array>` type
- Add more tests for deserialization and exception handling
- Add support for serialization between `XmlRpcType` and user-types a la `std/json`
- Check ISO-8601 XMl-RPC deserialization
- Improve XML-RPC integer parsing support. Can be `<int>` or `<i4>`, or in general `<iN>`
where `N` is the bit width of the integer on that platform
- Improve code generation in macros
