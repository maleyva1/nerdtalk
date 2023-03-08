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

An `int` maps to `<iN>` where `N` is the bit width of the integer on the host platform

A `float` maps to `<double>`

A `string`, `char`, and `cstring` maps to `<string>`

A `DateTime` object maps to `<datetime.iso8601>`

A `bool` maps to `<boolean>`

An `object` maps to `<struct>`

An `object` marked with  `{.xrarray.}` maps to `<array>`

Sequences and arrays map to `<array>`

Any type `T` can map to `<base64>` iff `T` implements `$`

## F.A.Q.

* Why `{.xarray.}`?

    XML-RPC allows different types within `<array>`.
    The `{.xarray.}` allows users to do so with custom
    object types since sequences and arrays require
    the same type.

## TODO

* Add support for deserialization between `XmlRpcType` and user-types a la `std/json`
* Add more tests for deserialization and exception handling

