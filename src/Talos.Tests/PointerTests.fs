module PointerTests

open Talos.Pointer
open Xunit
open FsCheck.Xunit
open Chiron
open KeyArb
module D = Json.Decode
module E = Json.Encode
module Json = Inference.Json

let inline rt (x : 'a) =
    x |> Json.serialize |> Json.deserialize |> JsonResult.getOrThrow

[<Fact>]
let ``Key serialization works`` () =
    let o = Json.serialize (OKey "test")
    Assert.Equal("\"test\"", o)

    let a = Json.serialize (AKey 12)
    Assert.Equal("12", a)

[<Fact>]
let ``Key deserialization works`` () =
    let o : Key = Json.deserialize ("\"test\"") |> JsonResult.getOrThrow
    Assert.Equal(OKey "test", o)
    let a : Key = Json.deserialize ("12") |> JsonResult.getOrThrow
    Assert.Equal(AKey 12, a)

[<Property(Arbitrary = [| typeof<Arbitrary> |])>]
let ``Key serialization round trips`` (key : Key) =
    key = rt key

let pointerTestCases =
    [
        "\"\"", []
        "\"/\"", [OKey ""]
        "\"/ \"", [OKey " "]
        "\"/foo\"", [OKey "foo"]
        "\"/foo/0\"", [OKey "foo"; AKey 0]
        "\"/a~1b\"", [OKey "a/b"]
        "\"/c%d\"", [OKey "c%d"]
        "\"/e^f\"", [OKey "e^f"]
        "\"/g|h\"", [OKey "g|h"]
        "\"/i\\\\j\"", [OKey "i\\j"]
        "\"/k\\\"l\"", [OKey "k\"l"]
        "\"/m~0n\"", [OKey "m~n"]
        "\"/ 0\"", [OKey " 0"]
        "\"/0 \"", [OKey "0 "]
    ]

[<Fact>]
let ``Pointer serialization works`` () =
    pointerTestCases
    |> List.iter (fun (expected, path) -> 
        Assert.Equal(expected, Json.serialize (Pointer path)))

[<Fact>]
let ``Pointer deserialization works`` () =
    pointerTestCases
    |> List.iter (fun (pointer, expected) ->
        Assert.Equal(Pointer expected, Json.deserialize(pointer) |> JsonResult.getOrThrow))

[<Property(Arbitrary = [| typeof<Arbitrary> |])>]
let ``Pointer serialization round trips`` (pointer : Pointer) =
    let pointer' =
        let (Pointer p) = pointer
        if obj.ReferenceEquals(p, null)
        then Pointer []
        else Pointer p

    let rtPointer = rt pointer'

    // A path like [OKey "0"] will serialize to "/0" which will then deserialize
    // to [AKey 0]. This isn't inherently problematic, but needs to be handled explicitly
    let matchForLossyOKey (Pointer p1) (Pointer p2) =
        List.zip p1 p2
        |> List.forall (function
            | AKey i, OKey o -> i |> string = o
            | OKey o, AKey i -> i |> string = o
            | k1, k2 -> k1 = k2)

    pointer' = rtPointer || matchForLossyOKey pointer' rtPointer