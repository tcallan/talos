module DiffTests

open Xunit
open FsCheck.Xunit
open Swensen.Unquote
open JsonArb
open Talos.Diff
open Chiron
open DiffTestData
module E = Json.Encode
module EI = Inference.Json.Encode
module Json = Inference.Json

type SimpleContract = {
    prop : string
}

type SimpleContract with
    static member ToJson (x : SimpleContract) = 
        E.buildWith (fun x jObj ->
            jObj
            |> EI.required "prop" x.prop) x

type NestedContract = {
    yep : SimpleContract
}

type NestedContract with
    static member ToJson (x : NestedContract) =
        EI.required "yep" x.yep

let parseUnsafe json = json |> Json.parse |> JsonResult.getOrThrow
let inline deserializeUnsafe json = json |> Json.deserialize |> JsonResult.getOrThrow

let assertPatchSuccess original p expected =
    let original = parseUnsafe original
    let p = deserializeUnsafe p
    let expected = parseUnsafe expected

    patch p original |> JsonResult.getOrThrow =! expected

let assertPatchFailure original p msg =
    let original = parseUnsafe original
    let p = deserializeUnsafe p
    let expected = msg |> JsonResult.invalidJson

    patch p original =! expected

let assertPatchForgivingSuccess original p expected =
    let original = parseUnsafe original
    let p = deserializeUnsafe p
    let expected = parseUnsafe expected

    patchForgiving p original |> JsonResult.getOrThrow =! expected

let assertPatchForgivingFailure original p msg =
    let original = parseUnsafe original
    let p = deserializeUnsafe p
    let expected = msg |> JsonResult.invalidJson

    patchForgiving p original =! expected

[<Fact>]
let ``case 1 produces correct result`` () =
    assertPatchSuccess Case1.original Case1.patch Case1.expected

[<Fact>]
let ``case 2 produces correct result`` () =
    assertPatchSuccess Case2.original Case2.patch Case2.expected

[<Fact>]
let ``case 3 produces correct result`` () =
    assertPatchFailure Case3.original Case3.patch Case3.error

[<Fact>]
let ``case 3 produces correct forgiving result`` () =
    assertPatchForgivingSuccess Case3.original Case3.patch Case3.expected

[<Fact>]
let ``case 4 produces correct result`` () =
    assertPatchFailure Case4.original Case4.patch Case4.error

[<Fact>]
let ``case 4 produces correct forgiving result`` () =
    assertPatchForgivingSuccess Case4.original Case4.patch Case4.expected

[<Fact>]
let ``case a1 produces correct result`` () =
    assertPatchSuccess CaseA1.original CaseA1.patch CaseA1.expected

[<Fact>]
let ``case a2 produces correct result`` () =
    assertPatchSuccess CaseA2.original CaseA2.patch CaseA2.expected

[<Fact>]
let ``case a3 produces correct result`` () =
    assertPatchSuccess CaseA3.original CaseA3.patch CaseA3.expected

[<Fact>]
let ``case a4 produces correct result`` () =
    assertPatchSuccess CaseA4.original CaseA4.patch CaseA4.expected

[<Fact>]
let ``case a5 produces correct result`` () =
    assertPatchSuccess CaseA5.original CaseA5.patch CaseA5.expected

[<Fact>]
let ``case a6 produces correct result`` () =
    assertPatchSuccess CaseA6.original CaseA6.patch CaseA6.expected

[<Fact>]
let ``case a7 produces correct result`` () =
    assertPatchSuccess CaseA7.original CaseA7.patch CaseA7.expected

[<Fact>]
let ``case a8 produces correct result`` () =
    assertPatchSuccess CaseA8.original CaseA8.patch CaseA8.expected

[<Fact>]
let ``case a9 produces correct result`` () =
    assertPatchFailure CaseA9.original CaseA9.patch CaseA9.error

[<Fact>]
let ``case a9 produces correct forgiving result`` () =
    assertPatchForgivingFailure CaseA9.original CaseA9.patch CaseA9.error

[<Fact>]
let ``case a10 produces correct result`` () =
    assertPatchSuccess CaseA10.original CaseA10.patch CaseA10.expected

[<Fact>]
let ``case a11 produces correct result`` () =
    assertPatchSuccess CaseA11.original CaseA11.patch CaseA11.expected

[<Fact>]
let ``case a12 produces correct result`` () =
    assertPatchFailure CaseA12.original CaseA12.patch CaseA12.error

[<Fact>]
let ``case a12 produces correct forgiving result`` () =
    assertPatchForgivingSuccess CaseA12.original CaseA12.patch CaseA12.expected

[<Fact>]
let ``case a14 produces correct result`` () =
    assertPatchSuccess CaseA14.original CaseA14.patch CaseA14.expected

[<Fact>]
let ``case a15 produces correct result`` () =
    assertPatchFailure CaseA15.original CaseA15.patch CaseA15.error

[<Fact>]
let ``case a15 produces correct forgiving result`` () =
    assertPatchForgivingFailure CaseA15.original CaseA15.patch CaseA15.error

[<Fact>]
let ``case a16 produces correct result`` () =
    assertPatchSuccess CaseA16.original CaseA16.patch CaseA16.expected

[<Property(Arbitrary = [| typeof<Arbitrary> |])>]
let ``diff of identical documents is empty`` json =
    diff json json = {PatchOperations = []}

[<Property(Arbitrary = [| typeof<Arbitrary> |])>]
let ``creating and then applying a patch is equivalent to identity`` f t =
    let p = diff f t
    patch p f |> JsonResult.getOrThrow = t

[<Fact>]
let ``updating a non-existent property works (forgiving)`` () =
    assertPatchForgivingSuccess CaseF1.original CaseF1.patch CaseF1.expected

[<Fact>]
let ``updating a non-existent property fails`` () =
    assertPatchFailure CaseF1.original CaseF1.patch CaseF1.error