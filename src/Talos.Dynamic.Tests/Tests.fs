module Tests

open Xunit
open Swensen.Unquote
open Chiron
module Json = Inference.Json
open Talos.Dynamic
open Microsoft.AspNetCore.JsonPatch.Operations
open Microsoft.AspNetCore.JsonPatch
open Newtonsoft.Json

type SimpleContract = {
    [<JsonProperty("prop")>]
    Prop : string
}

type NestedContract = {
    [<JsonProperty("yep")>]
    Yep : SimpleContract
}

type NullableContact = {
    MaybeProp : System.Nullable<int>
}

[<Fact>]
let ``DiffToJsonPatch produces correct result`` () =
    let a = {Yep = {Prop = "foo"}}
    let b = {Yep = {Prop = "bar"}}

    let res = Diff.DiffToJsonPatch (a, b)

    let op = Assert.Single(res.Operations)

    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/yep/prop", op.path)
    Assert.Equal("bar", op.value :?> string)

[<Fact>]
let ``PatchWithJsonPatch produces correct result`` () =
    let orig = {Yep = {Prop = "foo"}}
    let p =
        JsonPatchDocument()
            .Replace("/yep/prop", "bar")

    Diff.PatchWithJsonPatch(p, orig) =! {Yep = {Prop = "bar"}}

[<Fact>]
let ``DiffToJsonPatch produces correct result with non-null nullable`` () =
    let a = { MaybeProp = System.Nullable(10)}
    let b = { MaybeProp = System.Nullable(12)}

    let res = Diff.DiffToJsonPatch(a, b)

    let op = Assert.Single(res.Operations)
    
    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/MaybeProp", op.path)
    Assert.Equal(System.Nullable(12 |> int64), op.value :?> System.Nullable<int64>)

[<Fact>]
let ``DiffToJsonPatch produces correct result with nullable made null`` () =
    let a = { MaybeProp = System.Nullable(10)}
    let b = { MaybeProp = System.Nullable()}

    let res = Diff.DiffToJsonPatch (a, b)

    let op = Assert.Single(res.Operations)
    
    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/MaybeProp", op.path)
    Assert.Equal(System.Nullable(), op.value :?> System.Nullable<int>)

[<Fact>]
let ``DiffToJsonPatch produces correct result with nullable made non-null`` () =
    let a = { MaybeProp = System.Nullable()}
    let b = { MaybeProp = System.Nullable(12)}

    let res = Diff.DiffToJsonPatch (a, b)

    let op = Assert.Single(res.Operations)
    
    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/MaybeProp", op.path)
    Assert.Equal(System.Nullable(12 |> int64), op.value :?> System.Nullable<int64>)

[<Fact>]
let ``PatchWithJsonPatch respects IgnoreErrors`` () =
    let orig = {Yep = {Prop = "foo"}}
    let p =
        JsonPatchDocument()
            .Replace("/yep/prop", "bar")
            .Replace("/yop/prip", "baz")

    Assert.Throws<exn>(System.Action(fun _ -> Diff.PatchWithJsonPatch (p, orig)|> ignore)) |> ignore

    let settings = DiffSettings(IgnoreErrors = true)

    let res = Diff.PatchWithJsonPatch (p, orig, settings)

    Assert.Equal("bar", res.Yep.Prop)