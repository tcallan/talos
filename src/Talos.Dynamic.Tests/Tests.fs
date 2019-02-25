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

type DecimalContract = {
    DecProp : decimal
}

type IntContract = {
    IntProp : int32
}

type LongContract = {
    LongProp : int64
}

type FloatContract = {
    FloatProp : float
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
let ``DiffToJsonPatch produces correct result for decimal values`` () =
    let a = { DecProp = 50m }
    let b = { DecProp = 50.00m }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Empty(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for distinct decimal values`` () =
    let a = { DecProp = 50m }
    let b = { DecProp = 50.01m }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Single(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for int32 values`` () =
    let a = { IntProp = 50 }
    let b = { IntProp = 50 }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Empty(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for distinct int32 values`` () =
    let a = { IntProp = 50 }
    let b = { IntProp = 51 }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Single(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for int64 values`` () =
    let a = { LongProp = 50L }
    let b = { LongProp = 50L }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Empty(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for distinct int64 values`` () =
    let a = { LongProp = 50L }
    let b = { LongProp = 51L }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Single(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for float values`` () =
    let a = { FloatProp = 50.0 }
    let b = { FloatProp = 50.00 }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Empty(res.Operations)

[<Fact>]
let ``DiffToJsonPatch produces correct result for distinct float values`` () =
    let a = { FloatProp = 50.0 }
    let b = { FloatProp = 50.01 }

    let res = Diff.DiffToJsonPatch (a, b)

    Assert.Single(res.Operations)

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