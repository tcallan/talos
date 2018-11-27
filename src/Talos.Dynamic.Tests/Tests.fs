module Tests

open Xunit
open Swensen.Unquote
open Chiron
module Json = Inference.Json
open Talos.Patch
open Talos.Pointer
open Talos.Dynamic.Diff
open Microsoft.AspNetCore.JsonPatch.Operations
open Microsoft.AspNetCore.JsonPatch
open Newtonsoft.Json

[<Fact>]
let ``talosPatchToJsonPatch produces the correct result`` () =
    let ops = [
        Add {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"}
        Cpy {ChangePointer = Pointer [OKey "to"]; FromPointer = Pointer [OKey "from"]}
        Mov {ChangePointer = Pointer [OKey "to"]; FromPointer = Pointer [OKey "from"]}
        Rem {ChangePointer = Pointer [OKey "test"]}
        Rep {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"}
        Tst {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"}
    ]
    let p = {PatchOperations = ops}
    let p' = talosPatchToJsonPatch p

    Assert.Equal(6, p'.Operations.Count)
    Assert.Contains(p'.Operations,
        fun op -> op.OperationType = OperationType.Add && op.path = "/test" && op.value = box "value") 
    Assert.Contains(p'.Operations, 
        fun op -> op.OperationType = OperationType.Copy && op.path = "/to" && op.from = "/from")
    Assert.Contains(p'.Operations, 
        fun op -> op.OperationType = OperationType.Move && op.path = "/to" && op.from = "/from")
    Assert.Contains(p'.Operations, 
        fun op -> op.OperationType = OperationType.Remove && op.path = "/test")
    Assert.Contains(p'.Operations, 
        fun op -> op.OperationType = OperationType.Replace && op.path = "/test" && op.value = box "value")
    Assert.Contains(p'.Operations, 
        fun op -> op.OperationType = OperationType.Test && op.path = "/test" && op.value = box "value")

[<Fact>]
let ``jsonPatchToTalosPatch produces the correct result`` () =
    let p =
        JsonPatchDocument()
            .Add("/test", box "value")
            .Copy("/from", "/to")
            .Move("/from", "/to")
            .Remove("/test")
            .Replace("/test", box "value")
            .Test("/test", box "value")
    let p' = jsonPatchToTalosPatch p

    let ops = p'.PatchOperations
    Assert.Equal(6, ops |> List.length)
    Assert.Contains(ops,
        fun op -> op = Add {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"})
    Assert.Contains(ops,
        fun op -> op = Cpy {ChangePointer = Pointer [OKey "to"]; FromPointer = Pointer [OKey "from"]})
    Assert.Contains(ops,
        fun op -> op = Mov {ChangePointer = Pointer [OKey "to"]; FromPointer = Pointer [OKey "from"]})
    Assert.Contains(ops,
        fun op -> op = Rem {ChangePointer = Pointer [OKey "test"]})
    Assert.Contains(ops,
        fun op -> op = Rep {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"})
    Assert.Contains(ops,
        fun op -> op = Tst {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"})

[<Fact>]
let ``talosPatchToJsonPatch preserves timezone`` () =
    let time = System.DateTimeOffset.FromUnixTimeSeconds(1533759265L).ToLocalTime()
    let ops = [Add {ChangePointer = Pointer [OKey "test"]; ChangeValue = String (time.ToString("o"))}]
    let p = {PatchOperations = ops}
    let p' = talosPatchToJsonPatch p

    let op = Assert.Single(p'.Operations)

    Assert.IsType(typeof<System.DateTimeOffset>, op.value)

    let dtoValue = op.value :?> System.DateTimeOffset

    Assert.Equal(time.ToString("o"), dtoValue.ToString("o"))

type SimpleContract = {
    [<JsonProperty("prop")>]
    Prop : string
}

type NestedContract = {
    [<JsonProperty("yep")>]
    Yep : SimpleContract
}

[<Fact>]
let ``Diff produces correct result`` () =
    let a = {Yep = {Prop = "foo"}}
    let b = {Yep = {Prop = "bar"}}

    let res = diff a b

    let op = Assert.Single(res.PatchOperations)
    let expectedOp = Rep {
        ChangePointer = Pointer [OKey "yep"; OKey "prop"]
        ChangeValue = String "bar"
    }

    op =! expectedOp

[<Fact>]
let ``Patch produces correct result`` () =
    let orig = {Yep = {Prop = "foo"}}
    let p =
        {PatchOperations =
            [Rep {ChangePointer = Pointer [OKey "yep"; OKey "prop"]; ChangeValue = String "bar"}]}   

    patch p orig =! {Yep = {Prop = "bar"}}

type NullableContact = {
    MaybeProp : System.Nullable<int>
}

[<Fact>]
let ``Diff produces correct result with non-null nullable`` () =
    let a = { MaybeProp = System.Nullable(10)}
    let b = { MaybeProp = System.Nullable(12)}

    let res = diff a b

    let op = Assert.Single(res.PatchOperations)

    let expectedOp = Rep {
        ChangePointer = Pointer [OKey "MaybeProp"]
        ChangeValue = Number "12"
    }

    op =! expectedOp

[<Fact>]
let ``Diff produces correct result with nullable made null`` () =
    let a = { MaybeProp = System.Nullable(10)}
    let b = { MaybeProp = System.Nullable()}

    let res = diff a b

    let op = Assert.Single(res.PatchOperations)

    let expectedOp = Rep {
        ChangePointer = Pointer [OKey "MaybeProp"]
        ChangeValue = Null
    }

    op =! expectedOp

[<Fact>]
let ``Diff produces correct result with nullable made not-null`` () =
    let a = { MaybeProp = System.Nullable()}
    let b = { MaybeProp = System.Nullable(12)}

    let res = diff a b

    let op = Assert.Single(res.PatchOperations)

    let expectedOp = Rep {
        ChangePointer = Pointer [OKey "MaybeProp"]
        ChangeValue = Number "12"
    }

    op =! expectedOp

[<Fact>]
let ``Patch produces correct result with non-null nullable`` () =
    let orig = { MaybeProp = System.Nullable(10)}
    let p =
        {PatchOperations =
            [Rep {ChangePointer = Pointer [OKey "MaybeProp"]; ChangeValue = Number "12" }]}   

    patch p orig =! { MaybeProp = System.Nullable(12)}

[<Fact>]
let ``Patch produces correct result with nullable made null`` () =
    let orig = { MaybeProp = System.Nullable(10)}
    let p =
        {PatchOperations =
            [Rep {ChangePointer = Pointer [OKey "MaybeProp"]; ChangeValue = Null }]}   

    patch p orig =! { MaybeProp = System.Nullable()}

[<Fact>]
let ``Patch produces correct result with nullable made non-null`` () =
    let orig = { MaybeProp = System.Nullable()}
    let p =
        {PatchOperations =
            [Rep {ChangePointer = Pointer [OKey "MaybeProp"]; ChangeValue = Number "12" }]}   

    patch p orig =! { MaybeProp = System.Nullable(12)}

[<Fact>]
let ``DiffToJsonPatch produces correct result`` () =
    let a = {Yep = {Prop = "foo"}}
    let b = {Yep = {Prop = "bar"}}

    let res = diffToJsonPatch a b

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

    patchWithJsonPatch p orig =! {Yep = {Prop = "bar"}}

[<Fact>]
let ``DiffToJsonPatch produces correct result with non-null nullable`` () =
    let a = { MaybeProp = System.Nullable(10)}
    let b = { MaybeProp = System.Nullable(12)}

    let res = diffToJsonPatch a b

    let op = Assert.Single(res.Operations)
    
    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/MaybeProp", op.path)
    Assert.Equal(System.Nullable(12 |> int64), op.value :?> System.Nullable<int64>)

[<Fact>]
let ``DiffToJsonPatch produces correct result with nullable made null`` () =
    let a = { MaybeProp = System.Nullable(10)}
    let b = { MaybeProp = System.Nullable()}

    let res = diffToJsonPatch a b

    let op = Assert.Single(res.Operations)
    
    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/MaybeProp", op.path)
    Assert.Equal(System.Nullable(), op.value :?> System.Nullable<int>)

[<Fact>]
let ``DiffToJsonPatch produces correct result with nullable made non-null`` () =
    let a = { MaybeProp = System.Nullable()}
    let b = { MaybeProp = System.Nullable(12)}

    let res = diffToJsonPatch a b

    let op = Assert.Single(res.Operations)
    
    Assert.Equal(OperationType.Replace, op.OperationType)
    Assert.Equal("/MaybeProp", op.path)
    Assert.Equal(System.Nullable(12 |> int64), op.value :?> System.Nullable<int64>)

type ArrayTest(vals : string array) =
    member __.Vals with get() = vals

[<Fact>]
let ``diff handles arrays properly with differences`` () =
    let a = ArrayTest([|"line1"|])
    let b = ArrayTest([|"line1"; "line2"|])
    let res = diff a b

    let op = Assert.Single(res.PatchOperations)
    let expected = Add { ChangePointer = Pointer [OKey "Vals"; AKey 1]; ChangeValue = String "line2"}

    op =! expected

[<Fact>]
let ``diff handles arrays properly without differences`` () =
    let a = ArrayTest([|"line1"|])
    let b = ArrayTest([|"line1"|])
    let res = diff a b

    Assert.Empty(res.PatchOperations)

[<Fact>]
let ``patch handles arrays properly`` () =
    let a = ArrayTest([||])
    let p =
        {PatchOperations =
            [Add { ChangePointer = Pointer [OKey "Vals"; AKey 0]; ChangeValue = String "line1"}]}

    let res = patch p a

    res.Vals =! [|"line1"|]

type DateTest(date : System.DateTimeOffset) =
    member __.Date with get() = date
