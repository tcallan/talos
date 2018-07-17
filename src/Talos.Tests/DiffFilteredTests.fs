module DiffFilteredTests

open Xunit
open Swensen.Unquote
open Chiron
open Talos
open Talos.Pointer
open Talos.Patch
module E = Json.Encode
module EI = Inference.Json.Encode
module Json = Inference.Json

type TestContract1 = {
    prop1 : string list
    prop2 : string list
}
with
    static member ToJson (x : TestContract1) =
        E.buildWith (fun x jObj ->
            jObj
            |> EI.required "prop1" x.prop1
            |> EI.required "prop2" x.prop2) x

type TestContract2 = {
    propS : TestContract1
}
with
    static member ToJson (x : TestContract2) =
        E.buildWith (fun x jObj ->
            jObj
            |> EI.required "propS" x.propS) x

type TestContract3 = {
    propL : TestContract1 list
}
with
    static member ToJson (x : TestContract3) =
        E.buildWith (fun x jObj ->
            jObj
            |> EI.required "propL" x.propL) x

type TestContract4 = {
    prop3 : TestContract1
    prop4 : TestContract1
}
with
    static member ToJson (x : TestContract4) =
        E.buildWith (fun x jObj ->
            jObj
            |> EI.required "prop3" x.prop3
            |> EI.required "prop4" x.prop4) x

type TestContract5 = {
    prop5 : TestContract2
    prop6 : TestContract2
}
with
    static member ToJson (x : TestContract5) =
        E.buildWith (fun x jObj ->
            jObj
            |> EI.required "prop5" x.prop5
            |> EI.required "prop6" x.prop6) x

[<Fact>]
let ``filtering an array results in a simple replace`` () =
    let original = Json.encode <| {
        prop1 = ["a"; "b"]
        prop2 = ["a"; "b"]
    }
    let modified = Json.encode <| {
        prop1 = ["a"; "b"; "c"]
        prop2 = ["a"; "b"; "c"]
    }
    let filter = [Pointer [OKey "prop2"]]

    let result = Diff.diffFiltered filter original modified

    let expected =
        { PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "prop1"; AKey 2];
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "prop2"];
                   ChangeValue = Array [String "a"; String "b"; String "c"]}]}

    result =! expected

[<Fact>]
let ``filtering an array with quotes results in a simple replace`` () =
    let original = Json.encode <| {
        prop1 = ["a"; "b"]
        prop2 = ["a"; "b"]
    }
    let modified = Json.encode <| {
        prop1 = ["a"; "b"; "c"]
        prop2 = ["a"; "b"; "c"]
    }
    let filter = [ <@@ fun j -> j.prop2 @@> ]

    let result = Diff.diffFilteredQ filter original modified

    let expected =
        { PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "prop1"; AKey 2];
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "prop2"];
                   ChangeValue = Array [String "a"; String "b"; String "c"]}]}

    result =! expected

[<Fact>]
let ``filtering a nested array results in a simple replace`` () =
    let original = Json.encode <| {
        propS =
            {
                prop1 = ["a"; "b"]
                prop2 = ["a"; "b"]
            }
    }
    let modified = Json.encode <| {
        propS =
            {
                prop1 = ["a"; "b"; "c"]
                prop2 = ["a"; "b"; "c"]
            }
    }
    let filter = [Pointer [OKey "propS"; OKey "prop2"]]

    let result = Diff.diffFiltered filter original modified

    let expected =
        {PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "propS"; OKey "prop1"; AKey 2];
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "propS"; OKey "prop2"];
                   ChangeValue = Array [String "a"; String "b"; String "c"]}]}

    result =! expected

[<Fact>]
let ``filtering a nested array with quotes results in a simple replace`` () =
    let original = Json.encode <| {
        propS =
            {
                prop1 = ["a"; "b"]
                prop2 = ["a"; "b"]
            }
    }
    let modified = Json.encode <| {
        propS =
            {
                prop1 = ["a"; "b"; "c"]
                prop2 = ["a"; "b"; "c"]
            }
    }
    let filter = [ <@ fun j -> j.propS.prop2 @> ]

    let result = Diff.diffFilteredQ filter original modified

    let expected =
        {PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "propS"; OKey "prop1"; AKey 2];
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "propS"; OKey "prop2"];
                   ChangeValue = Array [String "a"; String "b"; String "c"]}]}

    result =! expected

[<Fact>]
let ``filtering an array within an array results in a simple replace`` () =
    let original = Json.encode <| {
        propL = [{prop1 = ["a"; "b"]; prop2 = ["a"; "b"]}]
    }
    let modified = Json.encode <| {
        propL = [{prop1 = ["a"; "b"; "c"]; prop2 = ["a"; "b"; "c"]}]
    }
    let filter = [Pointer [OKey "propL"; AKey 0; OKey "prop2"]]

    let result = Diff.diffFiltered filter original modified

    let expected =
        {PatchOperations = 
            [ Add {ChangePointer = Pointer [OKey "propL"; AKey 0; OKey "prop1"; AKey 2]
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "propL"; AKey 0; OKey "prop2"]
                   ChangeValue = Array [String "a"; String "b"; String "c"]}]}
    
    result =! expected

[<Fact>]
let ``filtering an object results in a simple replace`` () =
    let original = Json.encode <| {
        prop3 = {prop1 = ["a"; "b"]; prop2 = []}
        prop4 = {prop1 = ["a"; "b"]; prop2 = []}
    }
    let modified = Json.encode <| {
        prop3 = {prop1 = ["a"; "b"; "c"]; prop2 = []}
        prop4 = {prop1 = ["a"; "b"; "c"]; prop2 = []}
    }
    let filter = [Pointer [OKey "prop4"]]

    let result = Diff.diffFiltered filter original modified

    let expected =
        {PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "prop3"; OKey "prop1"; AKey 2]
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "prop4"]
                   ChangeValue = {prop1 = ["a"; "b"; "c"]; prop2 = []}|> Json.encode}]}

    result =! expected

[<Fact>]
let ``filtering an object with quotes results in a simple replace`` () =
    let original = Json.encode <| {
        prop3 = {prop1 = ["a"; "b"]; prop2 = []}
        prop4 = {prop1 = ["a"; "b"]; prop2 = []}
    }
    let modified = Json.encode <| {
        prop3 = {prop1 = ["a"; "b"; "c"]; prop2 = []}
        prop4 = {prop1 = ["a"; "b"; "c"]; prop2 = []}
    }
    let filter = [ <@@ fun j -> j.prop4 @@> ]

    let result = Diff.diffFilteredQ filter original modified

    let expected =
        {PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "prop3"; OKey "prop1"; AKey 2]
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "prop4"]
                   ChangeValue = {prop1 = ["a"; "b"; "c"]; prop2 = []}|> Json.encode}]}

    result =! expected

[<Fact>]
let ``filtering a nested object results in a simple replace`` () =
    let original = Json.encode <| {
        prop5 = {propS = {prop1 = ["a"; "b"]; prop2 = []}}
        prop6 = {propS = {prop1 = ["a"; "b"]; prop2 = []}}
    }
    let modified = Json.encode <| {
        prop5 = {propS = {prop1 = ["a"; "b"; "c"]; prop2 = []}}
        prop6 = {propS = {prop1 = ["a"; "b"; "c"]; prop2 = []}}
    }
    let filter = [Pointer [OKey "prop6"; OKey "propS"]]

    let result = Diff.diffFiltered filter original modified

    let expected =
        {PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "prop5"; OKey "propS"; OKey "prop1"; AKey 2]
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "prop6"; OKey "propS"]
                   ChangeValue = {prop1 = ["a"; "b"; "c"]; prop2 = []} |> Json.encode}]}

    result =! expected

[<Fact>]
let ``filtering a nested object with quotes results in a simple replace`` () =
    let original = Json.encode <| {
        prop5 = {propS = {prop1 = ["a"; "b"]; prop2 = []}}
        prop6 = {propS = {prop1 = ["a"; "b"]; prop2 = []}}
    }
    let modified = Json.encode <| {
        prop5 = {propS = {prop1 = ["a"; "b"; "c"]; prop2 = []}}
        prop6 = {propS = {prop1 = ["a"; "b"; "c"]; prop2 = []}}
    }
    let filter = [ <@@ fun j -> j.prop6.propS @@> ]

    let result = Diff.diffFilteredQ filter original modified

    let expected =
        {PatchOperations =
            [ Add {ChangePointer = Pointer [OKey "prop5"; OKey "propS"; OKey "prop1"; AKey 2]
                   ChangeValue = String "c"}
              Rep {ChangePointer = Pointer [OKey "prop6"; OKey "propS"]
                   ChangeValue = {prop1 = ["a"; "b"; "c"]; prop2 = []} |> Json.encode}]}

    result =! expected