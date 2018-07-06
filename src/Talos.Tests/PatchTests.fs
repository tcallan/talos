module PatchTests

open Xunit
open Swensen.Unquote
open Talos.Patch
open Talos.Pointer
open Chiron
module Json = Inference.Json

let inline rt x =
    x |> Json.serialize |> Json.deserialize |> JsonResult.getOrThrow

[<Fact>]
let ``Serializing operations works as expected`` () =
    let testCases = [
        Add {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"}
        Cpy {ChangePointer = Pointer [OKey "to"]; FromPointer = Pointer [OKey "from"]}
        Mov {ChangePointer = Pointer [OKey "to"]; FromPointer = Pointer [OKey "from"]}
        Rem {ChangePointer = Pointer [OKey "test"]}
        Rep {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"}
        Tst {ChangePointer = Pointer [OKey "test"]; ChangeValue = String "value"}
    ]

    testCases
    |> List.iter (fun case -> 
        rt case =! case)

let testPathModifier (Pointer p) =
    Pointer (p @ [OKey "testappend"])

[<Fact>]
let ``modifyPointer works with Add`` () =
    let add = Add {ChangePointer = Pointer []; ChangeValue = String "a"}
    let (Pointer result) = modifyPointer testPathModifier add |> changePointer

    result =! [OKey "testappend"]

[<Fact>]
let ``modifyPointer works with Cpy`` () =
    let cpy = Cpy {ChangePointer = Pointer []; FromPointer = Pointer []}
    let result =
        match modifyPointer testPathModifier cpy with
        | Cpy {ChangePointer=Pointer p1; FromPointer=Pointer p2} -> p1,p2
        | _ -> failwith "Cpy turned into something else????"

    result =! ([OKey "testappend"], [OKey "testappend"])

[<Fact>]
let ``modifyPointer works with Mov`` () =
    let mov = Mov {ChangePointer = Pointer []; FromPointer = Pointer []}
    let result =
        match modifyPointer testPathModifier mov with
        | Mov {ChangePointer=Pointer p1; FromPointer=Pointer p2} -> p1,p2
        | _ -> failwith "Mov turned into something else????"

    result =! ([OKey "testappend"], [OKey "testappend"])

[<Fact>]
let ``modifyPointer works with Rem`` () =
    let rem = Rem {ChangePointer = Pointer []}
    let (Pointer result) = modifyPointer testPathModifier rem |> changePointer

    result =! [OKey "testappend"]

[<Fact>]
let ``modifyPointer works with Rep`` () =
    let rep = Rep {ChangePointer = Pointer []; ChangeValue = String "a"}
    let (Pointer result) = modifyPointer testPathModifier rep |> changePointer

    result =! [OKey "testappend"]

[<Fact>]
let ``modifyPointer works with Tst`` () =
    let tst = Tst {ChangePointer = Pointer []; ChangeValue = String "a"}
    let (Pointer result) = modifyPointer testPathModifier tst |> changePointer

    result =! [OKey "testappend"]