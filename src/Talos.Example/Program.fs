open Talos
open Chiron
module Json = Inference.Json

let json1 = """
{
    "prop1": "val1",
    "prop2": {
        "nested1" : "val2",
        "nested2" : [
            { "p1" : 1, "p2" : 2},
            { "p1" : 3, "p2" : 4}
        ]
    }
}
"""

let json2 = """
{
    "prop1": "val1",
    "prop2": {
        "nested1" : "val2",
        "nested2" : [
            { "p1" : 1, "p2" : 2},
            { "p1" : 3, "p2" : 5},
            { "p1" : 5, "p2" : 6}
        ]
    }
}
"""

[<EntryPoint>]
let main _argv =
    let j1 = json1 |> Json.parse |> JsonResult.getOrThrow
    let j2 = json2 |> Json.parse |> JsonResult.getOrThrow
    let patch = Diff.diff j1 j2

    printfn "difference: %s" (patch |> Json.serialize)

    let j3 = Diff.patch patch j1 |> JsonResult.getOrThrow |> Json.format

    printfn "patched original: %s" j3
    0 // return an integer exit code
