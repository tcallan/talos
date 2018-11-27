namespace Talos.Dynamic
open Newtonsoft.Json
open Chiron
open Microsoft.AspNetCore.JsonPatch
module Json = Inference.Json

module Diff =
    let private jsonSerializerSettings =
        JsonSerializerSettings(
            DateParseHandling = DateParseHandling.DateTimeOffset,
            DateTimeZoneHandling = DateTimeZoneHandling.RoundtripKind)

    let internal dynamicToJson o =
        JsonConvert.SerializeObject(o, jsonSerializerSettings)
        |> Json.parse
        |> JsonResult.getOrThrow

    let internal dynamicFromJson<'T> j =
        Json.format j
        |> fun json -> JsonConvert.DeserializeObject<'T>(json, jsonSerializerSettings)

    [<CompiledName("TalosPatchToJsonPatch")>]
    let talosPatchToJsonPatch (p : Talos.Patch.Patch) : JsonPatchDocument =
        p
        |> Json.encode
        |> dynamicFromJson<JsonPatchDocument>

    [<CompiledName("JsonPatchToTalosPatch")>]
    let jsonPatchToTalosPatch (p : JsonPatchDocument) : Talos.Patch.Patch =
        p
        |> dynamicToJson
        |> Json.decode
        |> JsonResult.getOrThrow

    [<CompiledName("Diff")>]
    let diff src dst =
        let src = dynamicToJson src
        let dst = dynamicToJson dst

        Talos.Diff.diff src dst

    [<CompiledName("Patch")>]
    let patch p (src : 'T) =
        let src = dynamicToJson src

        Talos.Diff.patch p src
        |> JsonResult.getOrThrow
        |> dynamicFromJson<'T>

    [<CompiledName("DiffToJsonPatch")>]
    let diffToJsonPatch src dst =
        diff src dst |> talosPatchToJsonPatch

    [<CompiledName("PatchWithJsonPatch")>]
    let patchWithJsonPatch p src =
        patch (jsonPatchToTalosPatch p) src

    [<CompiledName("PatchWithJsonPatches")>]
    let patchWithJsonPatches ps (src : 'T) =
        let src' = dynamicToJson src

        ps
        |> Seq.map jsonPatchToTalosPatch
        |> Seq.fold (fun src p -> Talos.Diff.patch p src |> JsonResult.getOrThrow) src'
        |> dynamicFromJson<'T>
