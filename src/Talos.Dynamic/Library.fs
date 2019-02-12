namespace Talos.Dynamic
open Newtonsoft.Json
open Chiron
open Microsoft.AspNetCore.JsonPatch
open System.Runtime.InteropServices
module Json = Inference.Json

[<AllowNullLiteral>]
type DiffSettings() = 
    member val SerializerSettings =
        JsonSerializerSettings(
            DateParseHandling = DateParseHandling.DateTimeOffset,
            DateTimeZoneHandling = DateTimeZoneHandling.RoundtripKind)
        with get, set

    member val IgnoreErrors = false with get, set

type Diff() =
    static member private DynamicToJson (serializerSettings : JsonSerializerSettings) o =
        JsonConvert.SerializeObject(o, serializerSettings)
        |> Json.parse
        |> JsonResult.getOrThrow

    static member private DynamicFromJson<'T> (serializerSettings : JsonSerializerSettings) j =
        Json.format j
        |> fun json -> JsonConvert.DeserializeObject<'T>(json, serializerSettings)

    static member private TalosPatchToJsonPatch serializerSettings (p : Talos.Patch.Patch) : JsonPatchDocument =
        p
        |> Json.encode
        |> Diff.DynamicFromJson<JsonPatchDocument> serializerSettings

    static member private JsonPatchToTalosPatch serializerSettings (p : JsonPatchDocument) : Talos.Patch.Patch =
        p
        |> Diff.DynamicToJson serializerSettings
        |> Json.decode
        |> JsonResult.getOrThrow

    static member private Diff (settings : DiffSettings) src dst =
        let src = Diff.DynamicToJson (settings.SerializerSettings) src
        let dst = Diff.DynamicToJson (settings.SerializerSettings) dst

        Talos.Diff.diff src dst

    static member private Patch (settings : DiffSettings) p (src : 'T) =
        let src = Diff.DynamicToJson (settings.SerializerSettings) src
        let patch = if settings.IgnoreErrors then Talos.Diff.patchForgiving else Talos.Diff.patch

        patch p src
        |> JsonResult.getOrThrow
        |> Diff.DynamicFromJson<'T> (settings.SerializerSettings)

    static member DiffToJsonPatch (src, dst, [<Optional; DefaultParameterValue(value = (null : DiffSettings))>]settings : DiffSettings) =
        let settings = if isNull settings then DiffSettings() else settings
        Diff.Diff settings src dst |> Diff.TalosPatchToJsonPatch (settings.SerializerSettings)

    static member PatchWithJsonPatch (p : JsonPatchDocument, src, [<Optional; DefaultParameterValue(value = (null : DiffSettings))>]settings : DiffSettings) =
        let settings = if isNull settings then DiffSettings() else settings
        Diff.Patch settings (Diff.JsonPatchToTalosPatch (settings.SerializerSettings) p) src

    static member PatchWithJsonPatches ((ps : seq<JsonPatchDocument>), src : 'T, [<Optional; DefaultParameterValue(value = (null : DiffSettings))>]settings : DiffSettings) : 'T =
        let settings = if isNull settings then DiffSettings() else settings
        let src' = Diff.DynamicToJson (settings.SerializerSettings) src
        let patch = if settings.IgnoreErrors then Talos.Diff.patchForgiving else Talos.Diff.patch

        ps
        |> Seq.map (Diff.JsonPatchToTalosPatch (settings.SerializerSettings))
        |> Seq.fold (fun src p -> patch p src |> JsonResult.getOrThrow) src'
        |> Diff.DynamicFromJson (settings.SerializerSettings) 
