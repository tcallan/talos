namespace Talos.Dynamic
open Newtonsoft.Json
open Chiron
open Microsoft.AspNetCore.JsonPatch
open System.Linq.Expressions
open System
open System.Reflection
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
    let private tryCast<'a> : obj -> 'a option = function
        | :? 'a as x -> Some(x)
        | _ -> None

    let expressionToPointer (expr : Expression<Func<_, _>>) =
        let me =
            match expr.Body.NodeType with
            | ExpressionType.Convert | ExpressionType.ConvertChecked ->
                expr.Body
                |> tryCast<UnaryExpression>
                |> Option.bind (fun ue -> ue.Operand |> tryCast<MemberExpression>)
            | _ ->
                expr.Body |> tryCast<MemberExpression>
        
        let getNameFromConstructor (attr : CustomAttributeData) =
            attr.ConstructorArguments
            |> Seq.tryHead
            |> Option.bind (fun arg -> arg.Value |> tryCast<string>)

        let getNameFromNamedArgs (attr : CustomAttributeData) _ =
            attr.NamedArguments
            |> Seq.tryFind (fun arg -> arg.MemberName = "PropertyName")
            |> Option.bind (fun arg -> arg.TypedValue.Value |> tryCast<string>)

        let getPropertyName (mi : MemberInfo) =
            mi.CustomAttributes
            |> Seq.tryFind (fun attr -> attr.AttributeType = typeof<JsonPropertyAttribute>)
            |> Option.bind (fun attr -> getNameFromConstructor attr
                                        |> Option.orElseWith (getNameFromNamedArgs attr))
            |> Option.defaultValue mi.Name

        let rec getPath path (me : MemberExpression option) =
            match me with
            | Some x ->
                getPath (getPropertyName x.Member :: path) (x.Expression |> tryCast<MemberExpression>)
            | None ->
                path

        getPath [] me
        |> List.map (Talos.Pointer.OKey)
        |> Talos.Pointer.Pointer

    let expressionsToPointers exprs =
        exprs
        |> Seq.map expressionToPointer
        |> Seq.toList

    type Differ<'a> internal (filters) =
        member __.Diff (src : 'a, dst: 'a) =
            let src = dynamicToJson src
            let dst = dynamicToJson dst

            Talos.Diff.diffFiltered filters src dst

        member this.DiffToJsonPatch (src : 'a, dst : 'a) =
            this.Diff(src, dst) |> talosPatchToJsonPatch

    /// Construct a diff with special rules
    type DifferBuilder<'a>() =
        let mutable filters = []

        /// Add a property to exclude from nested object and array handling.
        /// When diffing, any difference in any value under the property pointed to by filter will
        /// trigger the entire property to be replaced rather than producing more targeted changes.
        member this.WithFilter(filter : Expression<Func<'a, obj>>) =
            filters <- expressionToPointer filter :: filters
            this

        member __.Build () =
            Differ<'a> (filters)
