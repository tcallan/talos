namespace Talos

open Chiron
open Patch
open Pointer
open Distance
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

module Diff =
    let rec private valueSize value = 
        match value with
        | Object o ->
            Seq.sumBy valueSize (JsonObject.toPropertyList o |> List.map snd)
        | Array a -> 
            Seq.sumBy valueSize a
        | _ -> 1

    let private operationCost = function
        | Add op -> valueSize (op.ChangeValue)
        | Rem _ -> 1
        | Rep op -> valueSize (op.ChangeValue)
        | Mov _ -> 1
        | Cpy _ -> 1
        | Tst op -> valueSize (op.ChangeValue)

    let private ins p v = [Add {ChangePointer = p; ChangeValue = v}]
    let private del p _ = [Rem {ChangePointer = p}]
    let private rep p v = [Rep {ChangePointer = p; ChangeValue = v}]

    let private check b v =
        if b then List.empty else v

    let private filtersFor p pointers =
        let filter op =
            pointers
            |> List.filter (fun (Pointer p) -> p |> List.tryHead = op)
            |> List.map (fun (Pointer p) -> p |> List.tail |> Pointer)

        match p with
        | [] ->
            pointers
        | [op] ->
            Some op |> filter
        | _ ->
            p |> List.tryLast |> filter

    let rec private workObject (Pointer p) filters o1 o2 =
        let updatePointer (Pointer pp) =
            Pointer (p @ pp)

        let myFilters =
            filtersFor p filters

        let k1 = JsonObject.toPropertyList o1 |> List.map fst
        let k2 = JsonObject.toPropertyList o2 |> List.map fst

        let get key map =
            Optics.get (Optics.JsonObject.key_ key) map
            |> JsonResult.getOrThrow

        let delKeys = k1 |> Seq.except k2
        let deletions = delKeys |> Seq.collect (fun k -> del (Pointer [OKey k]) (get k o1))

        let insKeys = k2 |> Seq.except k1
        let insertions = insKeys |> Seq.collect (fun k -> ins (Pointer [OKey k]) (get k o2))

        let chgKeys = k1 |> Set.ofSeq |> Set.intersect (Set.ofSeq k2)
        let changes =
            chgKeys |> Seq.collect
                (fun k -> worker (Pointer [OKey k]) (myFilters) (get k o1) (get k o2))

        Seq.concat [deletions; insertions; changes]
        |> Seq.map (modifyPointer updatePointer)
        |> Seq.toList

    and private workArray (Pointer p) filters a1 a2 =
        let updatePointer (Pointer pp) =
            Pointer (p @ pp)

        let related o1 o2 =
            let (Pointer p1) = (changePointer o1)
            let (Pointer p2) = (changePointer o2)
            match p1, p2 with
            | [_], [_] -> false
            | i1::_, i2::_ -> i1 = i2
            | _ -> failwith "Something went wrong"

        let pos = function
            | Rem _ -> 0
            | Add {ChangePointer = Pointer p} when (Seq.length p = 1) -> 1
            | Add _ -> 0
            | Rep {ChangePointer = Pointer p} when (Seq.length p = 1) -> 1
            | Rep _ -> 0
            | Cpy {ChangePointer = Pointer p} when (Seq.length p = 1) -> 1
            | Cpy _ -> 0
            | Mov {ChangePointer = Pointer p} when (Seq.length p = 1) -> 1
            | Mov _ -> 0
            | Tst _ -> 0

        let adv ops = 
            match ops |> Seq.toList with
            | [op] ->
                let (Pointer pp) = changePointer op
                if pp |> Seq.length = 1
                then pos op
                else 1
            | _ -> 1

        let myFilters = filtersFor p filters

        let param : Params<Json, Operation list, int> = {
            Equivalent = (=)
            Delete = fun i -> del (Pointer [AKey i])
            Insert = fun i -> ins (Pointer [AKey i])
            Substitute = fun i -> worker (Pointer [AKey i]) myFilters
            Cost = Seq.sumBy operationCost
            PositionOffset = fun o -> o |> List.segmentWith related |> Seq.sumBy adv
        }

        leastChanges param a1 a2
        |> snd |> List.concat
        |> Seq.map (modifyPointer updatePointer)
        |> Seq.toList

    and private worker p filters v1 v2 =
        match (v1, v2) with
        | (Null, Null) ->
            List.empty
        | (Bool b1, Bool b2) ->
            check (b1 = b2) <| rep p v2
        | (Number n1, Number n2) ->
            check (n1 = n2) <| rep p v2
        | (String s1, String s2) ->
            check (s1 = s2) <| rep p v2
        | (Array a1, Array a2) ->
            if filters |> List.contains p
            then
                check (a1 = a2) <| rep p v2
            else
                workArray p filters a1 a2
        | (Object o1, Object o2) ->
            if filters |> List.contains p
            then
                check (o1 = o2) <| rep p v2
            else
                workObject p filters o1 o2
        | _ -> rep p v2

    [<CompiledName("DiffFiltered")>]
    let diffFiltered filter v v' =
        {PatchOperations = (worker Pointer.empty filter v v')}

    let private quoteToFilter (quote : Expr) : Path =
        let rec getPath path quote =
            match quote with
            | Lambda(_, body) ->
                getPath path body
            | PropertyGet(prev, info, _) ->
                let path = (OKey info.Name :: path)
                match prev with
                | Some prev -> getPath path prev
                | None -> path
            | _ ->
                path

        getPath [] quote

    let private quotesToFilters qfilters =
        qfilters
        |> Seq.map (quoteToFilter >> Pointer)
        |> Seq.toList

    [<CompiledName("DiffFilteredQ")>]
    let diffFilteredQ qfilters v v' =
        diffFiltered (quotesToFilters qfilters) v v'

    [<CompiledName("Diff")>]
    let diff v v' = diffFiltered List.empty v v'

    type private JsonResultBuilder() =
        member __.Bind(x, f) =
            x |> JsonResult.bind f

        member __.Return(x) =
            x |> JsonResult.pass

        member __.ReturnFrom(x) = x

    let private jsonResult = new JsonResultBuilder()

    let private updateL i f l =
        let a = List.tryItem i l
        let a' = f a
        match a, a' with
        | None, JPass None ->
             JsonResult.pass l
        | Some _, JPass None ->
            List.remove i l |> JsonResult.pass
        | None, JPass (Some n) ->
            List.insert n i l |> JsonResult.pass
        | Some _, JPass (Some n) ->
            List.update n i l |> JsonResult.pass
        | _, JFail e -> JsonResult.fail e

    let private updateM k f m =
        match JsonObject.tryFind k m |> f with
        | JPass None -> JsonObject.remove k m |> JsonResult.pass
        | JPass (Some v) -> JsonObject.upsert k v m |> JsonResult.pass
        | JFail e -> JFail e

    let inline private cannot op ty ix p : JsonResult<'c> =
        sprintf "Cannot %s missing %s member at index %A in pointer %A" op ty ix (formatPointer p)
        |> JsonResult.invalidJson

    let private applyAdd pointer =
        let rec go pointer v v' =
            match (pointer, v, v') with
            | Pointer [], v, _ ->
                JsonResult.pass v
            | Pointer [AKey i], v', Array v ->
                List.insert v' i v
                |> Array
                |> JsonResult.pass
            | Pointer (AKey i :: path), v', Array v ->
                let fn : Json option -> JsonResult<Json option> = function
                    | None -> cannot "insert" "array" i pointer
                    | Some d -> go (Pointer path) v' d |> JsonResult.map Some
                updateL i fn v
                |> JsonResult.map Array
            | Pointer [OKey n], v', (Object m) ->
                JsonObject.add n v' m
                |> Object
                |> JsonResult.pass
            | Pointer (OKey n :: path), v', (Object o) ->
                let fn = function
                    | None -> cannot "insert" "object" n pointer
                    | Some d -> go (Pointer path) v' d |> JsonResult.map Some

                updateM n fn o
                |> JsonResult.map Object
            | Pointer (OKey "-" :: path), v', (Array v as array) ->
                go (Pointer (AKey (List.length v) :: path)) v' array
            | path, _, v ->
                pointerFailure path v

        go pointer

    let private applyRem (Pointer path as from) =
        let rec go path v =
            match path, v with
            | [], _ -> Null |> JsonResult.pass
            | [AKey i], Array v ->
                let fn = function
                    | None -> cannot "delete" "array" i from
                    | Some _ -> None |> JsonResult.pass

                updateL i fn v 
                |> JsonResult.map Array
            | AKey i::path, Array v ->
                let fn = function
                    | None -> cannot "travers" "array" i from
                    | Some o -> go path o |> JsonResult.map Some

                updateL i fn v
                |> JsonResult.map Array
            | [OKey n], (Object m) ->
                let fn = function
                    | None -> cannot "delete" "object" n from
                    | Some _ -> None |> JsonResult.pass

                updateM n fn m
                |> JsonResult.map Object
            | OKey n::path, Object m ->
                let fn = function
                    | None -> cannot "traverse" "object" n from
                    | Some o -> go path o |> JsonResult.map Some

                updateM n fn m
                |> JsonResult.map Object
            | OKey "-"::path, (Array v as array) ->
                go (AKey (List.length v) :: path) array
            | _, value ->
                pointerFailure from value

        go path

    let private applyRep from v doc =
        applyRem from doc
        |> JsonResult.bind (applyAdd from v)

    let private applyMov path from doc = jsonResult {
        let! v = get from doc
        let! v' = applyRem from doc
        return! applyAdd path v v'
    }

    let private applyCpy path from doc = jsonResult {
        let! v = get from doc
        return! applyAdd path v doc
    }

    let private applyTst path v doc = jsonResult {
        let! v' = get path doc
        if v = v'
        then return doc
        else return! sprintf "Element at %A fails test" (formatPointer path) |> JsonResult.invalidJson
    }

    [<CompiledName("ApplyOperation")>]
    let applyOperation op json =
        match op with
        | Add {ChangePointer=path; ChangeValue=v} ->
            applyAdd path v json
        | Rem {ChangePointer=path} ->
            applyRem path json
        | Rep {ChangePointer=path; ChangeValue=v} ->
            applyRep path v json
        | Tst {ChangePointer=path; ChangeValue=v} ->
            applyTst path v json
        | Cpy {ChangePointer=path; FromPointer=from} ->
            applyCpy path from json
        | Mov {ChangePointer=path; FromPointer=from} ->
            applyMov path from json

    let private operationFolder state op = (applyOperation >> JsonResult.bind) op state

    [<CompiledName("Patch")>]
    let patch p v =
        match p with
        | {PatchOperations = []} -> JPass v
        | {PatchOperations = ops} ->
            List.fold (operationFolder) (JPass v) ops