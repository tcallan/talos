namespace Talos

open Pointer
open Chiron
open Chiron.Operators
module Json = Inference.Json
module E = Json.Encode
module D = Json.Decode
module EI = Inference.Json.Encode
module DI = Inference.Json.Decode


module Patch =

    type AddOp = {
        ChangePointer : Pointer
        ChangeValue : Json
    }

    type CpyOp = {
        ChangePointer : Pointer
        FromPointer : Pointer
    }

    type MovOp = {
        ChangePointer : Pointer
        FromPointer : Pointer
    }

    type RemOp = {
        ChangePointer : Pointer
    }

    type RepOp = {
        ChangePointer : Pointer
        ChangeValue : Json
    }

    type TstOp = {
        ChangePointer : Pointer
        ChangeValue : Json
    }

    type Operation =
        | Add of AddOp
        | Cpy of CpyOp
        | Mov of MovOp
        | Rem of RemOp
        | Rep of RepOp
        | Tst of TstOp

        static member ToJson(x : Operation) : Json =
            E.jsonObjectWith (fun x jObj ->
                match x with
                | Add a ->
                    jObj
                    |> EI.required "op" "add"
                    |> EI.required "path" a.ChangePointer
                    |> EI.required "value" a.ChangeValue
                | Cpy c ->
                    jObj
                    |> EI.required "op" "copy"
                    |> EI.required "path" c.ChangePointer
                    |> EI.required "from" c.FromPointer
                | Mov m ->
                    jObj
                    |> EI.required "op" "move"
                    |> EI.required "path" m.ChangePointer
                    |> EI.required "from" m.FromPointer
                | Rem r ->
                    jObj
                    |> EI.required "op" "remove"
                    |> EI.required "path" r.ChangePointer
                | Rep r ->
                    jObj
                    |> EI.required "op" "replace"
                    |> EI.required "path" r.ChangePointer
                    |> EI.required "value" r.ChangeValue
                | Tst t ->
                    jObj
                    |> EI.required "op" "test"
                    |> EI.required "path" t.ChangePointer
                    |> EI.required "value" t.ChangeValue
            ) x

        static member FromJson(_x : Operation) : Decoder<Json, Operation> =
            let assertOp (op : string) =
                DI.required "op"
                >=> D.assertThat ((=) op) "wrong op"
                >-> ignore

            let add = jsonDecoder {
                do! assertOp "add"
                let! p = DI.required "path"
                let! v = DI.required "value"
                return Add { ChangePointer = p; ChangeValue = v }
            }
            let cpy = jsonDecoder {
                do! assertOp "copy"
                let! p = DI.required "path"
                let! f = DI.required "from"
                return Cpy { ChangePointer = p; FromPointer = f }
            }
            let mov = jsonDecoder {
                do! assertOp "move"
                let! p = DI.required "path"
                let! f = DI.required "from"
                return Mov { ChangePointer = p; FromPointer = f }
            }
            let rem = jsonDecoder {
                do! assertOp "remove"
                let! p = DI.required "path"
                return Rem { ChangePointer = p }
            }
            let rep = jsonDecoder {
                do! assertOp "replace"
                let! p = DI.required "path"
                let! v = DI.required "value"
                return Rep { ChangePointer = p; ChangeValue = v }
            }
            let tst = jsonDecoder {
                do! assertOp "test"
                let! p = DI.required "path"
                let! v = DI.required "value"
                return Tst { ChangePointer = p; ChangeValue = v }
            }
            D.oneOf [ add; cpy; mov; rem; rep; tst ]

    let changePointer = function
        | Add { ChangePointer = c } -> c
        | Cpy { ChangePointer = c } -> c
        | Mov { ChangePointer = c } -> c
        | Rem { ChangePointer = c } -> c
        | Rep { ChangePointer = c } -> c
        | Tst { ChangePointer = c } -> c

    type Patch =
        { PatchOperations : Operation list }

        static member ToJson(x : Patch) : Json =
            Json.encode x.PatchOperations

        static member FromJson(_ : Patch) : Decoder<Json, Patch> =
            D.listWith Json.decode >> JsonResult.map (fun a -> { PatchOperations = a })

    [<CompiledName("ModifyPointer")>]
    let modifyPointer f = function
        | Add o -> Add { o with ChangePointer = f o.ChangePointer }
        | Cpy o -> Cpy { o with ChangePointer = f o.ChangePointer; FromPointer = f o.FromPointer }
        | Mov o -> Mov { o with ChangePointer = f o.ChangePointer; FromPointer = f o.FromPointer }
        | Rem o -> Rem { o with ChangePointer = f o.ChangePointer }
        | Rep o -> Rep { o with ChangePointer = f o.ChangePointer }
        | Tst o -> Tst { o with ChangePointer = f o.ChangePointer }
