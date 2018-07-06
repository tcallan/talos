namespace Talos

open System
open Chiron
module E = Json.Encode
module D = Json.Decode
module Json = Inference.Json

module rec Pointer =
    type Key =
        | OKey of string
        | AKey of int

    with
        static member ToJson (x : Key) : Json =
            match x with
            | OKey n -> E.string n
            | AKey i -> E.number (string i)

        static member FromJson (_ : Key) : Decoder<Json, Key> =
            D.either
                (D.string >> JsonResult.map OKey)
                (D.int >> JsonResult.map AKey)

    type Path = Key list

    let private escape = function
        | '~' -> "~0"
        | '/' -> "~1"
        | c -> c.ToString()

    let private formatPath = function
        | AKey i -> i |> string
        | OKey o ->
            o |> String.collect escape

    let formatPointer = function
        | Pointer [] -> ""
        | Pointer path ->
            "/" + String.concat "/" (List.map formatPath path)


    type Pointer =
        | Pointer of Path
    let private toKey s =
        match Int32.TryParse(s) with
        | (true, i) ->
            if i |> string = s
            then AKey i
            else OKey s
        | (false, _) -> OKey s

    let private unescape (s : string) =
        s.Replace("~1", "/").Replace("~0", "~")

    let private parsePointer (s : string) =
        s.Split [| '/' |]
        |> Seq.skip 1
        |> Seq.map (unescape >> toKey)
        |> Seq.toList
        |> Pointer
        |> JsonResult.pass

    type Pointer with
        static member ToJson (x : Pointer) : Json =
            E.string (formatPointer x)

        static member FromJson (_ : Pointer) : Decoder<Json, Pointer> =
            (fun x -> D.string x |> JsonResult.bind parsePointer)

    let toOptic p : Optics.Lens<Json, Json> =
        let id = JPass, (fun a _ -> a)

        let index i : Optics.Lens<Json list, Json> =
            List.item i >> JsonResult.pass, (fun _ l -> l)

        let jsonIndex i = Optics.compose Optics.Json.Array_ (index  i)

        let rec getLens p =
            match p with
            | Pointer [] ->
                id
            | Pointer (AKey i :: path) ->
                Optics.compose (jsonIndex i) (getLens (Pointer path))
            | Pointer (OKey n :: path) ->
                Optics.compose (Optics.Json.Property_ n) (getLens (Pointer path))
        getLens p

    let get p v =
        Optics.get (toOptic p) v

    let pointerFailure pointer value =
        match pointer with
        | Pointer [] ->
            JsonResult.raise <| exn "Cannot follow empty pointer"
        | Pointer (key::_ as path) ->
            let ty =
                match key with 
                | AKey _ -> "array"
                | OKey _ -> "object"

            sprintf "Cannot follow pointer %A. Expected %s but got %A." path ty value
            |> JsonResult.invalidJson

    let empty = Pointer []