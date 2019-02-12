namespace Talos
open System.Collections.Immutable
open System

module List =
    let rec private segmentWithImpl p = function
        | [] -> []
        | x::xs ->
            let ys = List.takeWhile (p x) xs
            let zs = List.skipWhile (p x) xs
            (x::ys) :: segmentWithImpl p zs

    let segmentWith (predicate : 'T -> 'T -> bool) (source : 'T list) : 'T list list = 
        segmentWithImpl predicate source

    let insert v i (l : 'a list) =
        let l = l.ToImmutableList()
        let i = Math.Min(l.Count, Math.Max(0, i))
        l.Insert(i, v)
        |> List.ofSeq

    let remove i (l : 'a list) =
        if i < 0 || i >= List.length l
        then l
        else
            l.ToImmutableList().RemoveAt(i)
            |> List.ofSeq

    let update v i (l : 'a list) =
        if i < 0 || i >= List.length l
        then l
        else
            l.ToImmutableList().SetItem(i, v)
            |> List.ofSeq