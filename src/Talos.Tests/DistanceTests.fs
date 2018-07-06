module DistanceTests

open Xunit
open FsCheck
open FsCheck.Xunit
open Talos.Distance
open System.Diagnostics
open System

type C<'a> =
    | I of int * 'a
    | D of int * 'a
    | S of int * 'a * 'a

let str : Params<char, C<char>, int> = {
    Equivalent = (=)
    Delete = fun i v -> D (i, v)
    Insert = fun i v -> I (i, v)
    Substitute = fun i v v' -> S (i, v, v')
    Cost = fun _ -> 1
    PositionOffset = function
        | D _ -> 0
        | _ -> 1
}

let splitAt i (l : string) =
    (l.[0..i-1], l.[i..])

let tail (s : string) =
    s.Substring(1)

let rec applyC changes l =
    match changes with
    | [] -> l
    | D(i, _) :: r ->
        let (h, t) = splitAt i l
        let l' = h + tail t
        applyC r l'
    | I(i, a) :: r ->
        let (h, t) = splitAt i l
        let l' = h + string a + t
        applyC r l'
    | S(i, _, a) :: r ->
        let (h,t) = splitAt i l
        let l' = h + string a + tail t
        applyC r l'


[<Property>]
let ``Patch from identical documents should be empty`` (NonNull (s : string)) =
    let (cost, patch) = leastChanges str s s
    cost = 0 && patch |> Seq.toList = List.empty

[<Fact>]
let ``Identical documents sample 1`` () =
    Assert.Equal((0, Seq.empty), leastChanges str "foo" "foo")

[<Fact>]
let ``Identical documents sample 2`` () =
    Assert.Equal((0, Seq.empty), leastChanges str "" "")

[<Fact>]
let ``Identical documents performance sanity check`` () =
    let longString = String.replicate 100 "a"
    let sw = Stopwatch.StartNew()
    leastChanges str longString longString |> ignore
    sw.Stop()
    Assert.True(sw.Elapsed < TimeSpan.FromMilliseconds(200.), sprintf "too slow %A" sw.Elapsed)

[<Property>]
let ``Patch from anything to empty should be all deletes`` (NonNull (s : string)) =
    let (_, patch) = leastChanges str s ""
    patch |> Seq.forall (function | D _ -> true | _ -> false) 

[<Property>]
let ``Patch from empty to anything should be all inserts`` (NonNull (s : string)) =
    let (_, patch) = leastChanges str "" s
    patch |> Seq.forall (function | I _ -> true | _ -> false)

[<Fact>]
let ``Canned sample 1`` () =
    let result = leastChanges str "sitting" "kitten"
    let expected = (3, seq [S(0, 's', 'k'); S(4, 'i', 'e'); D(6, 'g')])
    Assert.Equal(expected, result)

[<Fact>]
let ``Canned sample 2`` () =
    let result = leastChanges str "kitten" "sitting"
    let expected = (3, seq [S(0, 'k', 's'); S(4, 'e', 'i'); I(6, 'g')])
    Assert.Equal(expected, result)

[<Fact>]
let ``Canned sample 3`` () =
    let result = leastChanges str "saturday" "sunday"
    let expected = (3, seq [D(1, 'a'); D(1, 't'); S(2, 'r', 'n')])
    Assert.Equal(expected, result)

[<Fact>]
let ``Canned sample 4`` () =
    let result = leastChanges str "sunday" "saturday"
    let expected = (3, seq [I(1, 'a'); I(2, 't'); S(4, 'n', 'r')])
    Assert.Equal(expected, result)

[<Property>]
let ``leastChanges + applyC = id`` (NonNull (ss : string)) (NonNull (tt : string)) =
    tt = applyC (leastChanges str ss tt |> snd |> Seq.toList) ss