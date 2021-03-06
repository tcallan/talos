namespace Talos

module Distance =
    let private (/%) x y = (x / y, x % y)

    let private (|Pred|) x = x - 1

    type Params<'v, 'o, 'c> = {
        Equivalent : 'v -> 'v -> bool
        Delete : int -> 'v -> 'o
        Insert : int -> 'v -> 'o
        Substitute : int -> 'v -> 'v -> 'o
        Cost : 'o -> 'c
        PositionOffset : 'o -> int
    }

    [<CompiledName("RawChanges")>]
    let rawChanges p src dst =
        let lenX = 1 + (dst |> Seq.length)
        let lenY = 1 + (src |> Seq.length)
        let lenN = lenX * lenY

        let ix x y = (x * lenY) + y

        let get m x y =
            let i = ix x y
            match m |> Array.tryItem i with
            | Some result -> result
            | None -> failwith <| sprintf "Unable to get (%i, %i) from changes" x y

        let position ops =
            ops |> Seq.sumBy (fun op -> op |> Option.map (p.PositionOffset) |> Option.defaultValue 1)

        let ctr ix (v : array<_>) =
            match ix /% lenY with
            | (0, 0) ->
                (0, List.empty)
            | (0, Pred y) ->
                let o = p.Delete 0 (src |> Seq.item y)
                let (pc, po) = get v 0 y
                (p.Cost o + pc, Some(o) :: po)
            | (Pred x, 0) ->
                let o = p.Insert x (dst |> Seq.item x)
                let (pc, po) = get v x 0
                (p.Cost o + pc, Some(o) :: po)
            | (Pred x, Pred y) ->
                let s = src |> Seq.item y
                let d = dst |> Seq.item x
                let tl = get v x y
                let top = get v (x + 1) y
                let left = get v x (y + 1)
                if p.Equivalent s d
                then (fst tl, None :: (snd tl))
                else
                    let c1 =
                        let c = p.Delete (position <| snd top) s
                        (p.Cost c + fst top, Some c :: snd top)
                    let c2 =
                        let c = p.Insert (position <| snd left) d
                        (p.Cost c + fst left, Some c :: snd left)
                    let c3 =
                        let c = p.Substitute (position <| snd tl) s d
                        (p.Cost c + fst tl, Some c :: snd tl)

                    List.minBy (fst) [ c1; c2; c3 ]

        let result = Array.replicate lenN (0, List.empty)

        for ix in 0..(lenN - 1) do
            result.[ix] <- ctr ix result

        result

    let private shortcircuit p src dst =
        let cmp a b = if p.Equivalent a b then 0 else 1
        match Seq.compareWith cmp src dst with
        | 0 -> true
        | _ -> false

    [<CompiledName("LeastChanges")>]
    let leastChanges p src dst =
        if shortcircuit p src dst
        then (0, Seq.empty)
        else 
            let (cost, ops) = rawChanges p src dst |> Seq.last
            (cost, ops |> Seq.filter (Option.isSome) |> Seq.map (Option.get) |> Seq.rev)
