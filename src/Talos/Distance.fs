namespace Talos

module Distance =
    // NOTE: using an array internally for performance reasons
    // Changing the following to reference Seq<'a> or List<'a> demonstrates that the code correctly
    // treats the collection as immutable, but with a performance hit (particularly for Seq<'a>).
    type private Matrix<'a> = 'a[]

    module private Matrix =
        let append = Array.append
        let empty = Array.empty
        let length = Array.length
        let singleton = Array.singleton
        let tryItem = Array.tryItem

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
            match m |> Matrix.tryItem i with
            | Some result -> result
            | None -> failwith <| sprintf "Unable to get (%i, %i) from changes" x y

        let position ops =
            ops |> Seq.sumBy (fun op -> op |> Option.map (p.PositionOffset) |> Option.defaultValue 1)

        let ctr (v : Matrix<_>) =
            match (v |> Matrix.length) /% lenY with
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
                        let c =p.Substitute (position <| snd tl) s d
                        (p.Cost c + fst tl, Some c :: snd tl)

                    List.minBy (fst) [c1; c2; c3]

        let mutable result = Matrix.empty

        for _ in 0..(lenN - 1) do
            let next = ctr result
            result <- (Matrix.append result (Matrix.singleton next))

        result

    [<CompiledName("LeastChanges")>]
    let leastChanges p src dst =
        let (cost, ops) = rawChanges p src dst |> Seq.last
        (cost, ops |> Seq.filter (Option.isSome) |> Seq.map (Option.get) |> Seq.rev)
