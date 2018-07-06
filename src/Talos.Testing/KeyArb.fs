module KeyArb

open FsCheck
open Talos.Pointer

let akeyGen =
    Arb.generate<PositiveInt>
    |> Gen.map (fun (PositiveInt i) -> AKey i)

let okeyGen =
    Arb.generate<NonNull<string>>
    |> Gen.map (fun (NonNull s) -> OKey s)

let keyGen =
    Gen.frequency
        [
            1, akeyGen
            9, okeyGen
        ]

let keyArb = { new Arbitrary<Key>() with
    override __.Generator = keyGen }

type Arbitrary = Arbitrary with
    static member Key () = keyArb