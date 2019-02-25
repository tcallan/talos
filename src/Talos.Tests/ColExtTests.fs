module ColExtTests

open Xunit
open Talos
open Swensen.Unquote

let pred x y =
    (x |> Seq.tryHead) = (y |> Seq.tryHead)

[<Fact>]
let ``segmentWith simple``() =
    let start = [ "aa"; "ab"; "ac"; "ba"; "bc"; "bd"; "ca"; "cb"; "cc" ]

    let result = List.segmentWith pred start

    result =! [ [ "aa"; "ab"; "ac" ]; [ "ba"; "bc"; "bd" ]; [ "ca"; "cb"; "cc" ] ]

[<Fact>]
let ``segmentWith repeated items``() =
    let start = [ "aa"; "ab"; "ba"; "bb"; "aa"; "ab" ]

    let result = List.segmentWith pred start

    result =! [ [ "aa"; "ab" ]; [ "ba"; "bb" ]; [ "aa"; "ab" ] ]

[<Fact>]
let ``segmentWith empty``() =
    let start = []
    let result = List.segmentWith pred start

    result =! []

[<Fact>]
let ``insert at beginning``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.insert "z" 0 start

    result =! [ "z"; "a"; "b"; "c" ]

[<Fact>]
let ``insert before beginning``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.insert "z" -1 start

    result =! [ "z"; "a"; "b"; "c" ]


[<Fact>]
let ``insert in middle``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.insert "z" 1 start

    result =! [ "a"; "z"; "b"; "c" ]

[<Fact>]
let ``insert at end``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.insert "z" 3 start

    result =! [ "a"; "b"; "c"; "z" ]

[<Fact>]
let ``insert past end``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.insert "z" 4 start

    result =! [ "a"; "b"; "c"; "z" ]

[<Fact>]
let ``remove at beginning``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.remove 0 start

    result =! [ "b"; "c" ]

[<Fact>]
let ``remove before beginning``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.remove -1 start

    result =! [ "a"; "b"; "c" ]

[<Fact>]
let ``remove in middle``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.remove 1 start

    result =! [ "a"; "c" ]

[<Fact>]
let ``remove at end``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.remove 2 start

    result =! [ "a"; "b" ]

[<Fact>]
let ``remove past end``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.remove 3 start

    result =! [ "a"; "b"; "c" ]


[<Fact>]
let ``update at beginning``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.update "z" 0 start

    result =! [ "z"; "b"; "c" ]

[<Fact>]
let ``update before beginning``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.update "z" -1 start

    result =! [ "a"; "b"; "c" ]

[<Fact>]
let ``update in middle``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.update "z" 1 start

    result =! [ "a"; "z"; "c" ]

[<Fact>]
let ``update at end``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.update "z" 2 start

    result =! [ "a"; "b"; "z" ]

[<Fact>]
let ``update past end``() =
    let start = [ "a"; "b"; "c" ]
    let result = List.update "z" 3 start

    result =! [ "a"; "b"; "c" ]
