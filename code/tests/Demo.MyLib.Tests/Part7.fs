namespace Demo.MyLib.Tests

module Part7 =

    open Swensen.Unquote
    open Xunit

    type Tree<'a> =
        | Leaf of 'a
        | Node of Tree<'a> * Tree<'a>

    let rec map f =
        fun tree ->
            match tree with
            | Leaf v -> Leaf(f v)
            | Node(l, r) -> Node(map f l, map f r)

    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    module Part7_1 =

        type WithCount<'v> = WithCount of (int -> 'v * int)

        let run (WithCount f) (count: int) = f count

        let rec index =
            function
            | Leaf v -> WithCount(fun count -> (Leaf(v, count), count + 1))
            | Node(l, r) ->
                WithCount(fun count ->
                    let li, lc = run (index l) count
                    let ri, rc = run (index r) lc
                    Node(li, ri), rc)


        [<Fact>]
        let ``indexes a tree`` () =
            let withCount = index tree
            let indexed, _ = run withCount 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
