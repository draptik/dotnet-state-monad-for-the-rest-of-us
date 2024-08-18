namespace Demo.MyLib.Tests

module Part6 =

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

    module Part6_1 =

        let rec index =
            function
            | Leaf v -> fun count -> (Leaf(v, count), count + 1)
            | Node(l, r) ->
                fun count ->
                    let li, lc = index l count
                    let ri, rc = index r lc
                    Node(li, ri), rc

        [<Fact>]
        let ``indexes a tree`` () =
            let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

            let indexed, _ = index tree 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
