namespace Demo.MyLib.Tests

module Part04 =

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

    module Part04_1 =

        let mutable counter = 1

        let impureIndex v =
            let indeedLeaf = (v, counter)
            counter <- counter + 1
            indeedLeaf

        [<Fact>]
        let ``indexes a tree - impure`` () =
            let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
            let indexed = map impureIndex tree
            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
