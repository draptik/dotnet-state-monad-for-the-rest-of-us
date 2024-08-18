namespace Demo.MyLib.Tests

module Part3 =

    open Swensen.Unquote
    open Xunit

    module Part3_1 =

        type Tree<'a> =
            | Leaf of 'a
            | Node of Tree<'a> * Tree<'a>

        let rec map f =
            fun tree ->
                match tree with
                | Leaf v -> Leaf(f v)
                | Node(l, r) -> Node(map f l, map f r)


        let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
        let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

        [<Fact>]
        let ``calculate the leaves' content length, using map`` () =
            let treeOfLengths = map String.length treeOfWords

            test <@ treeOfLengths = treeOfNumbers @>

        [<Fact>]
        let ``calculate the leaves' content length, using map w/ custom operator`` () =
            let (^) = map

            let treeOfLengths = String.length ^ treeOfWords

            test <@ treeOfLengths = treeOfNumbers @>
