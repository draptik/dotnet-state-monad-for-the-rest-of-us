namespace Demo.MyLib.Tests

module Part5 =

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

    module Part5_1 =

        let rec index =
            function
            | Leaf v -> failwith "Not yet implemented"
            | Node(l, r) -> failwith "Not yet implemented"

        [<Fact(Skip = "Exercise")>]
        let ``indexes a tree`` () =
            let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

            let indexed = index tree

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
