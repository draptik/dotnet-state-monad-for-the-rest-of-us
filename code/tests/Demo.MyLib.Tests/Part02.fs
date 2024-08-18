namespace Demo.MyLib.Tests

module Part02 =

    open Swensen.Unquote
    open Xunit

    module Part02_1 =

        type Tree<'a> =
            | Leaf of 'a
            | Node of Tree<'a> * Tree<'a>

        let rec lengths =
            function
            | Leaf v -> Leaf(String.length v)
            | Node(l, r) -> Node(lengths l, lengths r)

        [<Fact>]
        let ``calculate the leaves' content length`` () =
            let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
            let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

            let treeOfLengths = lengths treeOfWords

            test <@ treeOfLengths = treeOfNumbers @>

    module Part02_2 =

        type Tree<'a> =
            | Leaf of 'a
            | Node of Tree<'a> * Tree<'a>

        let (^+) l r = Node(l, r)

        let rec lengths =
            function
            | Leaf v -> Leaf(String.length v)
            | Node(l, r) -> lengths l ^+ lengths r

        [<Fact>]
        let ``calculate the leaves' content length`` () =
            let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
            let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

            let treeOfLengths = lengths treeOfWords

            test <@ treeOfLengths = treeOfNumbers @>

    module Part02_3 =

        type Tree<'a> =
            | Leaf of 'a
            | Node of Tree<'a> * Tree<'a>

        let baseCase _ = 1
        let baseCase' v = Leaf(String.length v)
        let (^+) l r = Node(l, r)

        let rec numberOfLeafs =
            function
            | Leaf v -> baseCase v
            | Node(l, r) -> numberOfLeafs l + numberOfLeafs r

        let rec lengths =
            function
            | Leaf v -> baseCase' v
            | Node(l, r) -> lengths l ^+ lengths r

        [<Fact>]
        let ``calculate the leaves' content length`` () =
            let treeOfWords = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
            let treeOfNumbers = Node(Leaf 3, Node(Leaf 3, Leaf 5))

            let treeOfLengths = lengths treeOfWords

            test <@ treeOfLengths = treeOfNumbers @>
