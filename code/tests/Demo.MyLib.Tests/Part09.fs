namespace Demo.MyLib.Tests

module Part09 =

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

    module Part09_1 =

        type WithCount<'v> = WithCount of (int -> 'v * int)

        let run (WithCount f) (count: int) = f count

        let buildNode l r = Node(l, r)

        let buildLeaf v count _ = Leaf(v, count)

        let pure' v = WithCount(fun count -> (v, count))

        let (<*>) f a =
            WithCount(fun count ->
                let fv, fc = run f count
                let av, ac = run a fc
                let b = fv av
                b, ac)

        let getCount = WithCount(fun count -> (count, count))

        let incrementCount = WithCount(fun count -> ((), count + 1))

        let rec index =
            function
            | Leaf v -> pure' buildLeaf <*> pure' v <*> getCount <*> incrementCount
            | Node(l, r) -> pure' buildNode <*> index l <*> index r

        [<Fact>]
        let ``indexes a tree`` () =
            let withCount = index tree
            let indexed, _ = run withCount 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>

    module Part09_2 =

        type WithCount<'v> = WithCount of (int -> 'v * int)

        let run (WithCount f) (count: int) = f count

        let buildNode l r = Node(l, r)

        let buildLeaf v count = Leaf(v, count)

        let pure' v = WithCount(fun count -> (v, count))

        let (<*>) f a =
            WithCount(fun count ->
                let fv, fc = run f count
                let av, ac = run a fc
                let b = fv av
                b, ac)

        let (<*) f v =
            WithCount(fun count ->
                let fv, fc = run f count
                let _, newCount = run v fc
                (fv, newCount))

        let getCount = WithCount(fun count -> (count, count))

        let incrementCount = WithCount(fun count -> ((), count + 1))

        let rec index =
            function
            | Leaf v -> pure' buildLeaf <*> pure' v <*> getCount <* incrementCount
            | Node(l, r) -> pure' buildNode <*> index l <*> index r

        [<Fact>]
        let ``indexes a tree`` () =
            let withCount = index tree
            let indexed, _ = run withCount 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
