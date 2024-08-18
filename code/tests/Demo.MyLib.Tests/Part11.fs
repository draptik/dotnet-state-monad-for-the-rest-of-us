namespace Demo.MyLib.Tests

module Part11 =

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

    module Part11_1 =

        // Make `WithCount` more generic
        type WithCount<'b, 'c> = WithCount of ('c -> 'b * 'c)
        // This is actually the signature of the state monad:
        // type State<'b, 's> = State of ('s -> ('b * 's))

        let run (WithCount f) (count: int) = f count

        // bind
        let (>>=) a f =
            WithCount(fun count ->
                let va, ca = run a count
                let result = f va
                run result ca)

        // reverse bind
        // let (=<<) a b = b (>>=) a

        let buildNode l r = Node(l, r)

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

        let putCount c = WithCount(fun _ -> ((), c))

        let rec index =
            function
            | Leaf v ->
                getCount
                >>= (fun count ->
                    let leaf = Leaf(v, count)
                    putCount (count + 1) >>= (fun _ -> pure' leaf))
            | Node(l, r) ->
                let li = index l
                let ri = index r
                let buildNode' l r = pure' (buildNode l r)
                li >>= (fun ll -> ri >>= (fun rr -> buildNode' ll rr))

        [<Fact>]
        let ``indexes a tree`` () =
            let withCount = index tree
            let indexed, _ = run withCount 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
