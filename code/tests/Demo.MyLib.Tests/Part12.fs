namespace Demo.MyLib.Tests

module Part12 =

    open Swensen.Unquote
    open Xunit

    type Tree<'a> =
        | Leaf of 'a
        | Node of Tree<'a> * Tree<'a>

    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    module Part12_1 =

        type WithCount<'b, 'c> = WithCount of ('c -> 'b * 'c)

        let run (WithCount f) (count: int) = f count

        let (>>=) a f =
            WithCount(fun count ->
                let va, ca = run a count
                let result = f va
                run result ca)

        let buildNode l r = Node(l, r)

        let pure' v = WithCount(fun count -> (v, count))

        // CE
        type WithCountExpression() =
            member this.Return v = pure' v
            member this.Bind(v, f) = v >>= f

        let withCount = WithCountExpression()

        let getCount = WithCount(fun count -> (count, count))

        let putCount c = WithCount(fun _ -> ((), c))

        let rec index =
            function
            | Leaf v ->
                withCount {
                    let! count = getCount
                    let leaf = Leaf(v, count)
                    let! _ = putCount (count + 1)
                    return leaf
                }
            | Node(l, r) ->
                withCount {
                    let! ll = index l
                    let! rr = index r
                    return buildNode ll rr
                }

        [<Fact>]
        let ``indexes a tree`` () =
            let withCount = index tree
            let indexed, _ = run withCount 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
