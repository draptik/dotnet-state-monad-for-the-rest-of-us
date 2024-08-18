namespace Demo.MyLib.Tests

module Part12 =

    open Swensen.Unquote
    open Xunit

    type Tree<'a> =
        | Leaf of 'a
        | Node of Tree<'a> * Tree<'a>

    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    module Part12_1 =

        type State<'b, 'c> = State of ('c -> 'b * 'c)

        let run (State f) count = f count

        let (>>=) a f =
            State(fun count ->
                let va, ca = run a count
                let result = f va
                run result ca)

        let buildNode l r = Node(l, r)

        let pure' v = State(fun count -> (v, count))

        // CE
        type StateExpression() =
            member this.Return v = pure' v
            member this.Bind(v, f) = v >>= f

        let state = StateExpression()

        let getCount = State(fun count -> (count, count))

        let putCount c = State(fun _ -> ((), c))

        let rec index =
            function
            | Leaf v ->
                state {
                    let! count = getCount
                    let leaf = Leaf(v, count)
                    let! _ = putCount (count + 1)
                    return leaf
                }
            | Node(l, r) ->
                state {
                    let! ll = index l
                    let! rr = index r
                    return buildNode ll rr
                }

        [<Fact>]
        let ``indexes a tree`` () =
            let stateWithCount = index tree
            let indexed, _ = run stateWithCount 1

            test <@ indexed = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3))) @>
