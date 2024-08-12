module Tests

open Swensen.Unquote
open Xunit

type Tree =
    | Leaf
    | Node of (Tree * Tree)

let rec numberOfLeaves =
    function
    | Leaf -> 1
    | Node(l, r) -> numberOfLeaves l + numberOfLeaves r
    
[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let treeWith3Leaves = Node(Leaf, Node(Leaf, Leaf))
    
    let leaves = numberOfLeaves treeWith3Leaves
    
    test <@ leaves = 3 @>