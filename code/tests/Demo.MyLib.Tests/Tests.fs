module Tests

open Swensen.Unquote
open Xunit

type Tree = Leaf | Node of (Tree * Tree)

let numberOfLeaves tree = failwith "Not yet implemented"

[<Fact>]
let ``counts the number of leaves in a tree`` () =
    let treeWith3Leaves = failwith "Not yet implemented"
    
    let leaves = numberOfLeaves treeWith3Leaves
    
    test <@ leaves = 3 @>