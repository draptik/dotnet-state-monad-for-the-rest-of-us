module Tests

open Swensen.Unquote
open Xunit

[<Fact>]
let ``My test`` () =
    test <@ true = true @>
