module PreludeTests.PreludeTests

open System
open Xunit
open Prelude

[<Fact>]
let ``mapPair smoke test``() =
    let mapFn = fun x -> x + 5
    Assert.Equal(mapPair mapFn (0,1),(5,6))

[<Fact>]
let ``flipTuple smoke test``() =
    Assert.Equal(flipTuple (0,1),(1,0))

[<Fact>]
let ``Prelude smoke test``() =
    Assert.True(true)

