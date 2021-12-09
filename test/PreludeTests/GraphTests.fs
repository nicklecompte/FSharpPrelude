module PreludeTests.GraphTests

open System
open Xunit
open Prelude
open Prelude.Graph

let testDomain1 = seq {1..100}
let testRelation1 : int -> int list =
    fun i ->
        [1..100] |> List.filter(fun j -> j % i = 0)

let testBinaryRelation1 : BinaryRelation<int> =
    {
        domain = testDomain1
        relations = testRelation1
    }

[<Fact>]
let ``IBinaryRelation smoke test``() =
    let ibinary = testBinaryRelation1 :> IBinaryRelation<int>
    let domain = ibinary.Domain()
    Assert.True(domain.Length = 100)
    for i in 1..100 do
        Assert.Equal(i,domain.[i - 1])
    let pairList = ibinary.AsPairList()
    for i,j in pairList do
        Assert.True(j % i = 0)
