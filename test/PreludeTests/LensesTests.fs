module PreludeTests.Lenses

open System
open Xunit
open Prelude

type TestTwoEltRecord = {
    item1 : int
    item2 : string
}

type TestTwoEltComposed = {
    record : TestTwoEltRecord
    item : float
}

let testLens : Lens<TestTwoEltComposed, TestTwoEltRecord> =
    (
        (fun composed -> composed.record),
        (fun record composed -> {composed with record = record})
    )

[<Fact>]
let ``Set with lens smoke test``() =
    let record1 = {item1 = 0; item2 = "old item"}
    let composed1 = {record = record1; item = 6.0}
    let composed2 = setWithLens composed1 {item1 = 5; item2 = "new item"} testLens
    Assert.Equal(composed1.item, composed2.item)
    Assert.Equal(composed1.record.item1, 0)
    Assert.Equal(composed2.record.item1, 5)
    Assert.Equal(composed1.record.item2, "old item")
    Assert.Equal(composed2.record.item2, "new item")

[<Fact>]
let ``Get with lens smoke test``() =
    let record1 = {item1 = 10; item2 = "item"}
    let composed1 = {record = record1; item = 11.0}
    let record2 = getWithLens composed1 testLens
    Assert.Equal(record1,record2)
    
[<Fact>]
let ``Map with lens smoke test``() =
    let record1 = {item1 = 10; item2 = "item"}
    let composed1 = {record = record1; item = -10.5}
    let map1 = 
        fun (record:TestTwoEltRecord) -> 
            {item1 = record.item1 + 10; 
            item2 = record.item2.Replace("item","new item")}
    let composed2 = mapWithLens testLens map1 composed1 
    Assert.Equal(composed2.record,{item1 = 20; item2 = "new item"})
    Assert.Equal(composed2.item, -10.5)

