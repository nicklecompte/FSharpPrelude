module PreludeTests.Fast2DArray

open System
open Xunit
open Prelude

/// Makes sure the Fast2DArray can be created
[<Fact>]
let ``Fast2DArray smoke test`` () =
    let fast2dAr = new Fast2DArray<int>(5,10)
    Assert.True(true)

[<Fact>]
let ``Fast2DArray init works`` () =
    let fast2dAr = Fast2DArray<int>.Init 5 5 (fun i j -> i + j)
    Assert.True(true)

[<Fact>]
let ``Fast2DArray get works`` () =
    let myFast2DArray = new Fast2DArray<int>([|1;2;3;4;5;6;7;8;9;10;11;12|],3,4)
    let testItem = myFast2DArray.[{x=3;y=2}]
    Assert.Equal(12,testItem)

[<Fact>]
let ``Fast2DArray set works`` () =
    let myFast2DArray = new Fast2DArray<int>([|1;2;3;4;5;6;7;8;9;10;11;12|],3,4)
    myFast2DArray.[{x=2;y=2}] <- 12
    Assert.Equal(12,myFast2DArray.[{x=2;y=2}])

[<Fact>]
let ``Fast2DArray copy smoke test`` () =
    let myFast2DArray = new Fast2DArray<int>([|1;2;3;4;5;6;7;8;9;10;11;12|],3,4)
    let copy = myFast2DArray.Copy()
    for i in 0..3 do
        for j in 0..2 do
            Assert.Equal(myFast2DArray.[{x=i;y=j}],copy.[{x=i;y=j}])

[<Fact>]
let ``Fast2DArray transpose smoke test`` () =
    let myFast2DArray = new Fast2DArray<int>([|1;2;3;4;5;6;7;8;9;10;11;12|],3,4)
    let transpose = Fast2DArray.transpose myFast2DArray
    printfn "%s" (myFast2DArray.ToString()) |> ignore
    printfn "%s" (transpose.ToString()) |> ignore
    Assert.Equal(transpose.[{x=0;y=0}],1)
    Assert.Equal(transpose.[{x=1;y=1}],6)
    Assert.Equal(transpose.[{x=1;y=2}],7)
