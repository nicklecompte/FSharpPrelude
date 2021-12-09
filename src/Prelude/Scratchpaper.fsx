type Callback<'T> = 'T -> unit

type AsyncSeqElement<'T> = 
    | Terminated
    | ACons of 'T * AsyncSeq<'T>
and
    AsyncSeq<'T> = (Callback<AsyncSeqElement<'T>> * Callback<exn>)-> unit

module AsyncSeq = 
    let emptyFunc() = failwith "not done"


/// Characterization of an Async operation by the dominant type of operation.
[<RequireQualifiedAccess>]
type AsyncOperationType = 
    | Read // of readsource? for e.g. web, disk, etc. 
    | Write
    | Compute
    | Unspecified
    | Mixed of primary:AsyncOperationType*secondary:AsyncOperationType
    static member Validate opType = 
        match opType with
        | Mixed (Unspecified,_) -> false
        | Mixed (_,Unspecified) -> false
        | _ -> true

type AsyncOperation<'T> = {
    opType: AsyncOperationType
    op: unit -> 'T
}

[<RequireQualifiedAccess>]
module AsyncOperation =
    
    let pureOp : 'T -> AsyncOperation<'T> = fun t ->
        {op = (fun () -> t); opType = AsyncOperationType.Unspecified}

    let bindOps : AsyncOperation<'T> -> ('T -> AsyncOperation<'U>) -> AsyncOperation<'U> =
        fun opT f -> f (opT.op())

//type StringOrDefaultInt = 
//    | Str of string
//    | DefaultInt of param:int

//type ExperimentalImplementBuilder(param:int) =
//    member __.Bind(m, f) =
//        match m with
//        | Str s -> f s
//        | DefaultInt _ -> DefaultInt param

//    member __.Return(x) =
//        Str x
//    member __.Return(x:int) =
//        DefaultInt param

//let myBuilder = new ExperimentalImplementBuilder(42)

//// It works!
//let ``should be DefaultInt 42`` = myBuilder {
//    return 12
}

#load "Prelude.fs"
#load "Fast2DArray.fs"
#load "..\Physics.fs"
#load "..\Contracts.fs"

open RoguelikePrototype
open Physics
open Contracts

let stepTowardsTest = Paths.stepTowards {x=3;y=0} {x= 3;y=1}
let getDirectionTest = Paths.getApproximateDirection {x=0;y=0} {x=31;y=11}
let getAngleTest = Paths.getAngleInRadians {x=0;y=0} {x=61;y=11}
let fromAngleTest = Direction.fromRadian 1.39<radian>
#time
 let asthecrowfiestest = Paths.getAsTheCrowFliesPath {x=0;y=0} {x=51442;y=612105} |> ignore