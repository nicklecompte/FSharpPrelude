namespace Prelude
#nowarn "9"
open Microsoft.FSharp.NativeInterop
open System
open System.Runtime.CompilerServices
//open System.Text


[<Struct;IsByRefLike>]
/// Unsafe2DArray 
type Unsafe2DArray<'T when 'T : unmanaged>(numRows : int, 
        numCols : int
    ) = 
    member _.RowNum = numRows
    member _.ColNum = numCols

module Unsafe2DArray =

    let empty = ()
