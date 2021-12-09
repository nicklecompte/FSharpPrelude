namespace Prelude

open System
open System.Runtime.CompilerServices
open System.Text

/// This struct represents an xy-pair for accessing an element in a Fast2DArray<'T>.
/// It is named geometrically. x is the number of columns, y is the number of rows.
[<Struct>]
type TwoDimensionalCoordinate = {x: int; y: int}
with
    static member (+) (a,b) = 
        {x = a.x + b.x; y = a.y + b.y}
    static member (-) (a,b) = 
        {x = a.x - b.x; y = a.y - b.y}
    static member (*) (a:int,b:TwoDimensionalCoordinate) = 
        {x = a*b.x; y = a*b.y}
    static member (/) (b : TwoDimensionalCoordinate, a : int) = 
        {x = b.x/a; y = b.y/a}
    static member UpdateX (coord,xVal) = 
        {x = xVal; y = coord.y}
    static member UpdateY (coord,yVal)= 
        {x = coord.x ; y = yVal}
    member this.L1Norm = 
        (abs this.x) + (abs this.y)
    member this.L2Norm = 
        sqrt (float(this.x)**2.0 + float(this.y)**2.0)
    member this.Neighbors() : List<TwoDimensionalCoordinate> =
        [{y = this.y ; x = this.x - 1}; {y = this.y ; x = this.x + 1};
         {x = this.x ; y = this.y - 1}; {x = this.x ; y = this.y + 1};
         {x = this.x + 1 ; y = this.y - 1};{x = this.x + 1 ; y = this.y + 1};
         {x = this.x - 1 ; y = this.y - 1};{x = this.x - 1 ; y = this.y + 1}]

/// A type functionally identical to ^T[,], but with unchecked row/column access
///for speed. The performance of built-in .NET 2d arrays is unpredictable, 
/// particularly if the array is large.
/// TODO Make this a byref-like struct? Or investigate a span/etc here.
type Fast2DArray< 'T>(numRows: int, numCols: int) =
    let mutable innerArray : 'T array = Array.zeroCreate (numRows*numCols)
    member internal _.InnerArray
        with get() = innerArray
        and set value = innerArray <- value

    new(input:'T array, numRows : int, numCols : int)
        as this = Fast2DArray(numRows, numCols) then this.InnerArray <- input

    member _.RowNum = numRows
    member _.ColNum = numCols

    member _.To1dArray = innerArray

    /// Get according to 0-based indexing.
    /// Throws the default .NET IndexAccessException if the underlying
    /// TwoDimensionalCoordinate is "too large"
    member _.Item
        with get(coord:TwoDimensionalCoordinate) : ^T = 
            innerArray.[numCols*coord.y + coord.x]
        and set (coord:TwoDimensionalCoordinate) (value : ^T) = 
            innerArray.[numCols*coord.y + coord.x] <- value

    member _.GetRow (rowNum:int) : 'T array = 
        innerArray.[rowNum*numCols..(rowNum*numCols + numCols - 1)]
    member _.GetColumn (colNum:int) : 'T array =
        [|0..numRows - 1|]
        |> Array.map(fun rowNum -> innerArray.[rowNum*numCols + colNum])
    member _.Copy() : Fast2DArray<'T> =
        new Fast2DArray<'T>(Array.copy innerArray,numRows,numCols)

    member _.Transpose() : Fast2DArray<'T> =
        let new1dArray =
            Array.init innerArray.Length
                (fun index -> 
                    innerArray.[(index % numRows)*numCols + index / numRows])
        new Fast2DArray<'T>(new1dArray, numCols, numRows)

    static member InitEmpty numRows numCols : Fast2DArray<'T> =
        let new1dArray : array<'T> = Array.zeroCreate (numRows*numCols)
        new Fast2DArray<'T>(new1dArray,numRows,numCols)

    static member Init 
        (numRows: int) (numCols: int) (initFunc: int -> int -> 'T) 
        : Fast2DArray<'T> =
        let new1dArray = Array.zeroCreate (numRows*numCols)
        for col in 0..(numCols - 1) do
            for row in 0..(numRows - 1) do
                new1dArray.[numCols*row + col] <- initFunc row col
        new Fast2DArray<'T>(new1dArray,numRows,numCols)

    override x.ToString() =
        let builder =  StringBuilder()
        let rowStringArray =
            [|0..(numRows-1)|] |> Array.map(fun i -> $"%A{x.GetRow i}")
        rowStringArray 
        |> Array.iter(fun row -> builder.Append(row + "\n") |> ignore)
        builder.ToString()

[<Struct;IsByRefLike>]
type ByRefFast2DArray< 'T>=
    //[<DefaultValue(false)>]
    val mutable internal innerSpan : Span<'T>
    val RowNum : int
    val ColNum : int
    new(rowNum,colNum) = 
        let newArray = Array.zeroCreate (rowNum*colNum)
        let span = Span<'T>(newArray)
        {RowNum=rowNum;ColNum=colNum;innerSpan=span}
    // member internal __.InnerArray
    //     with get() = innerArray.ToArray()
    //     and set(value) = innerArray <- value

    new(input:'T array, numRows : int, numCols : int) =
        {RowNum=numRows;ColNum=numCols;innerSpan=Span<'T>(input)}
        
    // member __.To1dArray = innerArray

    /// Get according to 0-based indexing.
    /// Throws the default .NET IndexAccessException if the underlying
    /// TwoDimensionalCoordinate is "too large"
    // member __.Item
    //     with get(coord:TwoDimensionalCoordinate) : ^T = innerArray.[numCols*(coord.y) + coord.x]
    //     and set (coord:TwoDimensionalCoordinate) (value : ^T) = innerArray.[numCols*(coord.y) + coord.x] <- value

    // member __.GetRow (rowNum:int) : 'T array = innerArray.[rowNum*(numCols)..(rowNum*(numCols) + numCols - 1)]
    // member __.GetColumn (colNum:int) : 'T array =
    //     [|0..numRows - 1|]
    //     |> Array.map(fun rowNum -> innerArray.[rowNum*numCols + colNum])
    // member __.Copy() : Fast2DArray<'T> =
    //     new Fast2DArray<'T>(Array.copy innerArray,numRows,numCols)

    // member __.Transpose() : Fast2DArray<'T> =
    //     let new1dArray =
    //         Array.init (innerArray.Length)
    //             (fun index -> innerArray.[(index % numRows)*numCols + index / numRows])
    //     new Fast2DArray<'T>(new1dArray, numCols, numRows)

    // static member InitEmpty numRows numCols : Fast2DArray<'T> =
    //     let new1dArray : array<'T> = Array.zeroCreate (numRows*numCols)
    //     new Fast2DArray<'T>(new1dArray,numRows,numCols)

    // static member Init (numRows: int) (numCols: int) (initFunc: int -> int -> 'T) : Fast2DArray<'T> =
    //     let new1dArray = Array.zeroCreate (numRows*numCols)
    //     for col in 0..(numCols - 1) do
    //         for row in 0..(numRows - 1) do
    //             new1dArray.[numCols*(row) + col] <- initFunc row col
    //     new Fast2DArray<'T>(new1dArray,numRows,numCols)

    // override x.ToString() =
    //     let builder = new StringBuilder()
    //     let rowStringArray =
    //         [|0..(numRows-1)|] |> Array.map(fun i -> sprintf "%A" (x.GetRow i))
    //     rowStringArray |> Array.iter(fun row -> builder.Append(row + "\n") |> ignore)
    //     builder.ToString()

/// Helper functions for declarative processing of Fast2DArrays.
[<RequireQualifiedAccess>]
module Fast2DArray =

    let inline getSafe<'T> (ar: Fast2DArray<'T>) coord = 
        match (coord.x <= ar.ColNum, coord.y <= ar.RowNum) with
        | true,true -> ValueSome ar.[coord]
        | _ -> ValueNone

    let inline getUnsafe<'T> (ar: Fast2DArray<'T>) coord =
        ar.[coord]

    let iter func (ar: Fast2DArray<_>) =
        ar.To1dArray |> Array.iter func

    let iter_row_col (func : int -> int -> 'T -> unit) (ar : Fast2DArray<'T>) : unit =
        for i in 0..(ar.RowNum - 1) do
            for j in 0..(ar.ColNum - 1) do
                func i j ar.[{x=i;y=j}]

    let map func (ar: Fast2DArray<_>) =
        let newAr = ar.To1dArray |> Array.map func
        Fast2DArray<_>(newAr,ar.RowNum,ar.ColNum)

    let rotate90DegreesClockWise (ar: Fast2DArray<'T>) =
        let newColNum = ar.RowNum
        let newRowNum = ar.ColNum
        Fast2DArray<'T>.Init
            newRowNum
            newColNum
            (fun i j ->
                let oldArRowNum = ar.RowNum - (j % ar.RowNum) - 1
                let oldArColNum = i % ar.ColNum
                ar.[{x=oldArColNum;y=oldArRowNum}])//(newAr, newRowNum, newColNum)

    let transpose (ar: Fast2DArray<'T>)  : Fast2DArray<'T> =
        let newAr = Fast2DArray<'T>.InitEmpty ar.ColNum ar.RowNum
        for col in 0..(ar.ColNum-1) do
            for row in 0..(ar.RowNum-1) do
                newAr.[{x=row;y=col}] <- ar.[{x=col;y=row}]
        newAr

    let fold folderFunc initialState (twoDArray : Fast2DArray<'T>) =
        Array.fold folderFunc initialState twoDArray.To1dArray

    let foldAlongRow 
        (folderFunc: 'T -> 'U) (initialState: 'U) (twoDArray: Fast2DArray<'T>) 
        : 'U array =
        failwith "not done"

    let foldAlongColumn 
        (folderFunc: 'T -> 'U) (initialState: 'U) (twoDArray: Fast2DArray<'T>) 
        : 'U array =
        failwith "not done"

    let toRowList (twoDArray: Fast2DArray<'T>) : List< 'T array> =
        notImpl()

    let toColList (twoDArray: Fast2DArray<'T>) : List< 'T array> =
        notImpl()

    let rotate180Degrees (ar: Fast2DArray<'T>) =
        Fast2DArray<'T>.Init
            ar.RowNum
            ar.ColNum
            (fun i j -> ar.[{y=ar.RowNum - i - 1;x=ar.ColNum - j - 1}])

    /// Takes an n-by-m Fast2DArray and returns an m-by-n Fast2DArray, rotated 
    /// 90 degrees counter-clockwise. The rightmost column becomes the upper 
    /// row, and so on. For instance, [|[1;2;3];[4;5;6]|] becomes 
    /// [|[6;3];[5;2];[4;1]|]
    let rotate90DegreesCounterClockWise (ar:Fast2DArray<'T>) =
        let newColNum = ar.RowNum
        let newRowNum = ar.ColNum
        Fast2DArray<'T>.Init
            newRowNum
            newColNum
            (fun i j ->
                let oldArRowNum = ar.RowNum - (j % ar.RowNum) - 1
                let oldArColNum = i % ar.ColNum
                ar.[{y=oldArRowNum;x=oldArColNum}])
