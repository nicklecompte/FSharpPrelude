/// An (unsafe!) circular queue for value types.
/// TODO: Unmanaged types?
module Prelude.Collections.CircularQueue

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open Interfaces
open Prelude

#nowarn "9"

/// A standard, managed circular queue. Not thread-safe.
type CircularQueue<'T>(numberElements : int) =
    let innerBuffer = Array.zeroCreate<'T> numberElements
    let mutable startIndex = 0
    let mutable endIndex = 0
    let mutable length = 0

    member x.Count : int = length

    member x.Insert(item: 'T) =
        endIndex <- (endIndex + 1) % numberElements
        if endIndex = startIndex then startIndex <- startIndex + 1
        innerBuffer.[endIndex] <- item
        if length <> numberElements then length <- length + 1
        notImpl()

    member x.Pop() : 'T voption =
        if length = 0 then ValueNone
        else
            let item = innerBuffer.[startIndex]
            startIndex <- (startIndex + 1) % numberElements
            ValueSome item
            
    member x.Head : 'T voption =
        match length with
        | 0 -> ValueNone
        | _ -> ValueSome (innerBuffer.[startIndex])
            
    /// This has a trivial Dispose implementation since all the data is managed.
    interface IDisposable with
        member x.Dispose() = ()

    interface IEnumerable<'T> with
        member x.GetEnumerator() = failwith "not done"

    interface IEnumerable with
        member x.GetEnumerator() =
            ((x :> IEnumerable<'T>).GetEnumerator()) :> IEnumerator
            
    interface IQueue<'T> with
        member this.Count = failwith "todo"
        member this.Head = failwith "todo"
        member this.Insert (x: 'T) = this.Insert x
        member this.IsEmpty = this.Count = 0
        member this.Tail = notImpl()

/// A standard managed circular queue using locks to ensure thread-safe queue
/// operations.
type CircularQueueThreadSafe<'T>(numElements : int) =
    class end
    
/// Optimized, unsafe circular queue over unmanaged types.
type UnmanagedCircularQueue<'T when 'T : unmanaged>(numElements:int) =
    let entries = NativePtr.stackalloc<'T> numElements
    let mutable startIndex = 0
    let mutable endIndex = 0
    let mutable count = 0

    member x.Count = count

    member x.Insert(t:'T) =
       let writeIndex : nativeptr<'T> = failwith "not done"
       NativePtr.write writeIndex t
       notImpl()
        
    member x.Pop() : 'T voption =
        failwith "not done"
        
    member x.Head : 'T voption =
        failwith "not done"

    member x.Capacity = (numElements - count)

    member x.HasCapacity : bool =
        count < numElements

    interface System.IDisposable with
        member x.Dispose() =
            Marshal.FreeHGlobal(NativePtr.toNativeInt entries)

    interface IEnumerable<'T> with
        member x.GetEnumerator() : IEnumerator<'T> = failwith "not done"

    interface IEnumerable with
        member x.GetEnumerator() =
            (x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator

//    interface IQueue<'T> with
//        member x.Insert(t) = x.Insert t
//        member x.Head = x.Head
//        member x.Count = x.Count
//        member x.IsEmpty = x.Count = 0