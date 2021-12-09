/// Signatures for the various collection data structures used in the Prelude.
/// MAJOR TODO: Should we give up on .NET interfaces and instead use records?
/// TODO: Things to consider:
///     1) The main thing I am concerned about is F#'s wonky approach to
///     interface polymorphism. Specifically things usually  need to be
///     explicitly cast.
///     2) OTOH 
///     
module Prelude.Collections.Interfaces

open System.Collections.Generic

/// Signature of a purely functional FIFO queue.
type IQueue<'T> =
    inherit IEnumerable<'T>
    /// Add an element to the end of the queue.
    abstract member Insert : 'T -> IQueue<'T>
    /// Get the element at the front of the queue.
    abstract member Head : 'T voption
    /// Get the resulting queue from removing the first element.
    abstract member Tail : IQueue<'T> voption
    /// Get the number of elements in the queue.
    abstract member Count : int
    /// Returns true if the queue is empty.
    abstract member IsEmpty : bool   
    
/// Signature of an imperative/mutable FIFO queue.
type IMutableQueue<'T> =
    inherit System.IDisposable
    inherit IEnumerable<'T>
    abstract member Insert : 'T -> unit 
    abstract member Head : 'T voption
    abstract member Tail : IMutableQueue<'T> voption
    abstract member Pop : 'T voption
    abstract member Count : int
    abstract member IsEmpty : bool
    
/// Signature of a purely functional dequeue, allowing reads and writes to both
/// ends of the queue.
type IDequeue<'T> =
    inherit IEnumerable<'T>
    /// Add an element to the front of the queue.
    abstract member InsertFront : 'T -> IQueue<'T>
    /// Get the element at the front of the queue.
    abstract member Head : 'T voption
    /// Get the resulting queue from removing the first element.
    abstract member Tail : IQueue<'T> voption
    /// Add an element to the end of the queue.
    abstract member InsertEnd : 'T -> IQueue<'T>
    /// Get the element at the end of the queue.
    abstract member Last : 'T voption
    /// Get the resulting queue from removing the last element.
    abstract member Init : IQueue<'T> voption
    /// Get the number of elements in the queue.
    abstract member Count : int
    /// Returns true if the queue is empty.
    abstract member IsEmpty : bool       

/// Signature of a purely functional ordered heap.
type IOrderedHeap<'T when 'T : comparison> =
    inherit IEnumerable<'T>
    abstract member Insert : 'T -> IOrderedHeap<'T>
    // note that lazy structures might need to compute a count.
    abstract member Count : unit -> int
    abstract member IsEmpty : bool
    abstract member Min : 'T voption
    abstract member DeleteMin : unit -> IOrderedHeap<'T>
    abstract member Merge : IOrderedHeap<'T> -> IOrderedHeap<'T>
    
    