module Prelude.Collections.FunctionalQueue

open Prelude
open Prelude.Collections.Interfaces
open System.Collections.Generic
open System.Collections

/// The most common implementation of queues in a purely functional setting is
/// as a pair of lists, f and r, where f contains the front elements of the
/// queue in the correct order and r contains the rear elements of the queue in
/// reverse order. For example, a queue containing the integers 1... 6 might be
/// represented by the lists f = [1,2,3] and r - [6,5,4]. T
type SimpleQueue<'T> = ('T list) * ('T list)

module SimpleQueue =
    
    let toList ((sql,sqr) : SimpleQueue<'T>) =
        List.append sql (List.rev sqr)
        
    let toSeq ((sql,sqr) : SimpleQueue<'T>) =
        seq {
            yield! List.toSeq sql
            yield! List.toSeq (List.rev sqr)
        }
    
    let empty : SimpleQueue<'T> = [],[]
    
    let isEmpty ((sql,_): SimpleQueue<'T>) =
        match sql with
        | [] -> true
        | _ -> false
        
    let head ((sql,_) : SimpleQueue<'T>) : 'T voption =
        match sql with
        | [] -> ValueNone
        | x :: _ -> ValueSome x
        
    let length ((sql,sqr) : SimpleQueue<'T>) =
        (List.length sql) + (List.length sqr)
        
    let tail ((sql,sqr) : SimpleQueue<'T>) : SimpleQueue<'T> voption =
        match sql with
        | [] -> ValueNone
        // A queue needs O(1) access to the first element, but if the first
        // element is in sqr, it takes O(n) time to access it. Therefore, we
        // want sql to be empty only if sqr is also empty - so, if sql only
        // has one element, we set sql to be the reversed sqr, and sqr to be
        // empty. This takes O(n) time but it's a one-time cost and preserves
        // O(1) access 
        | [_] -> ValueSome (List.rev sqr,[]) 
        | _ :: xs -> ValueSome (xs,sqr)
    
    /// snoc = cons from the left. Adding an element to the simple queue means
    /// consing it to the second list.
    let snoc ((sql,sqr) : SimpleQueue<'T>) (x:'T) : SimpleQueue<'T> =
        match sql with
        | [] -> (List.rev (x :: sqr),[])
        | _ -> (sql,x::sqr)
    
    let rec toIQueue (sq : SimpleQueue<'T>) : IQueue<'T> =
        {
          new IQueue<'T> with
            member x.Insert ob : IQueue<'T> = toIQueue (snoc sq ob)
            member x.Head : 'T voption = head sq
            member x.Tail : IQueue<'T> voption =
                match tail sq with
                | ValueNone -> ValueNone
                | ValueSome s -> ValueSome (toIQueue s)
            member x.Count : int = length sq
            member x.IsEmpty : bool = isEmpty sq
            member x.GetEnumerator() : IEnumerator<'T> =
                (sq |> toSeq).GetEnumerator()
            member x.GetEnumerator() : IEnumerator =
                (x.GetEnumerator()) :> IEnumerator
        }
