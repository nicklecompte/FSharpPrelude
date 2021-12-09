module Prelude.Collections.FunctionalDequeue

open Prelude
open Prelude.Collections.Interfaces

/// Okasaki:
/// [The double list] design [of the SimpleQueue<'T>] can easily be extended to
/// support the double-ended queue, or deque, abstraction, which allows reads
/// and writes to both ends of the queue (see Figure 5.3). The invariant is
/// updated to be symmetric in its treatment of f and r: both are required to be
/// non-empty whenever the deque contains two or more elements. When one list
/// becomes empty, split the other list in half and reverse one of the halves.
type SimpleDequeue<'T> = ('T list) * ('T list)

/// Standard functions and IDequeue<'T> implementation for SimpleDequeues.
/// Note: outside of the test method isValid and associated Prelude test cases,
/// few of the operations here enforce anything at a type/compiler level - these
/// are ad hoc operations on pairs of lists and have a bit more  "programmer
/// error" risk compared to an ideal functional program.
module SimpleDequeue =
    
    /// Determines if a SimpleDequeue<'T> is validly balanced.
    /// The only cases that aren't valid are when one of the lists is empty and
    /// the other has length greater than one - this test isn't about
    /// well-balancedness, just whether it violates the (semantic but
    /// unenforced) definition of SimpleDequeue<'T>.
    let isValid ((sdl,sdr):SimpleDequeue<'T>) : bool =
        match (sdl,sdr) with
        | [],[] -> true
        | [_],[] -> true
        | [],[_] -> true
        | [_],[_] -> true
        | _::_::_,[] -> false
        | [],_::_::_ -> false
        | _ -> true
    
    /// The empty SimpleDequeue<'T>.
    let empty : SimpleDequeue<'T> = ([],[])
    
    /// Determines if a SimpleDequeue<'T> is empty (i.e. whether its constituent
    /// lists are empty).
    let isEmpty ((sdl,sdr) : SimpleDequeue<'T>) =
        match sdl,sdr with
        | [],[] -> true
        | _ -> false
        
    /// Add the 'T object `e` to the front of a SimpleDequeue<'T> in the manner
    /// of a functional linked list. Typically this means consing it to the left
    /// sublist but the SimpleDequeue<'T> ([x],[]) is a special case. Otherwise,
    /// a SimpleDequeue<'T> can't become unbalanced by adding to the front.
    let cons ((sdl,sdr) : SimpleDequeue<'T>) (e: 'T) : SimpleDequeue<'T> =
        match sdl,sdr with
        // both lists empty - just put e at the front
        | [],[] -> ([e],[])
        // If the first list is nonempty but the second list is empty then it is
        // a violation to put e in the first list - the queue will have at least
        // two elements but the second list is empty. So we must split sdl,
        // reverse the second half, and move that half to the right, then cons e
        // to the first half on the left.
        // However, if every other operation proceeded correctly, sdr should
        // only ever be empty if sdl is a singleton. So we can keep the
        // SimpleDequeue balanced by just pushing the existing element to sdr. 
        | [x],[] -> ([e],[x])
        
        (*
        // This case should never be met since it is an invalid SimpleDeque.
        | (_::_::_,[]) ->
            invalidArg
                "sdl"
                "the SimpleDequeue (sdl,sdr) has two elements in sdl but sdr is empty" 
        *)
        
        // If sdr is nonempty then there is no violation if we put e in sdl,
        // regardless of what sdl looks like. We want to keep this operation
        // O(1) and only do an expensive O(n) rebalancing in the case of a
        // potentially unbalanced tail/init.
        | _ -> (e :: sdl, sdr)

    /// Get the first element of the SimpleDequeue<'T> if it is nonempty, or
    /// ValueNone if the SimpleDequeue<'T> is empty. 
    let head ((sdl,sdr) : SimpleDequeue<'T>) : 'T voption =
        match sdl,sdr with
        | [],[] -> ValueNone
        | [],x::_ -> ValueSome x // sdr's tail should always be empty
                                   // TODO we could raise an exception otherwise?
        | x::_,_ -> ValueSome x

    /// Get the SimpleDequeue<'T> resulting from deleting the first element, or
    /// ValueNone if the SimpleDequeue<'T> is empty. If the left constituent
    /// list sdl is a singleton and the right sdr is nonempty, then we rebalance 
    /// by splitting sdr in half, reversing the second half, then using that
    /// reversed second half as the new sdl.
    let tail ((sdl,sdr) : SimpleDequeue<'T>) : SimpleDequeue<'T> voption =
        match sdl,sdr with
        | [],[] -> ValueNone
        | [],_::_ -> ValueSome empty // sdr's tail should always be empty
                                         // TODO could raise an exception here
                                         // TODO if condition is not met?
        | [_],[] -> ValueSome empty
        // If sdl is a singleton then take back half of sdr, reverse it, put it
        // on the left to preserve invariant.
        | [_],_ ->
            let l = sdr.Length
            let firstHalf,secondHalf = List.splitAt (l/2) sdr
            ValueSome (List.rev secondHalf, firstHalf)
        // Here, xs and sdr are both nonempty.
        | (_::xs,_) -> ValueSome (xs,sdr)
        
    /// Add an element to the end of a SimpleDeque<'T>. Like cons, this can only
    /// make an unbalanced SimpleDequeue<'T> if sdl is empty and sdr is a
    /// singleton - otherwise
    let snoc ((sdl,sdr) : SimpleDequeue<'T>) (e:'T) : SimpleDequeue<'T> =
        match sdl,sdr with
        | [],[] -> ([e],[])
        | _,[] -> (sdl,[e])
        | [],[x] -> ([x], [e]) // TODO Raise an exception if |sdr| > 1
                               // TODO and sdl empty?
        // Otherwise adding to the end of the SimpleDequeue just means consing
        // it to the second list.
        | _ -> (sdl, e :: sdr)
        
    let last ((sdl,sdr) : SimpleDequeue<'T>) : 'T voption =
        match sdl,sdr with
        | [],[] -> ValueNone
        // If sdl is nonempty and sdr is empty, then sdl is a singleton but
        // we aren't checking. 
        // TODO throw an exception if malformed?        
        | x::_,[] -> ValueSome x
        | _,x::_ -> ValueSome x
        
    let init ((sdl,sdr) :SimpleDequeue<'T>) : SimpleDequeue<'T> voption =
        match sdl,sdr with
        | [],[] -> ValueNone
        // TODO investigate throwing if the lists aren't singletons - this
        // TODO shortcuts by assuming the SimpleDequeue is well-formed.
        | [],_ -> ValueSome empty
        | _,[] -> ValueSome empty
        // If sdl nonempty and sdr is a singleton then we can't have an
        // improperly balanced SimpleDequeue<'T> - so split sdl in half and
        // reverse the second half.
        | _,[_] ->
            let l = sdl.Length 
            let firstHalf,secondHalf = List.splitAt (l/2) sdl
            ValueSome (firstHalf,List.rev secondHalf)
        // otherwise just take the tail of sdr
        | _,_::xs -> ValueSome (sdl,xs)
        
    