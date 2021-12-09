module Prelude.Collections.Heap

open System
open Prelude
//open Prelude.Graph

(*
/// Signature of an ordered heap.
type IOrderedHeap<'T when 'T : comparison> =
    inherit IEnumerable<'T>
    abstract member Insert : 'T -> IOrderedHeap<'T>
    // note that lazy structures might need to compute a count.
    abstract member Count : unit -> int
    abstract member IsEmpty : bool
    abstract member Min : 'T voption
    abstract member DeleteMin : unit -> IOrderedHeap<'T>
    abstract member Merge : IOrderedHeap<'T> -> IOrderedHeap<'T>
*)

/// A LeftistHeap<'T> is just a binary tree where for every node the rank of the
/// left child is at least that of the right sibling. The rank is the minimum
/// distance between the node in question and an empty node - the rank of a
/// LeftistHeap is the rank of the root.
type LeftistHeap<'T when 'T : comparison > =
    | EmptyHeap
    | HeapTree of int*'T*LeftistHeap<'T>*LeftistHeap<'T>
with
    
    /// This is O(log(n)).
    member x.Count() =
        match x with
        | EmptyHeap -> 0
        | HeapTree(_,_,lT,rT) ->
            1 + (lT.Count()) + (rT.Count())
    
    /// This is O(1).
    member x.Rank =
        match x with
        | EmptyHeap -> 0
        | HeapTree(y,_,_,_) -> y
        
    member x.IsEmpty : bool =
        match x with | EmptyHeap -> true | _ -> false
        
    member x.Merge (h:LeftistHeap<'T>) : LeftistHeap<'T> =
        // helper function
        let makeT elt (t1:LeftistHeap<'T>) (t2:LeftistHeap<'T>) =
            if t1.Rank >= t2.Rank then HeapTree(t2.Rank + 1,elt,t1,t2)
            else HeapTree(t1.Rank + 1, elt,t2,t1)
        match (x,h) with
        | (EmptyHeap,_) -> h
        | (_,EmptyHeap) -> x
        | HeapTree(_,eltX,lX,rX),HeapTree(_,eltH,lH,rH) ->
            if eltX < eltH then makeT eltX lX (rX.Merge h)
            else makeT eltH lH (x.Merge rH)
            
    member x.Insert (e: 'T) : LeftistHeap<'T> =
        match x with
        | EmptyHeap -> HeapTree(1,e,EmptyHeap,EmptyHeap)
        | HeapTree(rank,smallest,left,right) ->
            if e < smallest then
            // then e is the new root, and we need to increase on the *left*
                HeapTree(rank+1,e,left.Insert smallest,right)
            else
            // then compare the rank of left and right subtrees
            // to decide which one to insert into
                if left.Rank > right.Rank then // then insert into the right
                     HeapTree(rank+1,smallest,left,right.Insert e)
                else
                     HeapTree(rank+1,smallest, left.Insert e, right)
                
//                if left.Rank >= right.Rank then
        //x.Merge (HeapTree(1,e,EmptyHeap,EmptyHeap))
        
    member x.Min =
        match x with | EmptyHeap -> ValueNone | HeapTree(_,a,_,_) -> ValueSome a
        
    member x.DeleteMin =
        match x with
        | EmptyHeap -> EmptyHeap
        | HeapTree(_,_,a,b) -> a.Merge b
        
    override x.ToString() =
        match x with
        | EmptyHeap -> "[]"
        | HeapTree(_,x,a,b) ->
            let s1 = a.ToString()
            let s2 = b.ToString()
            $"{s1}    {s2} \n     {x.ToString()}"


module LeftistHeap =
    
    /// Create a single-node Heap containing the object `x`.
    let singleton x = HeapTree(1,x,EmptyHeap,EmptyHeap)
    
    /// Implement a function fromList of type Elem.T list ->• Heap that
    /// produces a leftist heap from an unordered list of elements by first
    /// converting each element into a singleton heap and then merging the heaps
    /// until only one heap remains. Instead of merging the heaps in one
    /// right-to-left or left-to-right pass using foldr or foldl, merge the
    /// heaps in [logn] passes, where each pass merges adjacent pairs of heaps.
    /// Show that fromList takes only O(n) time.
    /// Exercise 3.3 from Okasaki.
    let fromList (ls : List<'T>) : LeftistHeap<'T> =
        let rec inner xs =
            match xs with
            | [] -> [EmptyHeap]
            | [x] -> [x]
            | x :: y :: xz -> (x.Merge y) :: (inner xz)
        let rec inner2 lss =
            match lss with
            | [] -> [EmptyHeap]
            | [x] -> [singleton x]
            | x :: y :: ys -> ((singleton x).Merge (singleton y)) :: (inner2 ys)
        let rec inner3 lss =
            match lss with
            | [] -> EmptyHeap
            | [x] -> x
            | _ -> inner3 (inner lss)
        inner3 (inner2 ls)
        
/// Weight-biased leftist heaps are an alternative to leftist heaps that replace
/// the leftist property with the weight-biased leftist property: the size of
/// any left child is at least as large as the size of its right sibling.
/// Okasaki Exercise 3.4 (Cho and Sahni [CS96])        
type WeightedLeftistHeap<'T when 'T : comparison> =
    | EmptyWeightedHeap
    | WeightedHeapTree of
        totalSize:int*'T*WeightedLeftistHeap<'T>*WeightedLeftistHeap<'T>
with
    /// Note that the count is part of the constructor here so this is O(1).
    member x.Count =
        match x with
        | EmptyWeightedHeap -> 0
        | WeightedHeapTree(c,_,_,_) -> c
        
    /// This is O(log(n)).
    member x.Rank() =
        match x with
        | EmptyWeightedHeap -> 0
        | WeightedHeapTree(_,_,lT,rT) ->
            1 + (lT.Rank()) + (rT.Rank())
            
    member x.Min =
        match x with
        | EmptyWeightedHeap -> ValueNone
        | WeightedHeapTree(_,c,_,_) -> ValueSome c
        
    member x.Insert (c: 'T) =
        match x with
        | EmptyWeightedHeap ->
            WeightedHeapTree(1,c,EmptyWeightedHeap,EmptyWeightedHeap)
        | WeightedHeapTree(count,smallest,tL,tR) ->
            notImpl()

    /// Currently, merge operates in two passes: a top-down pass consisting of
    /// calls to merge, and a bottom-up pass consisting of calls to the helper
    /// function makeT. Modify merge for weight-biased leftist heaps to operate
    /// in a single, top-down pass.
    /// (d) What advantages would the top-down version of merge have in a lazy
    /// environment? In a concurrent environment?            
    member x.Merge (other:WeightedLeftistHeap<'T>) : WeightedLeftistHeap<'T> =
        match (x,other) with
        | (EmptyWeightedHeap,_) -> other
        | (_,EmptyWeightedHeap) -> x
        | _ -> notImpl()
        
    member x.DeleteMin() =
        match x with
        | EmptyWeightedHeap -> EmptyWeightedHeap
        | WeightedHeapTree(_,_,l,r) ->
            l.Merge r
            
/// A binomial tree is inductively defined as follows:
/// 1) A binomial tree of rank 0 is a singleton
/// 2) A binomial tree of rank n is created by joining two binomial trees of
/// rank n-1, joining one to the leftmost child of the other.
/// We represent this as a triple from the root node: (rank,rootElement, 
type BinomialTree<'T when 'T : comparison> =
    | BHNode of int*'T*(BinomialTree<'T> list)
with
    
    member x.Rank = match x with BHNode(r,_,_) -> r
    
    member x.Link (y:BinomialTree<'T>) =
        match x,y with
        | BHNode(rankX,eltX,xChildren),BHNode(rankY,eltY,yChildren) ->
            if rankX <> rankY then invalidArg "y" "cannot link binomial trees with different rank"
            if eltX < eltY then BHNode(rankX + 1,eltX,(List.append yChildren xChildren))
            else BHNode(rankX + 1,eltY,(List.append xChildren yChildren))

type BinomialHeap<'T when 'T : comparison > = BinomialTree<'T> list

module BinomialHeap =
    
    let rec private insertTree (t:BinomialTree<'T>) (l: BinomialTree<'T> list) : BinomialTree<'T> list =
        match l with
        | [] -> [t]
        | x :: xs -> if t.Rank < x.Rank then t :: l
                     else insertTree (t.Link x) xs
                     
    let insert (x : 'T) (heap : BinomialHeap<'T>) =
        insertTree (BHNode(0,x,[])) heap

    
    


(*
/// Signature of an ordered heap.
type IOrderedHeap<'T when 'T : comparison> =
    inherit IEnumerable<'T>
    abstract member Insert : 'T -> IOrderedHeap<'T>
    // note that lazy structures might need to compute a count.
    abstract member Count : unit -> int
    abstract member IsEmpty : bool
    abstract member Min : 'T voption
    abstract member DeleteMin : unit -> IOrderedHeap<'T>
    abstract member Merge : IOrderedHeap<'T> -> IOrderedHeap<'T>
*)