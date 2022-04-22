module Prelude.Collections.SearchTree

open Prelude
open Prelude.Collections.Interfaces
open Prelude.Graph

[<Struct>]
type RBTColor = | Red | Black

/// Chris Okasaki's implementation of a red-black binary search tree. This
/// satisfies the following invariants:
/// 1) no Red node has a Red child.
/// 2) Every path from the root to a leaf (i.e. an empty node) contains the same
/// number of Black nodes. 
/// The EmptyRBTree is considered a Black "node."
/// All methods on this type are purely functional (though often inefficient).
/// Note that this data type is specifically different from the BinaryTree<'T>
/// in Prelude.Graph.
type RedBlackTree<'T when 'T : comparison> =
    | EmptyRBTree
    | RBT of RBTColor * RedBlackTree<'T> * 'T * RedBlackTree<'T>
with
    
    /// True if the tree is empty. Property determined 0(1) at instantiation.
    member x.IsEmpty =
        match x with | EmptyRBTree -> true | _ -> false
        
    member x.Root : 'T option =
        match x with
        | EmptyRBTree -> None
        | RBT(_,_,y,_) -> Some y
         
    /// Note that this computation is O(n) - it is probably better to keep track
    /// of the count separately than to invoke this method.
    member x.Count() =
        match x with
        | EmptyRBTree -> 0
        | RBT(_,tL,_,tR) -> 1 + tL.Count() + tR.Count()
        
    // Determines if the tree contains a given element. O(log(n)) complexity.
    member x.Contains (elem : 'T) =
        match x with
        | EmptyRBTree -> false
        | RBT(_,tLeft,curElem,tRight) -> // color not relevant for containment 
            if curElem = elem then true
            else if curElem < elem then tRight.Contains elem
                 else tLeft.Contains elem
                 
    /// Create a new RedBlackTree<'T> containing `elem` and all the original
    /// elements, properly balanced.
    member x.Insert (elem : 'T) : RedBlackTree<'T> =
        // balance enforces the RBTree invariants - in effect it "massages" the
        // intermediate RBTrees as x.Insert(elem) proceeds in order to properly
        // insert elements while ensuring no Red nodes have Red children, and
        // that Black nodes are partitioned equally among the paths joining to
        // and from curElem.
        (*
        Exercise 3.10 The balance function currently performs several unnecessary
    tests. For example, when the ins function recurses on the left child, there is no
    need for balance to test for red-red violations involving the right child.
    (a) Split balance into two functions, Ibalance and rbalance, that test for
    violations involving the left child and right child, respectively. Replace the
    calls to balance in ins with calls to either Ibalance or rbalance.
    (b) Extending the same logic one step further, one of the remaining tests on
    the grandchildren is also unnecessary. Rewrite ins so that it never tests
    the color of nodes not on the search path.
        *)
        let balance (col:RBTColor) (lTree:RedBlackTree<'T>)
                    (curElem:'T) (rTree:RedBlackTree<'T>) : RedBlackTree<'T> =
            match (col,lTree,rTree) with
            | (Black,RBT(Red,RBT(Red,a,x,b),y,c),d) ->
                RBT(Red,RBT(Black,a,x,b),y,RBT(Black,c,curElem,d))
            | (Black, RBT(Red,a,x,RBT(Red,b,y,c)),d) ->
                RBT(Red,RBT(Black,a,x,b),y,RBT(Black,c,curElem,d))
            | (Black,a,RBT(Red,RBT(Red,b,y,c),z,d)) ->
                RBT(Red,RBT(Black,a,curElem,b),y,RBT(Black,c,z,d))
            | (Black,a,RBT(Red,b,y,RBT(Red,c,z,d))) ->
                RBT(Red,RBT(Black,a,curElem,b),y,RBT(Black,c,z,d))
            | _ -> RBT(col,lTree,curElem,rTree)                
            (* From Okasaki:
            fun balance (B,T (R,T (R,a,x,b),y,c),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
              | balance (B,T (R,a,x,T (R,b,y,c)),z,d) = T (R,T (B,a,x,b),y,T (B,c,z,d))
              | balance (B,a,x,T (R,T (R,b,y,c),z,d)) = T (R,T (B,a,x,b),y,T (B,c,z,d))
              | balance (B,a,x,T (R,b,y,T (R,c,z,d))) = T (R,T (B,a,x,b),y,T (B,c,z,d))
              | balance body = T body
            *)
        // The inner function that performs the insert (and calls balance) - 
        // TODO investigate refactor to CPS. Use the profiler.
        let rec ins t =
            match t with
            | EmptyRBTree -> RBT(Red,EmptyRBTree,elem,EmptyRBTree)
            | RBT(color,tL,current,tR) ->
                if elem < current then balance color (ins tL) current tR
                else if elem > current then balance color tL current (ins tR)
                else t
        ins x
//    interface IOrderedHeap<'T> with
//        member x.Insert y = (x.Insert y) :> IOrderedHeap<'T>
//        member x.Count() = x.Count()
//        member x.IsEmpty() = x.IsEmpty
             
/// Functions for operations to/from/on balanced red-black trees. Much of this
/// comes directly from Okasaki or his exercises. Everything here is purely
/// functional.
module RedBlackTree =
    
    let isMember (tr : RedBlackTree<'T>) (elem : 'T) : bool =
        tr.Contains elem
                 
    let insert (tr : RedBlackTree<'T>) (elem : 'T) : RedBlackTree<'T> =
        tr.Insert elem
         
    /// fromOrdList converts a sorted list with no duplicates into a red-black
    /// tree. This function runs in O(n) time.
    /// NB: This is exercise 3.9 from Okasaki.
    let fromComparableList (l: 'T list) : RedBlackTree<'T> =
        // The inner function that runs through the list (ls) with the
        // intermediate RBTree tr. Here is a description of how it works:
        //      1) It takes for granted that ls is sorted and contains no
        //  duplicates. Therefore if ls and tr are both nonempty, say (x::xs)
        //  and (RBT(c,l,y,r)), then x > y always. 
        //      2) Therefore x always needs to be added to the rightmost branch
        //  of the tree. Since the tree will need to stay balanced, 
        // TODO: investigate CPS refactor.
        let rec inner (ls: 'T list) (tr: RedBlackTree<'T>) : RedBlackTree<'T> =
            match ls with
            | [] -> tr
            | x :: xs ->
                match tr with
                // Empty - start with a new red node and proceed through the rest
                // of the list.
                | EmptyRBTree -> inner xs (RBT(Red,EmptyRBTree,x,EmptyRBTree))
                // If we come across a Red node with the 'T item `y`, then we
                // know y < x because the input list is sorted. Hence we will
                // need to add to the right tree. 
                | RBT(Red,tL,y,tR) ->
                    match tR with
                    | EmptyRBTree -> notImpl() //Black,)
                    | _ -> notImpl()
                // If we come across a Black node with item `y` then y < x, so
                // we are adding to the                
                | RBT(Black,tL,y,tR) -> notImpl()
        let sorted = l |> List.distinct |> List.sort // sorted ascending order
        inner sorted EmptyRBTree
        
    let toSortedList (tr : RedBlackTree<'T>) : 'T list =
        let rec inner tree ls =
            match tree with
            | EmptyRBTree -> ls
            | RBT(_,tL,x,tR) ->
                notImpl() 
        inner tr []
        
    /// Delete a `T `toDelete` from the RedBlackTree<'T> `tr`.
    /// If `tr` doesn't actually contain 'toDelete' then just return 'tr.
    let deleteItem (tr : RedBlackTree<'T>) (toDelete : 'T) : RedBlackTree<'T> =
        let inner subTree = notImpl()
        match tr.IsEmpty with
        | true -> tr
        | false ->
            match isMember tr toDelete with
            | true -> inner tr
            | false -> tr