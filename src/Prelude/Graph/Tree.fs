namespace Prelude.Graph

open Prelude

/// A recursive, functional tree.
/// Note that the use of NonemptyList ensures that each representation is
/// unique under the incidence topology. This data does not include the concept
/// of an "empty" tree.
type Tree<'TVertex> =
    | Leaf of 'TVertex
    | Branch of 'TVertex * NonemptyList<Tree<'TVertex>>

    member x.Head =
        match x with
        | Leaf v -> v
        | Branch(v,_) -> v

    member x.Rest : Tree<'TVertex> list =
        match x with
        | Leaf _ -> []
        | Branch(_, ls) -> ls.ToList

    member x.VertexList() =
        let rec innerLoop cont t =
            match t with
            | Leaf v -> cont [v]
            | Branch(v,tl) ->
                match tl.rest with
                | [] -> innerLoop (fun acc -> cont (v :: acc)) tl.head
                | tree1 :: trees ->
                    innerLoop
                        (fun acc -> 
                            List.append acc (cont (tl.head.VertexList())))
                        (Branch(v,{head=tree1;rest=trees}))

        innerLoop id x

    /// Returns the edges of the tree, interpreted as a directed (root->branch)
    /// graph. Edges are in "left-to-right" "top-to-bottom" order.
    member x.Edges() : ('TVertex * 'TVertex) list =
        let rec innerLoop cont t =
            match t with
            | Leaf _ -> cont []
            | Branch(v,tl) ->
                match tl.head,tl.rest with
                | Leaf w, [] -> cont [(v,w)]
                | Leaf w,tree :: trees ->
                    innerLoop
                        (fun acc -> 
                            List.append (cont ((v,w) :: acc)) (tree.Edges()))
                        (Branch(v,{head=tree;rest=trees}))
                | Branch(w,tl),[] ->
                    innerLoop (fun acc -> cont ((v,w) :: acc)) (Branch(w,tl))
                | Branch(w,tl),trees -> notImpl()
        innerLoop id x

    interface IBinaryRelation<'TVertex> with
        member x.Domain() = x.VertexList()
        member x.AsPairList() = notImpl()


[<RequireQualifiedAccess>]
module Tree =

    /// Implements foldl on a Tree - from root to leaves, "left" to "right" 
    /// (defining l/r by the list structure defining branches.)
    /// NOTE: Noncommutative or nonassociative fold functions may lead to 
    /// unpredictable results.
    (*
    Comment on implementation:
    We actually rearrange the tree while folding to make this function easier to
    maintain - specifically, in a case of a Branch(vertex, listTrees), we append
    the listTrees end-to-end when doing the fold.
    *)
    let fold (zero: 'U) (folder: 'U -> 'T -> 'U) (t:Tree<'T>) : 'U =
        let rec inner cont tr =
            match tr with
            | Leaf v -> cont (folder zero v)
            | Branch(v,ls) ->
                match ls.rest with
                | [] -> cont (folder zero v)
                | [x] -> inner (fun state -> cont (folder state v)) x
                | x :: y :: xs ->
                    match x with
                    | Leaf u ->
                        // if we're at the end of the leftmost branch, transform 
                        // the tree by taking the next branches and "appending"
                        // them to the end. Then fold along that.
                        inner (fun state -> 
                            cont (folder state v)) (Branch(u,{head=y;rest=xs}))
                    | Branch(u,ys) ->
                        // otherwise append to the ys and fold along that.
                        inner (fun state -> 
                            cont (folder state v)) 
                                 (Branch(u, NonemptyList.append ls ys))
        inner id t
        

/// A fairly explicit implementation of binary trees.
type BinaryTree<'T when 'T : comparison> =
    | BLeaf of 'T
    | BLeft of 'T * BinaryTree<'T>
    | BRight of 'T * BinaryTree<'T>
    | BBoth of BinaryTree<'T> * 'T * BinaryTree<'T>
    
with
    member x.Count() =
        match x with
        | BLeaf _ -> 1
        | BLeft (_,tL) -> 1 + tL.Count()
        | BRight (_,tR) -> 1 + tR.Count()
        | BBoth (tL,_,tR) -> 1 + tL.Count() + tR.Count()
        
    member x.Root() =
        match x with
        | BLeaf r -> r
        | BLeft (r,_) -> r
        | BRight (r,_) ->r
        | BBoth (_,r,_) -> r

module BinaryTree =
    
    /// A binary tree is complete if every node has exactly 0 or 2 neighbors.
    let rec isComplete (t :BinaryTree<'T>) =
        match t with
        | BLeaf _ -> true
        | BLeft _ -> false
        | BRight _ -> false
        | BBoth(tL,_,tR) -> (isComplete tL) && (isComplete tR)
        
    /// Roughly: if the general tree node A has child B and sibling C, then B
    /// goes on the left and C goes on the right when encoding a general tree
    /// as a binary tree.
    /// See https://en.wikipedia.org/wiki/Binary_tree#Encoding_general_trees_as_binary_trees
    let rec fromTree (t : Tree<'T>) : BinaryTree<'T> =
        match t with
        | Leaf x -> BLeaf x
        | Branch(root,{head=h;rest=[]}) ->
            BLeft(root,fromTree h)
        | Branch(root,{head=h;rest=x::xs}) -> notImpl()
            
        (*
type Tree<'TVertex> =
    | Leaf of 'TVertex
    | Branch of 'TVertex * NonemptyList<Tree<'TVertex>>

        *)
        
    let rec toTree (bt: BinaryTree<'T>) : Tree<'T> =
        match bt with
        | BLeaf x -> Leaf x
        | BLeft(t,bt2) -> Branch(t,NonemptyList.singleton (toTree bt2))
        | BRight(t,bt2) -> Branch(t,NonemptyList.singleton (toTree bt2))
        | BBoth(bt1,t,bt2) -> Branch(t,NonemptyList.append (NonemptyList.singleton (toTree bt2)) (NonemptyList.singleton (toTree bt1)))