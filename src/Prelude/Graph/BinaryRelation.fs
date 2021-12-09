namespace Prelude.Graph

open Prelude

type IBinaryRelation<'TVertex> =
    abstract member Domain : unit -> 'TVertex list
    abstract member AsPairList: unit -> ('TVertex* 'TVertex) list


/// Signature / implementation of a very general graph
type BinaryRelation<'T when 'T : equality> = {
    /// The "vertices" of the graph.
    domain : seq<'T>
    /// Adjacency is a function which takes a vertex and returns a list of its
    /// neighbors.
    relations: 'T -> 'T list
}
with
    interface IBinaryRelation<'T> with
        member x.Domain() = x.domain |> Seq.toList
        member x.AsPairList() =
            x.domain
//                |> Seq.map(fun a -> (a,x.relations a))
            |> Seq.toList
            |> List.collect(fun a -> x.relations a |> List.map(fun b -> (a,b)))

/// Certain descriptive properties for binary relations
type BinaryRelationProperty<'T> =
    | Function // in the mathematical sense
    | Symmetric
    | Reflexive
    | Transitive
    | Injective
    | Combination of BinaryRelationProperty<'T> list
    /// If a BinaryRelation satisfies a property except for a finite number of
    /// points, then these points are enumerated in `exceptions`
    | WithException of BinaryRelationProperty<'T> * exceptions:'T list

[<RequireQualifiedAccess>]
/// TODO poorly optimized but I am still thinking about the data structure
module BinaryRelation =

    /// Helper for recursively going through the domain of a binary relation
    /// Given a BinaryRelation<'T> r, returns a BinaryRelation q such that
    /// r.relations = q.relations, and q.domain = tail of r.domain
    /// if r is nonempty, or just the empty sequence otherwise.
    let inline private tailDomain (r:BinaryRelation<_>) =
        match Seq.isEmpty r.domain with
        | true -> r
        | false -> {domain = Seq.tail r.domain;
                    relations = r.relations}

    let rec isFunction (relation:BinaryRelation<'T>) =
        let dom = relation.domain
        match Seq.isEmpty dom with
        | true -> true
        | false ->
            let first = Seq.head dom
            let image = relation.relations first 
            match image with
            | [] -> false
            | [_] -> isFunction (tailDomain relation)
            | _::_::_ -> false

    let rec isReflexive (relation: BinaryRelation<'T>) =
        let dom = relation.domain
        match Seq.isEmpty dom with
        | true -> true
        | false -> 
            let h = Seq.head dom
            match List.contains h (relation.relations h) with
            | true -> isReflexive (tailDomain relation)
            | false -> false

    let adjacencyPreservingMap mapping relation = notImpl()
