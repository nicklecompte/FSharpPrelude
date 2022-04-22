/// Prelude contains
namespace Prelude

open System
open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("Tests")>]
do()


[<AutoOpen>]
/// Very simple helpers/utilities/shortcuts, used throughout the entire program.
/// Everything in this specific submodule should satisfy the following criteria:
///     - the functions here could reasonably be used in any F# program
///     - they are functions that could reasonably be said to be "missing" from
///     the F# standard library or are part of STDLIB in similar languages
///     - they are purely functional and thread-safe
module Prelude =

    /// Map functor implemented over a pair.
    /// Example: `mapPair (fun x -> 2x) (5,6) => (10,12)`.
    let inline mapPair (f : 'a -> 'b) ((a1,a2) : 'a*'a) : 'b*'b =
        (f a1, f a2)

    /// Map functor implemented over a triple.
    /// Example: `mapPair (fun x -> 2x) (5,6,7) => (10,12,14)`.
    let inline mapTriple (f : 'a -> 'b) ((a1,a2,a3) : 'a*'a*'a) : 'b*'b*'b =
        (f a1, f a2, f a3)

    let pi = Math.PI
    let twoPi = 2.0*pi
    

    /// Simple struct for ordered types 'T.
    /// The entire point of this is to have a safer program-wide abstraction for
    /// conditional logic depending on the ordering of 'T.
    /// TODO: Consider performance implications of requiring `when 'T : struct`. 
    [<Struct>]
    type Bounds<'T> when 'T : comparison = {
        min : 'T
        max : 'T
    }

    /// Simple and widely-used shortcut for raising a not implemented exception.
    let inline notImpl () : 'T =
        raise (NotImplementedException())

    /// Flips a pair (a,b) to (b,a). Baffling that this isn't built-in...
    let inline flipTuple ((a,b) : 'a*'b) : 'b*'a =
        (b,a)


    /// Redoing the Choice<'T, 'U> type from F# core library.
    /// The Either<'T,'U> is used for cases where 'T isn't necessarily "better"
    /// than `U and is not intended as a replacement for 'Result<'OK,'Error>`.
    /// 'Result<'OK,'Error>-type types should be used for computations that can
    /// fail "badly", whereas `Either<'T,'U>` is a simple tool for polymorphic
    /// programming. 
    /// As a comment on this implementation, our motivation is driven by
    /// semantics, specifically coming from a type-driven-development philosophy.
    /// The language-level isomorphism between the types is not intended to be
    /// a program-level ambiguity - the name is important.
   type Either<'T, 'U> =
        | Left of 'T
        | Right of 'U

    /// A **left**-biased computation expression for Either<'L,'R>.
    type EitherBuilderL() =
        member _.Return x = Left x

        member _.ReturnFrom x = x
        /// Left-biased bind.
        member _.Bind (x: Either<'a,'b>) (f: 'a -> Either<'c,'b>) : Either<'c,'b> =
            match x with
            | Left a -> f a
            | Right b -> Right b
            
    /// A **right**-biased computation expression for Either<'L,'R>.
    type EitherBuilderR() =
        member _.Return x = Right x

        member _.ReturnFrom x = x
        /// Left-biased bind.
        member _.Bind (x: Either<'a,'b>) (f: 'b -> Either<'a,'c>) : Either<'a,'c> =
            match x with
            | Left a -> Left a
            | Right b -> f b

       
    /// A special Either type where the left branch is an IComparable, and the
    /// LeftComparableEither inherits its ordering.
    [<CustomEquality;CustomComparison>]
    type LeftComparableEither<'T, 'U when 'T:> IComparable<'T> and 'T: equality> =
        | OrderedLeft of 'T
        | UnorderedRight of 'U
    with
        interface IEquatable<LeftComparableEither<'T, 'U>> with
            // Equality inherited from 'T
            member x.Equals other =
                match (x,other) with
                | OrderedLeft l, OrderedLeft r -> l = r
                | OrderedLeft _, UnorderedRight _ -> false
                | UnorderedRight _, OrderedLeft _ -> false
                // note that we don't have equality on 'U so everything is equal
                | UnorderedRight _, UnorderedRight _ -> true
        interface IComparable<LeftComparableEither<'T, 'U>> with // <LeftComparableEither<'T, 'U>> with
            member x.CompareTo other' =
                match (x,other') with
                    | OrderedLeft newX, OrderedLeft newY -> (newX :> IComparable<'T>).CompareTo newY
                    | UnorderedRight _, OrderedLeft _ -> 1
                    | OrderedLeft _, UnorderedRight _ -> -1
                    | UnorderedRight _, UnorderedRight _ -> 0


    [<RequireQualifiedAccess>]
    module Either =
        let getLeft either =
            match either with
            | Left x -> Some x
            | Right _ -> None
        let getRight either =
            match either with
            | Left _ -> None
            | Right x -> Some x

        let mapL (a:Either<'Left1,'Right>) (f: 'Left1 -> 'Left2) : Either<'Left2,'Right> =
            match a with
            | Left x -> Left (f x)
            | Right y -> Right y

            ///// Right-biased bind.
            //member __.Bind (x: Either<'a,'b>) (f: 'b -> Either<'a,'c>) : Either<'a,'c> =
            //    match x with
            //    | Left a -> Left a
            //    | Right b -> f b


    type NonemptyList<'T> = {
        head : 'T
        rest : 'T list
    }
    with
        member x.ToList = x.head :: x.rest

        member x.Tail : NonemptyList<'T> option =
            match x.rest with
            | [] -> None
            | l :: ls -> Some {head = l; rest = ls}

    [<RequireQualifiedAccess>]
    module NonemptyList =
         
        let singleton x = {head = x; rest = []}
        
        let head x = x.head

        let tail (x:NonemptyList<'T>) = x.Tail

        let append xs ys =
            match xs.rest with
            | [] -> {head=xs.head;rest=ys.head::ys.rest}
            | _ -> {head=xs.head;rest=List.append xs.rest (ys.head::ys.rest)}

        let map mapping nList =
            {head = (mapping nList.head);rest = (List.map mapping nList.rest)}
            
        let fold foldFn initialState nList =
            let first = foldFn initialState nList.head
            List.fold foldFn first nList.rest
            
        let ofList x = match x with | [] -> ValueNone | y::ys -> ValueSome {head = y; rest = ys}

        let toList x = x.head :: x.rest