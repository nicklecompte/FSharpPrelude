namespace Prelude

[<RequireQualifiedAccess>]
module Endomorphism =
    
    [<Struct>]
    type Endomorphism<'T> =
        |Endomorphism of ('T -> 'T)
    with
        static member inline Zero = Endomorphism id
        static member inline (+) (endo1,endo2) = Endomorphism (fun t -> (endo2 (endo1 t)))

type Isomorphism<'a,'b> = ('a -> 'b) * ('b -> 'a)

[<RequireQualifiedAccess>]
module Isomorphism =

    /// Compose two isomorphisms in the obvious way.
    let compose ((mapAB,mapBA): Isomorphism<'a,'b>) ((mapBC,mapCB): Isomorphism<'b,'c>) : Isomorphism<'a,'c> =
        ((fun a -> mapBC (mapAB a)),(fun c -> mapBA (mapCB c)))

    /// Given a lens between 'a and 'b and an isomorphism between 'b and 'c,
    /// returns a lens between 'a and 'c.
    let getLensUnderIsomorphism ((getter,setter):Lens<'a,'b>) ((mapBC,mapCB): Isomorphism<'b,'c>) : Lens<'a,'c> =
        ((fun a -> mapBC (getter a)),(fun c a -> (setter (mapCB c) a)))


