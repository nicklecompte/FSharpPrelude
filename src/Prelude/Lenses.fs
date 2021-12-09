namespace Prelude

/// Optics, based largely on Aether: https://github.com/xyncro/aether/
[<AutoOpen>]
module Lenses =
    
    /// A Lens is a pair of a getter and a setter. Typically the 'a is an "outer"
    /// type and the 'b is an "inner" type in a record or class.
    /// But these can also be parameters for general functions.
    /// Getter: takes an 'a and returns its inner 'b.
    /// Setter: takes a 'b and an outer 'a, and returns an 'a with it's 'b value equal to the supplied 'b.
    /// Example: we have a record type Employee = {id=Guid,name=string}. We want a Lens<Employee,string>.
    /// Getter: fun emp -> emp.name
    /// Setter: fun newName emp = {emp with name = newName}
    type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

    /// Compose two lenses. 
    /// If we have a lens from 'a to its inner 'b, and a lens from 'b to its inner 'c,
    /// compose returns the lens from 'a to its (inner) inner 'c.
    let composeLens ((g1,s1):Lens<'a,'b>) ((g2,s2):Lens<'b,'c>) : Lens<'a,'c> =
            (fun a -> g2 (g1 a)), (fun c a -> s1 (s2 c (g1 a)) a)
    
    /// Use a lens to set a value of 'a according to parameters from 'a and 'b
    let setWithLens (a:'a) (b:'b) ((_,setter): Lens<'a,'b>) =
        setter b a

    /// Use a lens to get a 'b value from an input 'a value.
    let getWithLens (a:'a) ((getter,_): Lens<'a,'b>) =
        getter a

    /// Lenses have a natural functor structure.
    let mapWithLens ((getter,setter): Lens<'a,'b>) (f: 'b -> 'b) (a:'a) =
        setter (f (getter a)) a



