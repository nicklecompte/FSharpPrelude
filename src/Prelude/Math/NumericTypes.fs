namespace Prelude

#if DEBUG
#nowarn "193" // hopefully this is just to get around a v. annoying bug...
#endif

[<AutoOpen>]
/// Catch-all for utility types relating to numeric data - including some
/// specializations with complex numbers.
module NumericTypes =
    

    type Ratio(inval:float) = // todo: Reference type should be OK here since it won't get called very often.
        member x.Value =
            match inval < 1.0 with
            | true -> inval
            | false ->
                match inval < 100.05 with
                | true -> inval / 100.0
                | false -> invalidArg "inval" $"Ratio must be given a value less than 100.05, was given %f{inval}"

    /// TODO Is this worth using? I think we should just be explicit about the 
    /// TODO underlying unmanaged numeric type.
    type Complex< 'T when 'T : equality> =
        abstract member Real : 'T
        abstract member Imaginary : 'T
    //             real: ^T
    //             imaginary: ^T
                
    [<Struct>]
    type complexint = {
        re : int
        im : int
    }
    with
        static member inline Zero : complexint = {re=0;im=0}
        static member inline One : complexint = {re=1;im=0}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexint,b:complexint) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexint,b:complexint) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<int> with
            member x.Real = x.re
            member x.Imaginary = x.im


    [<Struct>]
    type complexint64 = {
        re : int64
        im : int64
    }
    with
        static member inline Zero : complexint64 = {re=0L;im=0L}
        static member inline One : complexint64 = {re=1L;im=0L}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexint64,b:complexint64) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexint64,b:complexint64) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<int64> with
            member x.Real = x.re
            member x.Imaginary = x.im
        

    [<Struct>]
    type complexint16 = {
        re : int16
        im : int16
    }
    with
        static member inline Zero : complexint16 = {re=0s;im=0s}
        static member inline One : complexint16 = {re=1s;im=0s}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexint16,b:complexint16) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexint16,b:complexint16) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<int16> with
            member x.Real = x.re
            member x.Imaginary = x.im


    [<Struct>]
    type complexbigint = {
        re : bigint
        im : bigint
    }
    with
        static member inline Zero : complexbigint = {re=0I;im=0I}
        static member inline One : complexbigint = {re=1I;im=0I}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexbigint,b:complexbigint) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexbigint,b:complexbigint) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<bigint> with
            member x.Real = x.re
            member x.Imaginary = x.im
            
    [<Struct>]
    type complexfloat = {
        re : float
        im : float
    }
    with
        static member inline Zero : complexfloat = {re=0.0;im=0.0}
        static member inline One : complexfloat = {re=1.0;im=0.0}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexfloat,b:complexfloat) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexfloat,b:complexfloat) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<float> with
            member x.Real = x.re
            member x.Imaginary = x.im

    [<Struct>]
    type complexfloat32 = {
        re : float32
        im : float32
    }
    with
        static member inline Zero : complexfloat32 = {re=0.0f;im=0.0f}
        static member inline One : complexfloat32 = {re=1.0f;im=0.0f}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexfloat32,b:complexfloat32) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexfloat32,b:complexfloat32) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<float32> with
            member x.Real = x.re
            member x.Imaginary = x.im

    [<Struct>]
    type complexdecimal = {
        re : decimal
        im : decimal
    }
    with
        static member inline Zero : complexdecimal = {re=0.0m;im=0.0m}
        static member inline One : complexdecimal = {re=1.0m;im=0.0m}
        member inline x.Conjugate =
            {re=x.re;im = -x.im}
        static member inline (+) (a:complexdecimal,b:complexdecimal) = 
                {re = a.re+b.re; im = a.im+b.im}

        static member inline (*) (a:complexdecimal,b:complexdecimal) =
                {re = a.re*a.re - (b.im*b.im);
                 im = (a.im*a.re)+(a.im*a.re)}

        interface Complex<decimal> with
            member x.Real = x.re
            member x.Imaginary = x.im            
    // [<Struct>]
    // /// Generic complex numbers
    // type Complex< ^T when 
    //         ^T : (static member (+) :  ^T * ^T -> ^T ) and 
    //         ^T : (static member (-) : ^T * ^T -> ^T ) and 
    //         ^T : (static member (*) : ^T * ^T -> ^T) and 
    //         ^T : (static member (/) : ^T * ^T -> ^T) and
    //         ^T : (static member Zero : ^T) and
    //         ^T : (static member One : ^T) and
    //       //  ^T : (static member Sqrt : ^T -> ^T) and
    //         ^T : struct and
    //         ^T : equality> = {
    //             real: ^T
    //             imaginary: ^T
    //         }
    // with
