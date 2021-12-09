namespace Prelude

type IProbabilityDistribution<'T> =
//        abstract member DensityFunction: seq<float*'T>
    abstract member Sample : unit -> 'T

        
type IBoundedProbabilityDistribution<'T when 'T: comparison> =
    abstract member GetSampleBetween: Bounds<'T> -> 'T
    inherit IProbabilityDistribution<'T>

type Probability = float
            

[<RequireQualifiedAccess>]    
module Probability =

    type Seed =
        | CurrentTime
        | ProvidedSeed of int

    /// XORShift random number generator.
    /// See https://github.com/pomma89/Troschuetz.Random/blob/master/src/Troschuetz.Random/Generators/XorShift128Generator.cs
    type XORShiftRandomNumberGenerator(seed: Seed) =
        /// One of the seeds used in number generation.
        /// The value of this static unsigned long is 521288629, left shifted by 32 bits.
        static member private XSeed : uint64 = 521288629UL <<< 32
        /// One of the seeds used in number generation.
        /// The value of this static unsigned long is 362436069.
        static member private YSeed : uint64 = 4101842887655102017UL
            

    //let checkDistributionWithSuppliedTolerance tolerance (distribution : #IProbabilityDistribution<'T>) =
    //    let sumProbability = distribution.DensityFunction |> Seq.sumBy fst
    //    sumProbability < 1.0 + tolerance && sumProbability > 1.0 - tolerance

    type BoundedUniformIntegralDistribution(min, max) =
        let innerRand = System.Random()
        interface IBoundedProbabilityDistribution<int> with
            //member __.DensityFunction = 
            //    let probability = 
            //        if min = max then 1.0
            //        else 1.0/(float(max - min + 1))
            //    seq{for i in min..max do yield (probability,i)}
            member _.Sample() = innerRand.Next(min,max)
            member x.GetSampleBetween b = innerRand.Next(b.min,b.max)

    /// An IProbability distribution corresponding to
    type NormalDistribution(average, standardDeviation) =
            
        interface IProbabilityDistribution<float> with
            //member __.DensityFunction = failwith "not done"
            member _.Sample() = failwith "not done"


/// Interface representing a non-standard dice. 
/// For a normal x-sided dice with distinct sides and equal weight, just use Dice.rollOneDice.
/// Examples of usage: a 6-sided dice where 3 of the faces are 0.
/// 'TNonintegralFace is a type representing something besides a numeric value.
/// For instance, a 6-sided dice where instead of zero we have a catastrophic failure, e.g. "grenade blows up in your hand."
type ICustomDice<'TNonIntegralFace> = 
    /// Dice have the obvious probability distribution associated with them.
    inherit IProbabilityDistribution<'TNonIntegralFace>
    /// The sides of the dice with their probability.
    /// For instance, a 6-sided dice with 3 zeros can be represented as [(0,0.5);(1,0.18);(2,0.18);(3,0.18)|].
    /// A 6-sided dice weighted towards 6 can be represented as [(0,0.13);(1,0.13);(2,0.13);(3,0.13);(4,0.13);(5,0.13);(6,0.28)|]
    /// It is the responsibility of the implementing type to make sure x.Sides |> List.sum snd = 1.0!
    abstract member Sides: (Either<int, 'TNonIntegralFace>*DiceProbability) list
    /// Typically samples the IProbabilityDistribution<'T> above, or the Sample() implementation runs Roll().
    abstract member Roll : unit -> Either<int, 'TNonIntegralFace>
and
    DiceProbability = float

/// Helpers involving ICustomDice factories, or processing dice rolls.
[<RequireQualifiedAccess>]
module Dice =

    /// Take a LEFT-biased EitherBuilderL for the CustomDice. As an implementation
    /// detail we know 
    let private either =  EitherBuilderL()

    let rollOneDice (sides:int) =
        let rand = System.Random()
        rand.Next(1,sides)

    let rollCustomDice (d: #ICustomDice<'T>) : Either<int, 'T> =
            let random = System.Random()
            let cdf = 
                let mutable p = 0.0
                d.Sides |> List.map(fun a -> (p + (snd a),fst a))
            let dbl = random.NextDouble()
            (cdf |> List.find(fun (a,_) -> a >= dbl) |> snd)
