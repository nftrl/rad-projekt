module HashFunctions

open RandomBytes

let multiplyShift (a : uint64) (l : int) (x : uint64) : uint64 =
    // assumes:
    //   a uneven
    //   0 < l < 64
    (a * x) >>> (64 - l)

let multiplyModPrime (a : bigint) (b : bigint) (l : int) (x : uint64) : bigint =
    // assumes:
    //   a < p
    //   b < p
    //   0 < l < 64
    let q : int = 89
    let p : bigint = (2I ** q) - (1I)
    x |> bigint
      |> fun y -> a * y + b                   // ax + b
      |> fun y -> (y &&& p) + (y >>> q)
      |> fun y -> if y >= p then y - p else y // (ax + b) mod p
      |> fun y -> y &&& (2I ** l - 1I)        // (ax + b) mod p mod m

let randomMultiplyShift (rnd : RandomSource) (l : int): (uint64 -> uint64) =
    // a must be odd
    let a = rnd.NextUInt64() ||| 1UL
    multiplyShift a l

let randomMultiplyModPrime (rnd: RandomSource) (l: int): (uint64 -> bigint) =
    // a and b must be <p.
    let p = ((1I <<< 89) - 1I)
    let mutable finished = false
    let mutable a = 0I
    let mutable b     = 0I
    while (not finished) do
        a <- rnd.NextBigInt128() &&& p
        b <- rnd.NextBigInt128() &&& p  
        if (a<p) && (b<p) then finished <- true
    multiplyModPrime a b l