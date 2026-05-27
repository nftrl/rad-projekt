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
    let mutable b = 0I
    while (not finished) do
        a <- rnd.NextBigInt128() &&& p
        b <- rnd.NextBigInt128() &&& p  
        if (a<p) && (b<p) then finished <- true
    multiplyModPrime a b l


// Opgave 4: 4-universal hashfunktion

let p89 = (1I <<< 89) - 1I
// p = 2^89 - 1

let modP89 (y: bigint) =
    // Beregner y mod p89
    let r = y % p89
    if r < 0I then
        r + p89
    else
        r


let g (a0: bigint) (a1: bigint) (a2: bigint) (a3: bigint) (x: uint64) : bigint =
    // Beregner:
    // g(x) = a0 + a1*x + a2*x^2 + a3*x^3 mod p

    let xb = bigint x

    // Horner-form:
    // (((a3*x + a2)*x + a1)*x + a0) mod p
    let mutable y = a3
    for a in [a2; a1; a0] do
        y <- y * x + a
        y <- (y &&& p89) + (y >>> 89)

    if y >= p89 then y - p89 else y


let randomForG (rnd: RandomSource) : (uint64 -> bigint) =
    // Laver én tilfældig g-hashfunktion

    let randomCoef () =
        // Laver ét tilfældigt tal i [0, p-1]
        let mutable a = p89

        while a >= p89 do
            a <- rnd.NextBigInt128() &&& p89

        a

    let a0 = randomCoef()
    let a1 = randomCoef()
    let a2 = randomCoef()
    let a3 = randomCoef()

    g a0 a1 a2 a3
