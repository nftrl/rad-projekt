module HashFunctions

let multiplyShift (a : int64) (l : int) (x : int64) : int64 =
    // assumes:
    //   a uneven
    //   0 < l < 64
    (a * x) >>> (64 - l)

let multiplyModPrime (a : bigint) (b : bigint) (l : int) (x : bigint) : bigint =
    // assumes:
    //   a < p
    //   b < p
    //   0 < l < 64
    let q : int = 89
    let p : bigint = (2I ** q) - (1I)
    x |> fun y -> a * y + b                   // ax + b
      |> fun y -> (y &&& p) + (y >>> q)
      |> fun y -> if y >= p then y - p else y // (ax + b) mod p
      |> fun y -> y &&& (2I ** l - 1I)        // (ax + b) mod p mod m
