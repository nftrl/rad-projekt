open HashFunctions
open StreamGenerator
open RandomBytes


let stream = createStream 10 10

let rnd = RandomSource("RandomNumbers.data")

// a must be odd
let a = rnd.NextUInt64() ||| 1UL
let h_multiplyShift = multiplyShift a 5

// a and b must be <p.
let p = ((1I <<< 89) - 1I)
let mutable finished = false
let mutable a_big = 0I
let mutable b     = 0I
while (not finished) do
    a_big <- rnd.NextBigInt128() &&& p
    b <- rnd.NextBigInt128() &&& p  
    if (a_big<p) && (b<p) then finished <- true

let h_multiplyModPrime = multiplyModPrime a_big b 32

printfn "stream:"
for (x, dx) in stream do
    printfn "%i, %i" x dx

printfn "multiply shift"
for (x, _) in stream do
    printfn "%i, %A" x (h_multiplyShift x)

printfn "multiply mod prime"
for (x, _) in stream do
    printfn "%i, %A" x (h_multiplyModPrime x)
