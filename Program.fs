open HashFunctions
open StreamGenerator
open RandomBytes


let stream = createStream 10 10

let rnd = RandomSource("RandomNumbers.data")

let h_multiplyShift = randomMultiplyShift rnd 5

let h_multiplyModPrime = randomMultiplyModPrime rnd 32

printfn "stream:"
for (x, dx) in stream do
    printfn "%i, %i" x dx

printfn "multiply shift"
for (x, _) in stream do
    printfn "%i, %A" x (h_multiplyShift x)

printfn "multiply mod prime"
for (x, _) in stream do
    printfn "%i, %A" x (h_multiplyModPrime x)
