open HashFunctions
open StreamGenerator

printfn "RAD Rules"

let stream = createStream 10 10

let a = 9163208UL
let b = 476198I

let h_multiplyShift = multiplyShift a 5
let h_multiplyModPrime = multiplyModPrime (bigint a) b 32

for (x, dx) in stream do
    printfn "stream:"
    printfn "%i, %i" x dx

for (x, _) in stream do
    printfn "multiply shift"
    printfn "%i, %A" x (h_multiplyShift x)

for (x, _) in stream do
    printfn "multiply mod prime"
    printfn "%i, %A" x (h_multiplyModPrime x)
