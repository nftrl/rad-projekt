open HashFunctions
open StreamGenerator

printfn "RAD Rules"

let stream = createStream 10 10

let a = 9163208UL
let b = 476198I

let h_multiplyShift = multiplyShift a 5
let h_multiplyModPrime = multiplyModPrime (bigint a) b 32

printfn "stream:"
for (x, dx) in stream do
    printfn "%i, %i" x dx

printfn "multiply shift"
for (x, _) in stream do
    printfn "%i, %A" x (h_multiplyShift x)

printfn "multiply mod prime"
for (x, _) in stream do
    printfn "%i, %A" x (h_multiplyModPrime x)
