open HashFunctions
open StreamGenerator
open RandomBytes
open TestRuntimeOfHashFunctions
open CalculateSquaresums

let simpleExample() =
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

let args = System.Environment.GetCommandLineArgs()
let options = [| 
    "Simple Example", simpleExample;
    "Test Runtime of Hash Functions", TestRuntime;
    "Exact calculation and timing of square sums", CalculateSquaresums
|]

printfn "==========================="
printfn "RAD 2026 - hold 3, gruppe 1"
printfn "==========================="

if Array.length args < 2 then
    printfn "No command line argument given."
    printfn "Run with 'dotnet run [option]'. The options are"
    for i in 0 .. Array.length options - 1 do
        printfn "%i: %A" i (fst options[i])
else
    let success, option = System.Int32.TryParse args[1]
    if success && option < Array.length options then
        snd options[option] ()
    else
        printfn "Wrong option."

