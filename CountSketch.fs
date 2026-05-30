module CountSketch

open StreamGenerator
open HashFunctions
open HashTable
open RandomBytes
open System.Diagnostics

let getExactSqSum (stream: seq<uint64 * int>) (h: HashFunction) (l: int) : uint64 =
    let table = HashTable(h, l)
    for (x, dx) in stream do
        table.Increment (uint64 x) (dx)

    table.GetSquareSum()


// Opgave 5
let hashFunctionsForCountSketch (t: int) (g: uint64 -> bigint): (uint64 -> uint64) * (uint64 -> int) =
    if t < 0 || t > 64 then
        invalidArg "t" "t should be between 0 and 64"

    let m = 1I <<< t
    
    let h (x: uint64): uint64 =
        uint64 (g x &&& (m - 1I)) // h(x) = g(x) mod m
        
    let s (x: uint64): int =
        1 - 2 * int (g x >>> 88) // s(x) = 1 - 2 * floor(g(x) / 2^88)
        
    (h, s)
    
// t, som bestemmer størrelsen af arrayet.

// h, som bestemmer hvilken counter vi bruger.

// s, som bestemmer plus eller minus.

// stream, som er en sekvens af par (x, d) 
// Opgave 6. Vi bygge count-Sketch arrayet c ud fra streem
let buildCountSketch (t: int) (h: uint64 -> uint64) (s: uint64 -> int) (stream: seq<uint64 * int>): bigint array= 
    if t < 0 || t > 30 then 
        invalidArg "t" "t should be between 0 and 30"
    
    let m = 1 <<< t


    // C[0], ..., C[m-1] starter på 0
    let C = Array.create m 0I

    // den løbe igennem streem
    for (x, d) in stream do
        let index = int (h x)
        let sign = s x

        // C[h(x)] <- C[h(x)] + s(x) * d
        C[index] <- C[index] + bigint (sign *d)

    C


// Beregner estimatet X = sum_y C[y]^2
let estimateSecondMoment (c: bigint array) : bigint =
    c 
    |> Array.sumBy (fun cy -> cy * cy)



let runCountSketch () =
    let n = 1<<<20
    let l = 10
    let t = 10
    let m = 1<<<t
    let stream = createStream n l
    let rnd = RandomSource("RandomNumbers.data")
    
    let g = randomForG rnd
    let (h,s) = hashFunctionsForCountSketch t g
    let c = buildCountSketch t h s stream
    let X = estimateSecondMoment c
    printfn "Result from CountSketch: %A" X

    let h = randomMultiplyShift rnd l
    let S = getExactSqSum stream h l
    printfn "Exact result:            %A" S


// Opgave 7 runCountSketchExp 

let squareSum (l: int) (hExact: uint64 -> uint64) (stream: seq<uint64 * int>) : bigint =
    let table = HashTable(hExact, l)

    for (x, d) in stream do
        table.Increment x d

    bigint (table.GetSquareSum())


let runCountSketchExp
    (rnd: RandomSource)
    (n: int)
    (l: int)
    (t: int)
    (runs: int)
    : bigint * bigint array * bigint array * float =

    // Samme stream bruges i alle forsøg
    let stream = createStream n l |> Seq.toArray

    // Eksakt værdi S med chaining
    let hExact = randomMultiplyShift rnd l
    let S = squareSum l hExact stream

    // Count-Sketch køres runs gange
    let estimates =
        [|
            for i in 1 .. runs do
                // Ny random g hver gang
                let g = randomForG rnd

                // h og s fra Opgave 5
                let hCS, sCS = hashFunctionsForCountSketch t g

                // C fra Opgave 6
                let C = buildCountSketch t hCS sCS stream

                // X = sum_y C[y]^2
                let X = estimateSecondMoment C

                yield X
        |]

    let sortedEstimates =
        estimates |> Array.sort

    let mse =
        estimates
        |> Array.averageBy (fun X ->
            let diff = float (X - S)
            diff * diff
        )

    (S, estimates, sortedEstimates, mse)

let runCountSketchExpTimed
    (rnd: RandomSource)
    (n: int)
    (l: int)
    (t: int)
    (runs: int)
    (inputStream: seq<uint64 * int>)
    : bigint * bigint array * bigint array * float * int64 * int64 =

    // Samme stream bruges i alle forsøg
    let stream = inputStream |> Seq.toArray

    let timer = new Stopwatch()

    // Eksakt værdi S med chaining
    timer.Start()
    let hExact = randomMultiplyShift rnd l
    let S = squareSum l hExact stream
    timer.Stop()
    let timeExact = timer.ElapsedMilliseconds

    timer.Reset()
    timer.Start()
    // Count-Sketch køres runs gange
    let estimates =
        [|
            for i in 1 .. runs do
                // Ny random g hver gang
                let g = randomForG rnd

                // h og s fra Opgave 5
                let hCS, sCS = hashFunctionsForCountSketch t g

                // C fra Opgave 6
                let C = buildCountSketch t hCS sCS stream

                // X = sum_y C[y]^2
                let X = estimateSecondMoment C

                yield X
        |]
    timer.Stop()
    let timePerEstimate = timer.ElapsedMilliseconds / (int64 runs)

    let sortedEstimates =
        estimates |> Array.sort

    let mse =
        estimates
        |> Array.averageBy (fun X ->
            let diff = float (X - S)
            diff * diff
        )

    (S, estimates, sortedEstimates, mse, timeExact, timePerEstimate)

// Opgave 7: median-trick
let medianTrick (estimates: bigint array) : bigint array =
    if estimates.Length < 99 then
        invalidArg "estimates" "Need at least 99 estimates"

    estimates
    |> Array.take 99
    |> Array.chunkBySize 11
    |> Array.map (fun group ->
        let sortedGroup = group |> Array.sort
        sortedGroup.[5]
    )
    |> Array.sort
