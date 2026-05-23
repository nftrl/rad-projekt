module CalculateSquaresums
// Opgave 3

open HashTable
open RandomBytes
open HashFunctions
open StreamGenerator
open System.Diagnostics

let getTimeAndSqSum (h: HashFunction) (n: int) (l: int) : (int64 * uint64) =
    let table = HashTable(h, l)
    let stream = createStream n l
    let timer = new Stopwatch()
    timer.Start()
    for (x, dx) in stream do
        table.Increment (uint64 x) (dx)
    timer.Stop()
    (timer.ElapsedMilliseconds, table.GetSquareSum())



let CalculateSquaresums() =
    let n = 2<<<20   
    let max_l = 20
    let rnd = RandomSource("RandomNumbers.data")

    printfn "n=%d" n
    for l in 1 .. max_l do
        let h_ms = randomMultiplyShift rnd l
        let (ms_time, ms_sqSum) = getTimeAndSqSum h_ms n l
        let hBig = randomMultiplyModPrime rnd l
        let h_mmp = fun x -> uint64 (hBig x)  
        let (mmp_time, mmp_sqSum) = getTimeAndSqSum h_mmp n l
        printfn "l: %d. Multiplyshift: %A ms (S=%A). Multiplymodprime: %A ms (S=%A)" l ms_time ms_sqSum mmp_time mmp_sqSum





