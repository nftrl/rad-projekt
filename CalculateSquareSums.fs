module CalculateSquaresums
// Opgave 3

open HashTable
open RandomBytes
open HashFunctions
open StreamGenerator
open System.Diagnostics

let CalculateSquaresums() =
    let n = 10      
    let l = 3
    let rnd = RandomSource("RandomNumbers.data")
    let h = randomMultiplyShift rnd l
    let table = HashTable(h, l)
    let stream = createStream n l

    let timer = new Stopwatch()
    timer.Start()
    for (x, dx) in stream do
        table.Increment (uint64 x) (dx)
    timer.Stop()
    printfn "Time elapsed %A ms" timer.ElapsedMilliseconds

    printfn "%A" (table.GetSquareSum())



