module CalculateSquaresums
// Opgave 3

open HashTable
open RandomBytes
open HashFunctions
open StreamGenerator
open System.Diagnostics
open System.Collections.Generic
open Plotly.NET

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
    let n = 1<<<20  
    let min_l = 1
    let max_l = 20 
    let rnd = RandomSource("RandomNumbers.data")
    let l_values = ResizeArray([])
    let mmp_times = ResizeArray([])
    let ms_times = ResizeArray([])

    printfn "n=%d" n
    for l in min_l .. max_l do
        let h_ms = randomMultiplyShift rnd l
        let (ms_time, ms_sqSum) = getTimeAndSqSum h_ms n l
        let hBig = randomMultiplyModPrime rnd l
        let h_mmp = fun x -> uint64 (hBig x)  
        let (mmp_time, mmp_sqSum) = getTimeAndSqSum h_mmp n l
        printfn "l: %d. Multiplyshift: %A ms (S=%A). Multiplymodprime: %A ms (S=%A)" l ms_time ms_sqSum mmp_time mmp_sqSum
        l_values.Add(l)
        mmp_times.Add(mmp_time)
        ms_times.Add(ms_time)

    let chart =
        [
            Chart.Point(l_values, mmp_times, Name = "MultiplyModPrime")
            Chart.Point(l_values, ms_times, Name = "MultiplyShift")
        ]
        |> Chart.combine
        |> Chart.withTitle $"Opgave 3: Køretid for eksakt beregning"
        |> Chart.withXAxisStyle("l-værdi")
        |> Chart.withYAxisStyle("Køretid/ms")

    Chart.saveHtml $"opgave3.html" chart







