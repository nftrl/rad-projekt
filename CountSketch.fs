module CountSketch

open StreamGenerator
open HashFunctions
open HashTable
open RandomBytes


let getSketch (stream: seq<uint64 * int>) (m: int) = 
    // NOT the right hash functions !!!!!!
    let h = fun x -> x % (uint64 m)
    let s = fun x -> 2*((int x) % 2) - 1
    let C : int64 [] = Array.zeroCreate m
    let mutable index = 0
    for (x, dx) in stream do
        index <- int (h x)
        C[index] <- C[index] + int64 ((s x) * dx)
    C |> Array.fold (fun acc x -> acc + x*x) 0L

let getExactSqSum (stream: seq<uint64 * int>) (h: HashFunction) (n: int) (l: int) : uint64 =
    let table = HashTable(h, l)
    let stream = createStream n l
    for (x, dx) in stream do
        table.Increment (uint64 x) (dx)

    table.GetSquareSum()

let runCountSketch () =
    let n = 1<<<20
    let l = 10
    let m = 1000
    let stream = createStream n l
    let rnd = RandomSource("RandomNumbers.data")
    let h = randomMultiplyShift rnd l

    let X = getSketch stream m
    printfn "Result from CountSketch: %A" X

    let S = getExactSqSum stream h n l
    printfn "Exact result:            %A" S
