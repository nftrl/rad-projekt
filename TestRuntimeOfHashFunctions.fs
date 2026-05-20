open HashFunctions
open StreamGenerator

// a,b random variables less than 2^89 - 1
let a = 9163203UL
let b = 476198I

// 0 < l < 64
let l = 32

let n = 1000
let stream = createStream n 1

let printFunc stream (h : uint64 -> bigint) =
    stream
        |> Seq.map (fun (x,_) -> x)
        |> Seq.map h
        |> Seq.sum
        |> printfn "%A"

printfn "Calculating sum(h(x_i)) for different h"

printfn "multiply shift"
printFunc stream (fun x -> x |> multiplyShift a l |> bigint)

printfn "multiply mod prime"
printFunc stream (multiplyModPrime (bigint a) b l)
