open HashFunctions
open StreamGenerator

let n = 1000
let stream = createStream n 1

let printFunc stream (h : uint64 -> bigint) =
    stream
        |> Seq.map (fun (x,_) -> x)
        |> Seq.map h
        |> Seq.sum
        |> printfn "%A"

printfn "Calculating sum(h(x_i)) for different h"

// 0 < l < 64
let l = 32

printfn "multiply shift"
// a odd r.v.
let a = 12381UL
printFunc stream (fun x -> x |> multiplyShift a l |> bigint)

printfn "multiply mod prime"
// b,c r.v.s less than 2^89 - 1
let b = 9163203I
let c = 476198I
printFunc stream (multiplyModPrime (bigint b) c l)
