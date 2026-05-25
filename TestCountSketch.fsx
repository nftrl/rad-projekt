#load "RandomBytes.fs"
#load "HashFunctions.fs"
#load "CountSketch.fs"
#load "StreamGenerator.fs"

open RandomBytes
open HashFunctions
open CountSketch
open StreamGenerator

printfn ""
printfn "Opgave 4 test: fixed 4-universal g"

let testKeys =
    [
        123UL
        456UL
        789UL
        1029718409216UL
        959925190656UL
    ]

// Faste koefficienter, så testen er nem at forstå
let a0 = 123I
let a1 = 456I
let a2 = 789I
let a3 = 999I

let fixedG = g a0 a1 a2 a3

for x in testKeys do
    let gx = fixedG x

    printfn "x = %A, g(x) = %A" x gx

    if gx < 0I || gx >= p89 then
        failwithf "Fejl i g: g(%A) = %A, men skal være i [0, p-1]" x gx

printfn "--------------------------------"
printfn "Opgave 4 fixed g test OK"
printfn ""
printfn "Opgave 4 test: random 4-universal g"
printfn "---------------------------------"

let rnd = RandomSource("RandomNumbers.data")

let myG = randomForG rnd

for x in testKeys do
    let gx = myG x

    printfn "x = %A, random g(x) = %A" x gx

    if gx < 0I || gx >= p89 then
        failwithf "Fejl i randomForG: g(%A) = %A, men skal være i [0, p-1]" x gx

printfn "Opgave 4 randomForG test OK"

printfn "------------------------------"
printfn "Test af Opgave 5"
printfn "------------------------------"

// Opgave 5
// t = 5 betyder m = 2^5 = 32
let t = 5
let mBig = 1I <<< t
let m = 1UL <<< t

let h, s = MakckCoutSkt t myG

for x in testKeys do
    let gx = myG x
    let hx = h x
    let sx = s x

    let expectedH = uint64 (gx % mBig)
    let expectedS = 1 - 2 * int (gx >>> 88)

    printfn "x = %A" x
    printfn "  g(x) = %A" gx
    printfn "  h(x) = %A" hx
    printfn "  s(x) = %A" sx

    if hx <> expectedH then
        failwithf "Fejl i h: h(%A) = %A, men forventede %A" x hx expectedH

    if hx >= m then
        failwithf "Fejl: h(%A) = %A, men m = %A" x hx m

    if sx <> expectedS then
        failwithf "Fejl i s: s(%A) = %A, men forventede %A" x sx expectedS

    if sx <> 1 && sx <> -1 then
        failwithf "Fejl: s(%A) = %A. s(x) skal være 1 eller -1." x sx

printfn "Opgave 5 test OK"

printfn ""
printfn "Opgave 6 test: buildCountSketch and estimateSecondMoment"
