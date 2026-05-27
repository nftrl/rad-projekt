module CountSketchExperiment

open HashFunctions
open CountSketch
open StreamGenerator
open RandomBytes

let runExperiment: unit =
    let rnd = RandomSource("RandomNumbers.data")
    let n = 1 <<< 20
    let l = 20
    let stream = createStream n l

    let h = randomMultiplyShift rnd l
    let S = getExactSqSum stream h l

    printfn "Opgave 7: Count-Sketch experiment"
    printfn "n = %A" n
    printfn "l = %A" l
    printfn "S = %A" S

    for i in 0 .. 99 do
        let t = 20 // t = 20 passer med n = 2^20
        let g = randomForG rnd
        let (h, s) = hashFunctionsForCountSketch t g 
        let sketch = buildCountSketch t h s stream
        let estimat = estimateSecondMoment sketch

        printfn "%A, %A" i estimat


