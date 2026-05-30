module CountSketchExperiment

open HashFunctions
open CountSketch
open StreamGenerator
open RandomBytes
open Plotly.NET

let runExperiment (): unit =
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

let makePlots 
    (medians: bigint array)
    (estimates: bigint array)
    (sortedEstimates: bigint array)
    (t: int)
    (S: bigint)
    : unit =


    if estimates.Length <> 100 then
        failwithf "Fejl: Der skal være 100 estimater, men fik %A" estimates.Length

    if medians.Length <> 9 then
        failwithf "Fejl: Der skal være 9 medianer, men fik %A" medians.Length

    printfn "Laver plots for t=%A" t

    // Plot 1: 100 sorterede estimater
    let xEst = [| 1 .. sortedEstimates.Length |]
    let yEst = sortedEstimates |> Array.map float
    let sLineEst = Array.init sortedEstimates.Length (fun _ -> float S)

    let chartEstimates =
        [
            Chart.Point(xEst, yEst, Name = "Sorted estimates")
            Chart.Line(xEst, sLineEst, Name = "Exact S")
        ]
        |> Chart.combine
        |> Chart.withTitle $"Opgave 8: 100 sorted Count-Sketch estimates t={t}"
        |> Chart.withXAxisStyle("Index")
        |> Chart.withYAxisStyle("Estimate")

    Chart.saveHtml $"opgave8_estimates_t_{t}.html" chartEstimates

    // Plot 2: 9 medianer
    let xMed = [| 1 .. medians.Length |]
    let yMed = medians |> Array.map float
    let sLineMed = Array.init medians.Length (fun _ -> float S)

    let chartMedians =
        [
            Chart.Point(xMed, yMed, Name = "Median estimates")
            Chart.Line(xMed, sLineMed, Name = "Exact S")
        ]
        |> Chart.combine
        |> Chart.withTitle $"Opgave 8: 9 sorted medians t={t}"
        |> Chart.withXAxisStyle("Index")
        |> Chart.withYAxisStyle("Median estimate")

    Chart.saveHtml $"opgave8_medians_t_{t}.html" chartMedians

    printfn "Plots saved:"
    printfn $"  opgave8_estimates_t_{t}.html"
    printfn $"  opgave8_medians_t_{t}.html"

let runExperimentsTimed (): unit =
    let rnd = RandomSource("RandomNumbers.data")
    let n = 1 <<< 20
    let l = 20                             
    let t_values = [5; 10; 15]
    let runs = 100
    let stream = createStream n l
    for t in t_values do
        printfn ""
        printfn "------------------------------------------"
        printfn "Running count sketch with t=%A" t    
        let (S, estimates, sortedEstimates, mse, timeExact, timePerEstimate) =
            runCountSketchExpTimed rnd n l t runs stream

        let theoretical_variance = 2I*S*S/(1I<<<t)
        let mean_estimate = (estimates |> Array.sum) / (bigint runs)

        printfn "Exact S = %A" S
        printfn "Time for exact calculation = %A" timeExact
        printfn "Time per estimate = %A" timePerEstimate  
        printfn "Number of estimates = %A" estimates.Length 
        printfn "MSE = %A" mse
        printfn "Theoretical variance 2S²/m = %A" theoretical_variance
        printfn "Mean of all estimates = %A" mean_estimate

        let medians = medianTrick estimates
        printfn "Medians = %A" medians
        makePlots medians estimates sortedEstimates t S

    


