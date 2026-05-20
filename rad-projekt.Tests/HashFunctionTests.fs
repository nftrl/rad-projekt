module HashFunctionTests

open Xunit
open HashFunctions 

[<Fact>]
let ``multiplyShift giver samme resultat som naiv løsning`` () =
    let a = 123UL
    let l = 8
    let h = multiplyShift a l
    let naiv x = (a * x) % (1UL <<< l)

    Assert.Equal(naiv 42UL, h 42UL)
    Assert.Equal(naiv 0UL, h 1UL)
    printfn "multiplyshift test passed"

[<Fact>]
let ``multiplyModPrime giver samme resultat som naiv løsning`` () =
    let a = 123I
    let b = 351I
    let l = 8
    let h = multiplyModPrime a b l
    let naiv x = ((a * x + b) % ((1I <<< 89) - 1I)) % (2I ** l)

    Assert.Equal(naiv 42I, h 42UL)
    Assert.Equal(naiv 0I, h 0UL)
    printfn "multiplyModPrime    test passed"