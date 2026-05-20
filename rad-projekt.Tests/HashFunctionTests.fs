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
    Assert.Equal(naiv 0UL, h 0UL)


[<Fact>]
let ``multiplyModPrime giver samme resultat som naiv løsning`` () =
    let a = 123I
    let b = 351I
    let l = 8
    let h = multiplyModPrime a b l
    let naiv x = ((a * x + b) % (bigint.Pow(2I, 89) - 1I)) % (bigint.Pow(2I, l))

    Assert.Equal(naiv 42I, (h 42UL))
    Assert.Equal(naiv 0I,   (h 0UL))    
