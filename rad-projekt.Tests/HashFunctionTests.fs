module HashFunctionTests

open Xunit
open HashFunctions 

[<Fact>]
let ``test af multiplyShift`` () =
    let a = 123UL
    let l = 8
    let h = multiplyShift a l
    // test overflow does not cause an error
    let _ = h System.UInt64.MaxValue
    // test deterministic
    Assert.Equal(h 22UL, h 22UL)

[<Fact>]
let ``multiplyModPrime giver samme resultat som naiv løsning`` () =
    let a = 123I
    let b = 351I
    let l = 8
    let h = multiplyModPrime a b l
    let naiv x = ((a * x + b) % (bigint.Pow(2I, 89) - 1I)) % (bigint.Pow(2I, l))

    Assert.Equal(naiv 42I, (h 42UL))
    Assert.Equal(naiv 0I,   (h 0UL))    
