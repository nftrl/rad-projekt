module HashTableTests

open Xunit
open HashTable
open HashFunctions
open RandomBytes

[<Fact>]
let hashTableTestMultiplyShift() =
    let rnd = RandomSource("RandomNumbers.data")
    let h = randomMultiplyShift rnd 5
    let table = HashTable(h, 10)
    table.Set 1UL 17UL
    Assert.Equal(table.Get 1UL, 17UL)
    table.Increment 1UL 5UL
    Assert.Equal(table.Get 1UL, 22UL)


[<Fact>]
let hashTableTestMultiplyModPrime() =
    let rnd = RandomSource("RandomNumbers.data")
    let hBig = randomMultiplyModPrime rnd 5   // kald én gang, luk over faste a og b
    let h = fun x -> uint64 (hBig x)          // brug den returnerede funktion
    let table = HashTable(h, 10)
    table.Set 1UL 17UL
    Assert.Equal(table.Get 1UL, 17UL)
    table.Increment 1UL 5UL
    Assert.Equal(table.Get 1UL, 22UL)