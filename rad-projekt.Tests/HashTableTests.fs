module HashTableTests

open Xunit
open HashTable
open HashFunctions
open RandomBytes
open StreamGenerator

[<Fact>]
let hashTableTestMultiplyShift() =
    let rnd = RandomSource("RandomNumbers.data")
    let h = randomMultiplyShift rnd 5
    let table = HashTable(h, 10)

    // Test Set
    table.Set 1UL 17
    Assert.Equal(table.Get 1UL, 17)

    // Test Increment
    table.Increment 1UL 5
    Assert.Equal(table.Get 1UL, 22)

    // Test GetSquareSum
    let table2 = HashTable(h, 10)
    let stream = createStream 100 10
    let mutable sqSum = 0
    for (x, dx) in stream do
        table2.Increment (uint64 x) (dx)
        sqSum <- sqSum + dx*dx
    Assert.Equal(sqSum, table2.GetSquareSum())


[<Fact>]
let hashTableTestMultiplyModPrime() =
    let rnd = RandomSource("RandomNumbers.data")
    let hBig = randomMultiplyModPrime rnd 5   // kald én gang, luk over faste a og b
    let h = fun x -> uint64 (hBig x)          // brug den returnerede funktion
    let table = HashTable(h, 10)

    // Test set
    table.Set 1UL 17
    Assert.Equal(table.Get 1UL, 17)

    // Test increment
    table.Increment 1UL 5
    Assert.Equal(table.Get 1UL, 22)

    // Test GetSquareSum
    let table2 = HashTable(h, 10)
    let stream = createStream 100 10
    let mutable sqSum = 0
    for (x, dx) in stream do
        table2.Increment (uint64 x) (dx)
        sqSum <- sqSum + dx*dx
    Assert.Equal(sqSum, table2.GetSquareSum())