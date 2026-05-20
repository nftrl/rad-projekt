module RandomBytes

open System
open System.IO

type RandomSource(fileName: string) =
    let bytes = File.ReadAllBytes(fileName)
    let mutable pos = 0

    let getBytes (n: int) : byte[] =
        if pos + n > bytes.Length then
            failwith $"Not enough random bytes in {fileName}"
        let slice = bytes[pos .. pos + n - 1]
        pos <- pos + n
        slice

    member _.Remaining = bytes.Length - pos

    // Hent en uint64 (8 bytes)
    member _.NextUInt64() : uint64 =
        let raw = getBytes 8
        BitConverter.ToUInt64(raw, 0)

    // Hent en bigint på op til 128 bit
    member _.NextBigInt128(): bigint = 
        let raw = getBytes 16
        bigint raw

