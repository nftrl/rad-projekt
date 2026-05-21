module HashTable

type HashFunction = uint64 -> uint64

type HashTable(h: HashFunction, l: int) =
    let array: ((uint64 * uint64) list) [] = Array.create (1 <<< l) []
    let h = h

    member this.GetArray =
        // for testing
        array

    member this.Get (x: uint64): uint64 =
        this.[x]

    member this.Set (x: uint64) (v: uint64) =
        this.[x] <- v

    member this.Increment (x: uint64) (d: uint64) =
        // Slow ?
        this.[x] <- this.[x] + d

    member this.Item
        with get (x: uint64): uint64 =
            let key = h x
            let list = array.[(int key)]
            match List.tryFind (fun (y,_) -> y = x) list with
            | Some (_, v) -> v
            | None -> 0UL

        and set (x: uint64) (v: uint64) =
            let key = h x
            let list = array.[(int key)]
            array.[(int key)] <-
                match List.tryFindIndex (fun (y,_) -> y = x) list with
                | Some 0 -> (x,v)::list.[1..]
                | Some idx -> List.append list.[..(idx-1)] ((x,v)::list.[(idx+1)..])
                | None ->  (x,v)::list
           
