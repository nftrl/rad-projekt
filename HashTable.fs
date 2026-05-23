module HashTable

type HashFunction = uint64 -> uint64

type HashTable(h: HashFunction, l: int) =
    let array: ((uint64 * int) list) [] = Array.create (1 <<< l) []
    let h = h

    member this.GetArray =
        // for testing
        array

    member this.Get (x: uint64): int =
        this.[x]

    member this.Set (x: uint64) (v: int) =
        this.[x] <- v

    member this.Increment (x: uint64) (d: int) =
        let index = int (h x)
        let rec update lst =
            match lst with
            | (y, v)::rest when y = x -> (y, v + d)::rest
            | head::rest -> head::update rest
            | [] -> [(x, d)]
        array.[index] <- update array.[index]

    member this.GetSquareSum () =
        this.GetArray |> Array.fold (fun acc x -> x |> List.fold (fun acc2 (_, v) -> acc2 + v*v) acc) 0


    member this.Item
        with get (x: uint64): int =
            let key = h x
            let list = array.[(int key)]
            match List.tryFind (fun (y,_) -> y = x) list with
            | Some (_, v) -> v
            | None -> 0

        and set (x: uint64) (v: int) =
            let key = h x
            let list = array.[(int key)]
            array.[(int key)] <-
                match List.tryFindIndex (fun (y,_) -> y = x) list with
                | Some 0 -> (x,v)::list.[1..]
                | Some idx -> List.append list.[..(idx-1)] ((x,v)::list.[(idx+1)..])
                | None ->  (x,v)::list
           
