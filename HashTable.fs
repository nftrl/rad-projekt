module HashTable

type HashFunction = uint64 -> uint64

let rec findInList (key : uint64) (xs : (uint64 * 'a) list) : 'a option =
    match xs with
    | (y,v) :: ys -> if y = key then Some v else findInList key xs
    | [] -> None

type HashTable(h : HashFunction, l : int) =
    let array : ((uint64 * uint64) list) [] = Array.create (1 <<< l) []
    let h = h

    member this.Item
        with get (x : uint64) : uint64 =
            let key_x = h x
            match findInList key_x array.[(int key_x)] with
            | Some v -> v
            | None -> 0UL
