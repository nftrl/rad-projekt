module HashTableFunctional

type HashFunction = uint64 -> uint64
type HashTable<'a> = ((uint64 * 'a) list) []

let rec findByKey (key : uint64) (xs : (uint64 * 'a) list) : 'a option =
    match xs with
    | (x, v) :: xs -> if x = key then Some v else findByKey key xs
    | [] -> None

let get (h : HashFunction) (ht : HashTable<'a>) (x : uint64) : 'a option =
    let key = h x
    findByKey key (ht.[(int key)])

let set (h : HashFunction) (ht : HashTable<'a>) (x : uint64) (v : 'a) : HashTable<'a> =
    ht
