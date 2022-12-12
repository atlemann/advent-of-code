// Day 4

let split (input: string) =
    input.Split ","

let toSectionIds (input: string) =
    let range =
        input.Split "-"
        |> Array.map int

    [| range[0] .. range[1] |]
    |> Set.ofArray


let sectionsPairs =
    "input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.map (fun pair ->
        pair
        |> split
        |> Array.map toSectionIds)

// Part 1

let overlapsFull a b =
    a |> Set.isSubset b ||
    b |> Set.isSubset a

sectionsPairs
|> Seq.filter (fun sections ->
    sections[0]
    |> overlapsFull sections[1])
|> Seq.length
|> printfn "Assignment pairs with full overlap: %d"

// Part 2

let overlaps a b =
    Set.intersect a b
    |> Seq.isEmpty
    |> not

sectionsPairs
|> Seq.filter (fun sections ->
    sections[0]
    |> overlaps sections[1])
|> Seq.length
|> printfn "Assignment pairs with overlap: %d"
