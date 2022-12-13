let testInput = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
let testInput2 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

let input =
    "input.txt"
    |> System.IO.File.ReadLines
    |> Seq.head
    |> fun s -> s.ToCharArray()

let findMarker makerSize input =
    input
    |> Seq.windowed makerSize
    |> Seq.map Array.distinct
    |> Seq.indexed
    |> Seq.find (fun (_, xs) -> xs.Length = makerSize)
    |> fun (i, _) -> i + makerSize

// Part 1
input
|> findMarker 4
|> printfn "Marker index: %A"

// Part 2
input
|> findMarker 14
|> printfn "Message index: %A"