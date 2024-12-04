open System
open System.IO

let (|Split|) (s: string) = s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
let (|Sorted|) = Array.sort

let result =
    Path.Combine(__SOURCE_DIRECTORY__, "input_part1.txt")
    |> File.ReadAllLines
    |> Array.map (fun (Split xs) -> int xs[0], int xs[1])
    |> Array.unzip
    |> fun (Sorted ls, Sorted rs) ->
        Array.map2 (-) ls rs
        |> Array.map abs
        |> Array.sum

printfn $"The distance is: %d{result}"
