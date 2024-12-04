open System
open System.IO

let (|Split|) (s: string) = s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

let ls, rs =
    Path.Combine(__SOURCE_DIRECTORY__, "input_part2.txt")
    |> File.ReadAllLines
    |> Array.map (fun (Split xs) -> int xs[0], int xs[1])
    |> Array.unzip

let rightCounts =
    rs
    |> Array.countBy id
    |> Map.ofArray

let result =
    ls
    |> Array.sumBy (fun l ->
        rightCounts
        |> Map.tryFind l
        |> Option.map (fun count -> l * count)
        |> Option.defaultValue 0)

printfn $"The distance is: %d{result}"