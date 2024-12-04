open System
open System.IO

let (|Split|) (s: string) = s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)

//let input = "test_input.txt"
let input = "input.txt"

let reports =
    Path.Combine(__SOURCE_DIRECTORY__, input)
    |> File.ReadAllLines
    |> Array.map (fun (Split xs) -> xs |> Array.map int)

// Part 1

let allIncreasingOrDecreasing xs =
    xs
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> a > b)
    |> Seq.distinct
    |> Seq.length= 1

let diff (a, b) = a - b |> abs

let isInRange x = x >= 1 && x <= 3

let areWithin3 xs =
    xs
    |> Seq.pairwise
    |> Seq.forall (diff >> isInRange)

let resultPart1 =
    reports
    |> Seq.filter (fun level -> allIncreasingOrDecreasing level && areWithin3 level)
    |> Seq.length

printfn $"Result day2 part 1: %d{resultPart1}"

// Part 2
let checkAlternatives xs =
    seq {
        let n = xs |> Array.length

        yield xs
        for i in 0 .. n - 1 ->
            xs |> Array.removeAt i
    }
    |> Seq.exists (fun level -> allIncreasingOrDecreasing level && areWithin3 level)

let resultPart2 =
    reports
    |> Seq.filter checkAlternatives
    |> Seq.length

printfn $"Result day2 part 2: %d{resultPart2}"
