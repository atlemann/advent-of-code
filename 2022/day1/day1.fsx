// Day 1

let (|Calories|NextElf|) current =
    match current with
    | "" -> NextElf
    | x -> Calories (int x)

module Calories =
    let getSorted caloriesList =
        let rec loop acc currentCount caloriesList =
            match caloriesList with
            | [] ->
                acc |> List.sortDescending

            | NextElf :: tail ->
                loop (currentCount :: acc) 0 tail

            | Calories cal :: tail ->
                loop acc (cal + currentCount) tail

        loop [] 0 caloriesList

let sortedCounts =
    "input.txt"
    |> System.IO.File.ReadAllLines
    |> Array.toList
    |> Calories.getSorted

// Part 1: Find most calories

sortedCounts
|> List.head
|> printfn "Elf with most: %d"

// Part 2: Find sum of top 3

sortedCounts
|> List.take 3
|> List.sum
|> printfn "Sum of top 3: %A"