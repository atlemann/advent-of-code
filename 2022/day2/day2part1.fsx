// Day 2 - Part 1

type Hand =
    | Rock
    | Paper
    | Scissors

module Hand =
    let create letter =
        match letter with
        | "A" | "X" -> Some Rock
        | "B" | "Y" -> Some Paper
        | "C" | "Z" -> Some Scissors
        | _ -> None

    let score =
        function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

type RoundResult =
    | Win
    | Draw
    | Loss

module RoundResult =
    let score =
        function
        | Win -> 6
        | Draw -> 3
        | Loss -> 0

let (|Hand|_|) hand =
    Hand.create hand

let (|HandScore|) hand =
    Hand.score hand

let (|RoundResultScore|) round =
    RoundResult.score round

type Round = Round of (Hand * Hand)

module Round =
    let create (line: string) =
        let hands = line.Split " "
        match hands[0], hands[1] with
        | Hand opponent, Hand yours ->
            (opponent, yours)
            |> Round
            |> Some
        | _ ->
            None

    let play (Round (opponent, yours)) =
        let result =
            match opponent, yours with
            | Rock, Rock -> Draw
            | Rock, Paper -> Win
            | Rock, Scissors -> Loss
            | Paper, Rock -> Loss
            | Paper, Paper -> Draw
            | Paper, Scissors -> Win
            | Scissors, Rock ->  Win
            | Scissors, Paper -> Loss
            | Scissors, Scissors -> Draw

        (yours, result)

    let score (HandScore hand, RoundResultScore round) =
        hand + round

"input.txt"
|> System.IO.File.ReadAllLines
|> Seq.choose Round.create
|> Seq.map (Round.play >> Round.score)
|> Seq.sum
|> printfn "Your score: %A"