// Day 2 - Part 2

type Hand =
    | Rock
    | Paper
    | Scissors

type RoundResult =
    | Win
    | Draw
    | Loss

let (|HandScore|) hand =
    match hand with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let (|RoundResultScore|) round =
    match round with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

module Round =
    let create (line: string) =
        let hands = line.Split " "
        match hands[0], hands[1] with
        | "A", "X" -> Some (Rock, Scissors)
        | "A", "Y" -> Some (Rock, Rock)
        | "A", "Z" -> Some (Rock, Paper)
        | "B", "X" -> Some (Paper, Rock)
        | "B", "Y" -> Some (Paper, Paper)
        | "B", "Z" -> Some (Paper, Scissors)
        | "C", "X" -> Some (Scissors, Paper)
        | "C", "Y" -> Some (Scissors, Scissors)
        | "C", "Z" -> Some (Scissors, Rock)
        | _ -> None

    let play (opponent, yours) =
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