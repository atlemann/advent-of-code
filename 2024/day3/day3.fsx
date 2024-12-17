#r "nuget: FParsec"

open System.IO
open FParsec

let input = "input.txt"

let memory =
    Path.Combine(__SOURCE_DIRECTORY__, input)
    |> File.ReadAllText

let mulParser = pstring "mul" >>. pchar '(' >>. pint32 .>> pchar ',' .>>. pint32 .>> pchar ')' |>> Some

let parseOrSkip = attempt mulParser <|> (skipAnyChar |>> fun _ -> None)

match run (many1 parseOrSkip) memory with
| Success (xs, _, _) ->
    let result =
        xs
        |> List.choose (Option.map (fun (a, b) -> a * b))
        |> List.reduce (+)

    printfn $"Result: {result}"

| Failure (err, _, _) ->
    printfn $"Failure {err}"
