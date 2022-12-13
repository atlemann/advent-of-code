// Day 5
open System
open System.Text.RegularExpressions
open System.Collections
open System.Collections.Generic

let rec parseInput initAcc movesAcc lines =
    match lines with
    | [] ->
        let initial =
            initAcc
            |> Seq.skip 1
            |> Seq.collect (fun (line: string) ->
                line.ToCharArray()
                |> Seq.indexed
                |> Seq.filter (fun (_, c) ->
                    Char.IsWhiteSpace(c) |> not &&
                    c <> '[' &&
                    c <> ']'))
            |> Seq.groupBy fst
            |> Seq.mapi (fun i (_, cs) ->
                let crates =
                    cs
                    |> Seq.map snd
                    |> Seq.rev
                    |> Seq.toList
                (i + 1, crates))
            |> Map.ofSeq

        initial, movesAcc

    | "" :: moves ->
        parseInput initAcc moves []

    | h :: tail ->
        parseInput (h :: initAcc) movesAcc tail

let moveRegex = Regex("move (?<count>[0-9]+) from (?<source>[0-9]+) to (?<destination>[0-9]+)")

type Move =
    { Count: int
      Source: int
      Destination: int }

let parseMove line =
    let m = moveRegex.Match line
    if m.Success then
        { Count = m.Groups["count"].Value |> int
          Source = m.Groups["source"].Value |> int
          Destination = m.Groups["destination"].Value |> int }
        |> Some
    else
        None

let (|ParsedMoves|) = Seq.choose parseMove

module Mutable =
    // Part 1

    let part1 state moves =
        let state =
            state
            |> Map.map (fun _ -> Seq.rev >> Stack<char>)

        for move in moves do
            let source = state |> Map.find move.Source
            let destination = state |> Map.find move.Destination
            for _ in 0 .. move.Count - 1 do
                destination.Push(source.Pop())

        state
        |> Map.toArray
        |> Array.map (fun (_, crates) -> crates.Peek())
        |> String

    // Part 2

    let part2 state moves =
        let state =
            state
            |> Map.map (fun _ -> Seq.rev >> Stack<char>)

        for move in moves do
            let source = state |> Map.find move.Source
            let destination = state |> Map.find move.Destination
            [
                for _ in 0 .. move.Count - 1 ->
                    source.Pop()
            ]
            |> List.rev
            |> List.iter destination.Push

        state
        |> Map.toArray
        |> Array.map (fun (_, crates) -> crates.Peek())
        |> String

module Immutable =
    let collectTopCrates map =
        map
        |> Map.toArray
        |> Array.map (fun (_, crates) -> crates |> List.head)
        |> String

    let part1 state moves =
        let folder state move =
            let source = state |> Map.find move.Source
            let destination = state |> Map.find move.Destination

            let newSource, newDest =
                Seq.init move.Count id
                |> Seq.fold (fun (src, dst) _ ->
                    let toMove = src |> List.head
                    let leftOver = src |> List.tail
                    (leftOver, toMove :: dst))
                    (source, destination)

            state
            |> Map.change move.Source (fun _ -> Some newSource)
            |> Map.change move.Destination (fun _ -> Some newDest)

        moves
        |> Seq.fold folder state
        |> collectTopCrates

    let part2 state moves =
        let folder state move =
            let source = state |> Map.find move.Source

            state
            |> Map.change move.Source (fun src ->
                src
                |> Option.map (List.skip move.Count))
            |> Map.change move.Destination (fun dst ->
                dst
                |> Option.map (fun ds ->
                    (source |> List.take move.Count) @ ds))

        moves
        |> Seq.fold folder state
        |> collectTopCrates

let readInput =
    System.IO.File.ReadAllLines
    >> Array.toList
    >> parseInput [] []

let state, (ParsedMoves moves) = readInput "input.txt"

let part1Result = Mutable.part1 state moves
printfn $"Top crates crane 9000 {part1Result}"

let part2Result = Mutable.part2 state moves
printfn $"Top crates crane 9001 {part2Result}"

let immutable1 = Immutable.part1 state moves
printfn $"Top crates crane 9000 {immutable1}"

let immutable2 = Immutable.part2 state moves
printfn $"Top crates crane 9001 test {immutable2}"