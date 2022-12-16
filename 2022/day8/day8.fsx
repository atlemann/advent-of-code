open System.IO

let data =
    "input.txt"
    |> File.ReadAllLines
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (string >> int))

let matrix = Array2D.init data.Length data[0].Length (fun i j -> data[i][j])
let nRows = matrix |> Array2D.length1
let nCols = matrix |> Array2D.length2

let getDirectionArrays i j =
    [|
        matrix[i, .. j - 1] |> Array.rev // Left
        matrix[i, j + 1 ..]              // Right
        matrix[.. i - 1, j] |> Array.rev // Up
        matrix[i + 1 .., j]              // Down
    |]

// Part 1
let part1 =
    [|
        for i in 0 .. nRows - 1 do
            for j in 0 .. nCols - 1 do
                let directionArrays = getDirectionArrays i j
                let current = matrix[i, j]

                let isVisible =
                    directionArrays
                    |> Array.exists (fun xs ->
                        xs |> Array.isEmpty ||
                        xs |> Array.forall (fun x -> x < current))

                if isVisible then
                    1
                else
                    0
    |]

part1
|> Array.sum
|> printfn "Part 1: %d"

// Part 2
let part2 =
    [|
        for i in 0 .. nRows - 1 do
            for j in 0 .. nCols - 1 do
                let directionArrays = getDirectionArrays i j
                let current = matrix[i, j]

                directionArrays
                |> Array.map (fun xs ->
                    if Array.isEmpty xs then
                        1
                    else
                        xs
                        |> Array.indexed
                        |> Array.tryPick (fun (i, x) ->
                            if x >= current then
                                Some (i + 1)
                            else
                                None)
                        |> Option.defaultValue xs.Length)
                |> Array.reduce (*)
    |]

part2
|> Array.max
|> printfn "Part 2: %d"