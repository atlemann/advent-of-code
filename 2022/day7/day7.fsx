open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let cd = Regex("\$ cd ([\/a-z]+)")
let cdUp = Regex("\$ cd ..")
let ls = Regex("\$ ls")
let dir = Regex("dir ([a-z]+)")
let file = Regex("([0-9]+)\s([a-z]+.?[a-z]*)")

let (|CD|_|) input =
    let m = cd.Match input
    if m.Success then
        Some m.Groups[1].Value
    else
        None

let (|CDUP|_|) input =
    let m = cdUp.Match input
    if m.Success then
        Some ()
    else
        None

let (|LS|_|) input =
    let m = ls.Match input
    if m.Success then
        Some ()
    else
        None

let (|DirListing|_|) input =
    let m = dir.Match input
    if m.Success then
        Some m.Groups[1].Value
    else
        None

let (|FileListing|_|) input =
    let m = file.Match input
    if m.Success then
        Some (m.Groups[1].Value |> int, m.Groups[2].Value)
    else
        None


// bug: There's a bug here somewhere, but I don't have time to figure out where it is, so will do mutable version instead
module Immutable =
    type Node =
        | Directory of Directory
        | File of File

    and Directory =
        { Parent: string
          Name: string
          Children: Node list }
        static member create(parent, name, children) =
            { Parent = parent
              Name = name
              Children = children }
        member this.Size() =
            this.Children
            |> Seq.sumBy (fun n ->
                match n with
                | File f -> f.Size
                | Directory d -> d.Size())

    and File =
        { Name: string
          Size: int }
        static member create(name, size) =
            { Name = name
              Size = size }

    type MetaData =
        | DirectoryMeta of Parent: string * Name: string * Level: int
        | FileMeta of Parent: string * Name: string * Size: int * Level: int

    let rec createMeta metaData dirStack input =
        let level = dirStack |> List.length
        match input with
        | [] ->
            metaData
        | h :: tail ->
            match h with
            | CD name ->
                createMeta metaData (name :: dirStack) tail

            | CDUP ->
                createMeta metaData (List.tail dirStack) tail

            | LS ->
                createMeta metaData dirStack tail

            | DirListing name ->
                let dirMeta = MetaData.DirectoryMeta (dirStack |> List.head, name, level)
                createMeta (dirMeta :: metaData) dirStack tail

            | FileListing (size, name) ->
                let fileMeta = MetaData.FileMeta (dirStack |> List.head, name, size, level)
                createMeta (fileMeta :: metaData) dirStack tail

            | x ->
                failwith $"Unknown input '{x}'"

    let buildTree metaData =
        metaData
        |> List.groupBy (fun m ->
            match m with
            | FileMeta (_, _, _, l) -> l
            | DirectoryMeta (_, _, l) -> l)
        |> List.sortByDescending fst
        |> List.fold (fun (acc: Directory list, currentFiles) (_, ns) ->
            let dirs =
                ns
                |> List.choose (fun n ->
                    match n with
                    | FileMeta _ -> None
                    | DirectoryMeta (parent, name, _) ->
                        let files =
                            currentFiles
                            |> Map.tryFind name
                            |> Option.map (List.map File)
                            |> Option.defaultValue []

                        let dirs =
                            acc
                            |> List.filter (fun d -> d.Parent = name)
                            |> List.map Directory

                        Directory.create (parent, name, (files @ dirs))
                        |> Some)

            let files =
                ns
                |> List.choose (fun n ->
                    match n with
                    | FileMeta (p, n, s, _) -> Some <| (p, File.create (n, s))
                    | _ -> None)
                |> List.groupBy fst
                |> List.map (fun (parent, xs) ->
                    (parent, xs |> List.map snd))
                |> Map.ofList

            (dirs @ acc, files))
            ([], Map.empty)
        |> fst

    let run input =
        let meta =
            input
            |> System.IO.File.ReadAllLines
            |> Array.toList
            |> createMeta [ DirectoryMeta("", "/", 0) ] []

        //printfn $"{meta}"

        let directories = buildTree meta

        // printfn $"{directories}"

        let rec getFileSum node =
            match node with
            | Node.File file ->
                (file.Name, file.Size)

            | Node.Directory dir ->
                let sum =
                    dir.Children
                    |> Seq.map getFileSum
                    |> Seq.sumBy snd

                (dir.Name, sum)

        let dirSizes =
            directories
            |> List.map (fun d -> (d.Name, d.Size()))

        dirSizes
        |> List.iter (printfn "%A")

        printfn "-------------------"

        let sizeLessThan100K =
            dirSizes
            |> List.filter (fun (_, size) -> size <= 100_000)

        sizeLessThan100K
        |> List.iter (printfn "%A")

        printfn "-------------------"

        printfn "Less than 100K sum: %A" (sizeLessThan100K |> List.sumBy snd)

module Mutable =
    type File =
        { Name: string
          Size: int }
        static member create(name, size) =
            { Name = name
              Size = size }

    type Directory(name) =
        let subDirs = Dictionary<string, Directory>()
        let files = ResizeArray<File>()
    with
        static member create(name) = Directory(name)
        member this.Name = name
        member this.addSubDir (dir: Directory) =
            subDirs.Add (dir.Name, dir)
        member this.getSubDir name =
            subDirs[name]
        member this.getAllSubDirs() =
            subDirs |> Seq.map (fun kvp -> kvp.Value)
        member this.addFile (file: File) =
            files.Add file
        member this.Size() =
            let fileSize = files |> Seq.sumBy (fun f -> f.Size)
            let dirSize = subDirs |> Seq.sumBy (fun f -> f.Value.Size())
            fileSize + dirSize

    let createDirectories input =
        let rootDir = Directory.create "/"
        let startDir = Directory.create ""
        startDir.addSubDir(rootDir)

        let dirStack = Stack<Directory>()
        dirStack.Push startDir

        for entry in input |> File.ReadAllLines do
            match entry with
            | CD name ->
                let currentDir = dirStack.Peek()
                dirStack.Push (currentDir.getSubDir(name))

            | CDUP ->
                dirStack.Pop() |> ignore

            | LS ->
                ()

            | DirListing name ->
                let newDir = Directory.create name
                let parentDir = dirStack.Peek()
                parentDir.addSubDir(newDir)

            | FileListing (size, name) ->
                let newFile = File.create (name, size)
                let parentDir = dirStack.Peek()
                parentDir.addFile(newFile)

            | x ->
                failwith $"Unknown input '{x}'"

        rootDir

    let rec getAllDirectories (dir: Directory) =
        [
            dir

            for d in dir.getAllSubDirs() do
                yield! getAllDirectories d
        ]

    let runPart1 input =
        let rootDir =
            input
            |> createDirectories

        let lessThan100K =
            rootDir
            |> getAllDirectories
            |> List.choose (fun d ->
                let size = d.Size()
                if size <= 100_000 then
                    Some (d.Name, size)
                else
                    None)

        // lessThan100K
        // |> List.iter (printfn "%A")

        lessThan100K
        |> Seq.sumBy snd
        |> printfn "Less than 100K sum: %d"

    let runPart2 input =
        let rootDir =
            input
            |> createDirectories

        let allDirs = getAllDirectories rootDir

        let totalDiskSpace = 70_000_000
        let requiredDiskSpace = 30_000_000

        let leftOver = totalDiskSpace - rootDir.Size()

        let candidates =
            allDirs
            |> List.choose (fun dir ->
                let size = dir.Size()
                if leftOver + size >= requiredDiskSpace then
                    Some (dir.Name, size)
                else
                    None)

        let toDelete =
            candidates
            |> List.minBy snd

        // printfn "Candidates:"
        // candidates
        // |> List.iter (printfn "%A")

        // printfn $"Used space: {rootDir.Size()}"
        // printfn $"Left over: {leftOver}"
        printfn $"To delete: {toDelete}"

//Immutable.run "input.txt"

Mutable.runPart1 "input.txt"
Mutable.runPart2 "input.txt"