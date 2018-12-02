open System
open System.IO

// Helpers

let parse lines = Array.map Int32.Parse lines

let read path = File.ReadAllLines path

// Take one

let resultingFrequency changes = Array.sum changes

let findFirstDuplicateFrequency changes =
    let rec findFirstDupFreqHelper (knownFrequencies, currentFrequency)
            (changes, atChange) =
        if Set.contains currentFrequency knownFrequencies then currentFrequency
        else
            let knownFrequencies = Set.add currentFrequency knownFrequencies
            let currentFrequency = currentFrequency + Array.get changes atChange

            findFirstDupFreqHelper (knownFrequencies, currentFrequency)
                (changes, (atChange + 1) % Array.length changes)

    findFirstDupFreqHelper (Set.empty, 0) (changes, 0)

// Take two

let repeat items =
    seq {
        while true do
            yield! items
    }

let frequencies changes = Seq.scan (+) 0 changes

let firstDuplicate sequence =
    let rec firstDuplicateHelper uniqueItems currentSeq =
        if Seq.isEmpty currentSeq then None
        else
            let item = Seq.head currentSeq
            if Set.contains item uniqueItems then Some(item)
            else
                let nextPrevious = Set.add item uniqueItems
                firstDuplicateHelper nextPrevious (Seq.tail currentSeq)

    firstDuplicateHelper Set.empty sequence

[<EntryPoint>]
let main _ =
    let changes = read "input.txt" |> parse

    printfn "⭐ Problem 1 (take one): %d" (changes |> resultingFrequency)
    printfn "⭐ Problem 2 (take one): %d" (findFirstDuplicateFrequency changes)

    printfn "⭐ Problem 1 (take two): %d" (changes |> frequencies |> Seq.last)
    printfn "⭐ Problem 2 (take two): TBA"
    //     (changes
    //      |> repeat
    //      |> frequencies
    //      |> firstDuplicate
    //      |> Option.defaultValue -1337)
    0
