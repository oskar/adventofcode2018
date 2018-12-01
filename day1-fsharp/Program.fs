open System
open System.IO

let convert (str:string) = Convert.ToInt32 str
let parse lines = Array.map convert lines
let sum (changes:int[]) = Array.sum changes
let read path = File.ReadAllLines path
let repeat items = seq { while true do yield! items }
let frequencies changes = Seq.scan (+) 0 changes

let firstDuplicate sequence =
  let rec firstDuplicateHelper uniqueItems currentSeq =
    if Seq.isEmpty currentSeq then
      None
    else
      let item = Seq.head currentSeq
      if Set.contains item uniqueItems then
        Some(item)
      else
        let nextPrevious = Set.add item uniqueItems
        firstDuplicateHelper nextPrevious (Seq.tail currentSeq)
  firstDuplicateHelper Set.empty sequence

let rec findDuplicate changes previous currentFrequency changeIndex =
  if Set.contains currentFrequency previous then
    currentFrequency
  else
    let nextPrevious = Set.add currentFrequency previous
    let nextFrequency = currentFrequency + Array.get changes changeIndex
    let nextIndex = (changeIndex + 1) % (Array.length changes)
    findDuplicate changes nextPrevious nextFrequency nextIndex

[<EntryPoint>]
let main argv =
    let changes = read "input.txt" |> parse
    printfn "Problem 1: %d" (changes |> sum)
    printfn "Problem 2a: %d" (findDuplicate changes Set.empty 0 0)
    printfn "Problem 2b: %d" ((firstDuplicate (frequencies (repeat changes))) |> (Option.defaultValue -666))
    0
