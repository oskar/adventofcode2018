// Learn more about F# at http://fsharp.org

open System
open System.IO

let convert (str:string) = Convert.ToInt32 str
let parse lines = Array.map convert lines
let sum (frequencies:int[]) = Array.sum frequencies
let read path = File.ReadAllLines path

[<EntryPoint>]
let main argv =
    read "input.txt" |> parse |> sum |> Console.WriteLine
    0
