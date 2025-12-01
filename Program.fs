open System.Diagnostics
open System.IO

let readInput (d: int) =
    let p = Path.Combine(__SOURCE_DIRECTORY__, "..", "inputs", $"day{d}.txt")
    File.ReadLines(p) |> List.ofSeq

let sw = Stopwatch.StartNew()
let result = Day01.Solve (readInput 1)
printfn "time: %dms\nresult:\n %A" sw.ElapsedMilliseconds result
