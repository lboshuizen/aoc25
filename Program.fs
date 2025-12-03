open System.IO

let readInput (d: int) =
    let p = Path.Combine(__SOURCE_DIRECTORY__, "..", "inputs", $"day{d}.txt")
    File.ReadLines(p) |> List.ofSeq

let (p1, t1), (p2, t2) = Day02.Solve (readInput 2)
printfn "part1: %A (%dms)\npart2: %A (%dms)" p1 t1 p2 t2
