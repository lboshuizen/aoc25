open System.IO

let readInput (d: int) =
    let p = Path.Combine(__SOURCE_DIRECTORY__, "..", "inputs", $"day{d}.txt")
    File.ReadLines(p) |> List.ofSeq

let (p1, t1), (p2, t2) = Day08.Solve (readInput 8)
printfn "part1: %A (%dms)\npart2: %A (%dms)" p1 t1 p2 t2
