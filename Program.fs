open System.IO

let readInput (d: int) =
    let p = Path.Combine(__SOURCE_DIRECTORY__, "..", "inputs", $"day{d}.txt")
    File.ReadLines(p) |> List.ofSeq

let (p1, t1), (p2, t2) = Day06.Solve (readInput 6)
printfn "part1: %A (%dms)\npart2: %A (%dms)" p1 t1 p2 t2
