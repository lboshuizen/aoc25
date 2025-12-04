module Day03

let parse: string seq -> int array list = Seq.map (Seq.map a2i >> Array.ofSeq) >> List.ofSeq

let pick (bn: int array) r (pos, acc) =
    let i = seq {pos .. bn.Length - r-1} |> Seq.maxBy (fun i -> bn[i])
    i + 1, acc * 10L + int64 bn[i]

let maxJoltageN n bn = Seq.foldBack (pick bn) (Seq.init n id) (0, 0L) |> snd

let part1 = List.sumBy (maxJoltageN 2)
let part2 = List.sumBy (maxJoltageN 12)

let Solve: string seq -> _ = parse >> bothTP part1 part2
