module Day09

let parse = Seq.map (allInt >> Seq.Tuple) >> List.ofSeq

let area ((x1, y1), (x2, y2)) = int64 (abs (x2 - x1) + 1) * int64 (abs (y2 - y1) + 1)

let minMax a b = min a b, max a b

let crossesAxis (minA, maxA) (minB, maxB) ((a, b1), (_, b2)) =
    a > minA && a < maxA && min b1 b2 < maxB && max b1 b2 > minB

let edgeCrossesRect rangeX rangeY ((x1, y1), (x2, y2) as e) =
    crossesAxis rangeX rangeY e || crossesAxis rangeY rangeX ((y1, x1), (y2, x2))

let rectValid edges ((x1, y1), (x2, y2)) =
    edges |> Array.forall (edgeCrossesRect (minMax x1 x2) (minMax y1 y2) >> not)

let part1 = combinations >> List.map area >> List.max

let part2 reds =
    let edges = List.pairwise reds @ [List.last reds, List.head reds] |> Array.ofList
    combinations reds |> Array.ofList |> Array.Parallel.choose (fun r -> if rectValid edges r then Some (area r) else None) |> Array.max

let Solve: string seq -> _ = parse >> bothTP part1 part2
