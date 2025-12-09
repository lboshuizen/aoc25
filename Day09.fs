module Day09

let parse = Seq.map (allInt >> Seq.Tuple) >> List.ofSeq

let area ((x1, y1), (x2, y2)) = int64 (abs (x2 - x1) + 1) * int64 (abs (y2 - y1) + 1)

let rangeOverlaps a1 a2 b1 b2 = min a1 a2 < max b1 b2 && max a1 a2 > min b1 b2

let crossesAxis minA maxA minB maxB ((a, b1), (_, b2)) = a > minA && a < maxA && rangeOverlaps b1 b2 minB maxB

let edgeCrossesRect minX maxX minY maxY edge =
    crossesAxis minX maxX minY maxY edge || crossesAxis minY maxY minX maxX (biMap swap edge)

let rectValid edges ((x1, y1), (x2, y2)) =
    let minX, maxX, minY, maxY = min x1 x2, max x1 x2, min y1 y2, max y1 y2
    edges |> Array.forall (edgeCrossesRect minX maxX minY maxY >> not)

let part1 = combinations >> List.map area >> List.max

let part2 reds =
    let edges = List.pairwise reds @ [List.last reds, List.head reds] |> Array.ofList
    combinations reds |> Array.ofList |> Array.Parallel.filter (rectValid edges) |> Array.Parallel.map area |> Array.max

let Solve: string seq -> _ = parse >> bothTP part1 part2
