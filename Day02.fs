module Day02

let parse: string seq -> _ list =
    Seq.head
    >> fun s ->
        s.Split ','
        |> Seq.map (fun r -> let p = r.Split '-' in int64 p[0], int64 p[1])
        |> Seq.toList

let pow10 = pown 10L
let digitLen n = int (floor (log10 (float n))) + 1

let repeatedInRange t (lo, hi) =
    let multiplier l = [0..t-1] |> Seq.sumBy (fun i -> pow10 (i*l))
    let candidates l =  seq { pow10 (l-1) .. pow10 l - 1L } |> Seq.map ((*) (multiplier l))

    [for l in digitLen lo .. digitLen hi do if l%t = 0 then l]
    |> Seq.collect (fun l -> candidates (l/t) |> Seq.filter (fun d -> d >= lo && d <= hi))

let allRepeated (lo, hi) = [2..digitLen hi] |> Seq.collect (flip repeatedInRange (lo, hi)) |> Seq.distinct

let part1 = Seq.collect (repeatedInRange 2) >> Seq.sum
let part2 = Seq.collect allRepeated >> Seq.sum

let Solve: string seq -> _ = parse >> bothTP part1 part2
