module Day04

let parse: string seq -> Set<int * int> = toGrid2d >> Seq.filter (snd >> (=) '@') >> Seq.map fst >> Set.ofSeq

let neighbors rolls = around >> Seq.filter (flip Set.contains rolls) >> Seq.length

let accessible rolls = Set.filter (neighbors rolls >> (>) 4) rolls

let part1 = accessible >> Set.count

let part2 =
    let step r =
        match accessible r with
        | acc when Set.isEmpty acc -> None
        | acc -> Some (Set.count acc, Set.difference r acc)
    Seq.unfold step >> Seq.sum

let Solve: string seq -> _ = parse >> bothTP part1 part2
