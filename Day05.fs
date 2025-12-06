module Day05

let parse = splitOnEmpty >> Seq.Tuple >> map2 (List.map (splitOn '-' >> Array.map int64 >> pair), List.map int64)

let merge ranges =
    let fold acc (lo, hi) =
        match acc with
        | (alo, ahi) :: rest when lo <= ahi + 1L -> (alo, max ahi hi) :: rest
        | _ -> (lo, hi) :: acc
    ([], ranges |> List.sortBy fst) ||> List.fold fold

let part1 (ranges, ids) =
    let inRange id (lo, hi) = lo <= id && id <= hi
    ids |> List.filter (List.exists << inRange >> (|>) ranges) |> List.length

let part2 (ranges, _) = 
    let rangeLen (lo, hi) = hi - lo + 1L
    merge ranges |> List.sumBy rangeLen

let Solve: string seq -> _ = parse >> bothTP part1 part2
