module Day07

let parse: string seq -> _ = Seq.toArray

let simulate (grid: string array) =
    
    let addTo col n m = Map.change col (fun v -> Some (defaultArg v 0L + n)) m

    let step row (b, s) col n =
        match grid.[row].[col] with
        | '^' -> b |> addTo (col-1) n |> addTo (col+1) n, s+1L
        | _ -> addTo col n b, s

    let processRow (beams, splits) row =
        let beams', splits' = beams |> Map.fold (step row) (Map.empty, 0L)
        beams', splits + splits'

    let startCol = grid.[0].IndexOf('S')
    let beams, splits = List.fold processRow (Map.ofList [startCol, 1L], 0L) [1..grid.Length-1]
    splits, beams |> Map.values |> Seq.sum

let part1 = simulate >> fst
let part2 = simulate >> snd

let Solve: string seq -> _ = parse >> bothTP part1 part2
