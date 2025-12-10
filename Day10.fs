module Day10

open System.Text.RegularExpressions

let [<Literal>] Eps = 1e-9

let parse (lines: string seq) = lines |> Seq.map (fun line ->
    Regex.Match(line, @"\[([.#]+)\]").Groups.[1].Value |> Seq.map (function '#' -> 1 | _ -> 0) |> Array.ofSeq,
    Regex.Matches(line, @"\(([0-9,]+)\)") |> Seq.map (fun m -> m.Groups.[1].Value.Split(',') |> Array.map int) |> Array.ofSeq,
    Regex.Match(line, @"\{([0-9,]+)\}").Groups.[1].Value.Split(',') |> Array.map int) |> Array.ofSeq

let matrix n m (buttons: int[][]) zero one target =
    Array.init n (fun i -> Array.init (m+1) (fun j -> if j < m then (if Array.contains i buttons.[j] then one else zero) else target i))

let swap r1 r2 mat = if r1 = r2 then mat else mat |> Array.mapi (fun i r -> if i = r1 then mat.[r2] elif i = r2 then mat.[r1] else r)

let freeVars m pivots = let ps = Set.ofList pivots in [0..m-1] |> List.filter (ps.Contains >> not)

let gaussGF2 n m (mat: int[][]) =
    let elim p c (m: int[][]) = m |> Array.mapi (fun r row -> if r <> p && row.[c] = 1 then Array.map2 (^^^) row m.[p] else row)
    let step (m: int[][], piv, p) c =
        match [p..n-1] |> List.tryFind (fun r -> m.[r].[c] = 1) with
        | None -> (m, piv, p) | Some i -> (m |> swap p i |> elim p c, c :: piv, p + 1)
    let mat', piv, p = ((mat, [], 0), [0..m-1]) ||> List.fold step in (mat', List.rev piv, p)

let solve (target: int[]) (buttons: int[][]) =
    let n, m = target.Length, buttons.Length
    let mat, pivots, pRow = matrix n m buttons 0 1 (Array.get target) |> gaussGF2 n m
    if [pRow..n-1] |> List.exists (fun r -> mat.[r].[m] = 1) then None else
    let free = freeVars m pivots |> Array.ofList
    let pMap = pivots |> List.mapi (fun i c -> c, i) |> Map.ofList
    let back fm = Array.init m (fun j -> fm |> Map.tryFind j |> Option.defaultWith (fun () ->
        free |> Array.fold (fun s fv -> s ^^^ (mat.[pMap.[j]].[fv] * fm.[fv])) mat.[pMap.[j]].[m]))
    [0..(1 <<< free.Length)-1] |> List.map (fun mask ->
        free |> Array.mapi (fun i fv -> fv, (mask >>> i) &&& 1) |> Map.ofArray |> back |> Array.sum) |> List.min |> Some

let part1 = Array.sumBy (fun (t, b, _) -> solve t b |> Option.defaultValue 0)

let gaussReal n m (mat: float[][]) =
    let elim p c (m: float[][]) = m |> Array.mapi (fun r row -> if r <> p && abs row.[c] > Eps then Array.map2 (fun x y -> y - row.[c] * x) m.[p] row else row)
    let step (m: float[][], piv, p) c =
        if p >= n then (m, piv, p) else
        let best = [p..n-1] |> List.maxBy (fun r -> abs m.[r].[c])
        if abs m.[best].[c] <= Eps then (m, piv, p) else
        let m' = m |> swap p best
        let m'' = m' |> Array.mapi (fun r row -> if r = p then Array.map (fun x -> x / m'.[p].[c]) row else row)
        (elim p c m'', c :: piv, p + 1)
    let m, piv, _ = ((mat, [], 0), [0..m-1]) ||> List.fold step in (m, List.rev piv)

let solveJoltage (joltage: int[]) (buttons: int[][]) =
    let n, m = joltage.Length, buttons.Length
    let mat, pivots = matrix n m buttons 0.0 1.0 (Array.get joltage >> float) |> gaussReal n m
    let free = freeVars m pivots |> Array.ofList
    let nFree, pMap = free.Length, pivots |> List.mapi (fun i c -> c, i) |> Map.ofList
    let back fm = Array.init m (fun j -> fm |> Map.tryFind j |> Option.defaultWith (fun () ->
        free |> Array.fold (fun s fv -> s - mat.[pMap.[j]].[fv] * fm.[fv]) mat.[pMap.[j]].[m]))
    let valid = Array.forall (fun x -> x >= -Eps && abs (x - round x) < Eps)
    let total = Array.sumBy (round >> int)
    match nFree with
    | 0 -> let sol = back Map.empty in if valid sol then total sol else failwith "No solution"
    | _ ->
        let maxJ, sumJ = Array.max joltage, Array.sum joltage
        let coeff pi k = mat.[pi].[free.[k]]
        let feasible idx (v: int[]) = [0..pivots.Length-1] |> List.forall (fun pi ->
            let pv = [0..idx-1] |> List.fold (fun a k -> a - coeff pi k * float v.[k]) mat.[pi].[m]
            pv >= -Eps || [idx..nFree-1] |> List.exists (fun k -> coeff pi k < -Eps))
        let rec go best = function
            | [] -> best
            | (i, v, s) :: t when s >= best || s > sumJ || not (feasible i v) -> go best t
            | (i, v, s) :: t when i = nFree ->
                let fm = free |> Array.mapi (fun j fv -> fv, float v.[j]) |> Map.ofArray
                go (let sol = back fm in if valid sol then min best (total sol) else best) t
            | (i, v, s) :: t ->
                let ch = [min maxJ (sumJ-s) .. -1 .. 0] |> List.map (fun x -> i+1, Array.mapi (fun j y -> if j = i then x else y) v, s+x)
                go best (ch @ t)
        go System.Int32.MaxValue [(0, Array.zeroCreate nFree, 0)]

let part2 = Array.sumBy (fun (_, b, j) -> solveJoltage j b)

let Solve: string seq -> _ = parse >> bothTP part1 part2
