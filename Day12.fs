module Day12

let parseShape lines = [ for r, line in List.indexed lines do for c, ch in Seq.indexed line do if ch = '#' then r, c ]

let orientations shape =
    let rot, flip = List.map (fun (r, c) -> c, -r), List.map (fun (r, c) -> -r, c)
    let normalize s =
        let rs, cs = List.unzip s in let mr, mc = List.min rs, List.min cs
        s |> List.map (fun (r, c) -> r - mr, c - mc) |> List.sort
    [id; rot; rot >> rot; rot >> rot >> rot] |> List.collect (fun r -> [r; flip >> r])
    |> List.map (fun t -> normalize (t shape) |> Array.ofList) |> List.distinct |> Array.ofList

let parseRegion line = let n = allInt line |> Array.ofSeq in n.[1], n.[0], n.[2..]

let parse input =
    let secs = input |> splitOnEmpty
    let isShape (sec: string list) = not (sec.Head.Contains 'x')
    let shapes, regions = secs |> List.partition isShape
    let orients = shapes |> List.map (List.tail >> parseShape) |> Array.ofList |> Array.map orientations
    orients, regions |> List.concat |> List.filter ((<>) "") |> List.map parseRegion |> Array.ofList

let canFit h w (allOrients: (int*int)[] array array) quantities =
    let pad, stride = 3, w + 6
    let isPadding i = let r, c = i / stride, i % stride in r < pad || r >= h + pad || c < pad || c >= w + pad
    let grid = Array.init ((h + 6) * stride) isPadding
    let offsets1D = [| for o in allOrients -> [| for c in o -> [| for dr, dc in c -> dr * stride + dc |] |] |]
    let shapeSizes = allOrients |> Array.map (fun o -> o.[0].Length)
    let toPlace = [| for i, qty in Array.indexed quantities do for _ in 1..qty -> i |]

    match (quantities, shapeSizes) ||> Array.map2 (*) |> Array.sum <= h * w with
    | false -> false
    | true ->
        let positions = [| for r in 0..h-1 do for c in 0..w-1 -> (r + pad) * stride + c + pad |]
        let place offsets baseIdx v = for off in offsets do grid.[baseIdx + off] <- v
        let canPlace offsets baseIdx = offsets |> Array.forall (fun off -> not grid.[baseIdx + off])
        let tryPlace offsets baseIdx solve =
            canPlace offsets baseIdx && (place offsets baseIdx true; solve () || (place offsets baseIdx false; false))

        let rec solve = function
            | idx when idx = toPlace.Length -> true
            | idx -> offsets1D.[toPlace.[idx]] |> Array.exists (fun offsets ->
                    positions |> Array.exists (fun baseIdx -> tryPlace offsets baseIdx (fun () -> solve (idx + 1))))
        solve 0

let part1 (orients, regions) = regions |> Array.sumBy (fun (h, w, q) -> if canFit h w orients q then 1 else 0)
let part2 _ = Some 0

let Solve: string seq -> (int * int64) * (int option * int64) = parse >> both (timed part1) (timed part2)
