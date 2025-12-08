module Day08

let parse: string seq -> _ =
    let distSq ((_,(x1,y1,z1)), (_,(x2,y2,z2))) =
        let dx, dy, dz = x2-x1, y2-y1, z2-z1
        dx*dx + dy*dy + dz*dz
    Seq.map (allInt64 >> Seq.Tuple3)
    >> List.ofSeq >> fun pts -> List.length pts, pts |> List.indexed |> combinations |> List.sortBy distSq

let mkUnionFind n =
    let parent = Array.init n id
    let find x = iterate (Array.get parent) x |> Seq.find (fun i -> parent[i] = i)
    let union i j =
        let pi, pj = find i, find j
        if pi = pj then false else parent[pi] <- pj; true
    find, union

let part1 (n, pairs) =
    let find, union = mkUnionFind n
    for (i,_),(j,_) in List.take 1000 pairs do union i j |> ignore
    [| 0 .. n-1 |] |> Array.countBy find |> Array.map snd
    |> Array.sortDescending |> Array.take 3 |> Array.reduce (*) |> int64

let part2 (n, pairs) =
    let _, union = mkUnionFind n
    let folder last ((i,p1),(j,p2)) = if union i j then Some (p1,p2) else last
    let (x1,_,_), (x2,_,_) = pairs |> List.fold folder None |> Option.get
    x1 * x2

let Solve: string seq -> _ = parse >> bothTP part1 part2
