/// Day 11: Reactor - DAG path counting with memoization
module Day11

open System.Collections.Generic

let parse: string seq -> Map<string, string list> =
    Seq.map (fun l -> let p = l.Split ": " in p.[0], p.[1].Split ' ' |> Array.toList) >> Map.ofSeq

let part1 (graph: Map<string, string list>) =
    let cache = Dictionary<string, int64>()
    let rec count node =
        match node, cache.TryGetValue node with
        | "out", _ -> 1L
        | _, (true, v) -> v
        | _ -> let v = graph.[node] |> List.sumBy count
               cache.[node] <- v; v
    count "you"

let part2 (graph: Map<string, string list>) =
    let cache = Dictionary<string * bool * bool, int64>()
    let rec count node seenDac seenFft =
        let seenDac, seenFft = seenDac || node = "dac", seenFft || node = "fft"
        match node, cache.TryGetValue ((node, seenDac, seenFft)) with
        | "out", _ -> if seenDac && seenFft then 1L else 0L
        | _, (true, v) -> v
        | _ -> let v = graph.[node] |> List.sumBy (fun c -> count c seenDac seenFft)
               cache.[(node, seenDac, seenFft)] <- v; v
    count "svr" false false

let Solve: string seq -> _ = parse >> bothTP part1 part2
