module Day06
open System.Text.RegularExpressions

let digits n = string n |> Seq.map a2i |> List.ofSeq
let fromDigits = List.fold (fun acc d -> acc * 10L + int64 d) 0L
let tokenize s = Regex.Matches(s, @"\S+") |> Seq.map (fun m -> m.Value, m.Index) |> Array.ofSeq

let parse (input: string seq) =
    let rows = input |> Seq.map tokenize |> Array.ofSeq
    let ops, nums = Array.last rows, rows[..rows.Length-2]
    ops |> Array.mapi (fun i (v, p) ->
        let vals, aligned = nums |> Array.foldBack (fun r (vs, ok) -> int64 (fst r.[i]) :: vs, ok && snd r.[i] = p) <| ([], true)
        (if v[0] = '*' then (*) else (+)), vals, aligned) |> List.ofArray

let decompose (op, nums: int64 list, leftAlign) =
    let maxLen = nums |> List.max |> float |> log10 |> int |> (+) 1
    let pad ds = List.replicate (maxLen - List.length ds) -1 |> fun p -> if leftAlign then ds @ p else p @ ds
    nums |> List.map (digits >> pad) |> List.transpose |> List.map (List.filter ((<>) -1) >> fromDigits)
    |> List.reduce op

let part1 = parse >> List.sumBy (fun (op, nums, _) -> List.reduce op nums)
let part2 = parse >> List.sumBy decompose

let Solve: string seq -> _ = both part1 part2 >> map2 (timed id, timed id)
