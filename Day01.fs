module Day01

let parse = Seq.map (fun (s: string) -> s[0], int s[1..])

let dial n (d, i) =
    let delta = if d = 'L' then -i else i
    ((n + delta) % 100 + 100) % 100

let countZeros n =
    let fd a = int (floor (float a / 100.0))
    function
    | 'L', i -> fd (n - 1) - fd (n - i - 1)
    | _, i -> (n + i) / 100 - n / 100

let part1 = Seq.scan dial 50 >> Seq.filter ((=) 0) >> Seq.length

let part2 =
    Seq.fold (fun (pos, cnt) mv -> dial pos mv, cnt + countZeros pos mv) (50, 0) >> snd

let Solve: string seq -> int * int = parse >> both part1 part2
