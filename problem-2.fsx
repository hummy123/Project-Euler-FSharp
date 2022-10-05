let rec fibonacci a b evenNums =
    let higher = if a > b then a else b
    let lower = if higher = a then b else a
    let sum = higher + lower

    match sum with
    | sum when sum > 4000_000 -> evenNums
    | sum when (sum % 2 = 0) -> fibonacci sum higher (evenNums @ [ sum ])
    | sum -> fibonacci sum higher evenNums

printfn "%i" (fibonacci 1 2 [ 1; 2 ] |> List.sum)
