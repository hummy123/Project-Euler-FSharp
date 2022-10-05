let rec fibonacci a b evenTotal =
    let higher = if a > b then a else b
    let lower = if higher = a then b else a
    let sum = higher + lower

    match sum with
    | sum when sum > 4000_000 -> evenTotal
    | sum when (sum % 2 = 0) -> fibonacci sum higher (evenTotal + sum)
    | sum -> fibonacci sum higher evenTotal

printfn "%i" (fibonacci 1 2 2)
