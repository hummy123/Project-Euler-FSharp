// initial brute force solution
let rec fibonacci a b evenTotal =
    let higher = if a > b then a else b
    let lower = if higher = a then b else a
    let sum = higher + lower

    match sum with
    | sum when sum > 4000_000 -> evenTotal
    | sum when (sum % 2 = 0) -> fibonacci sum higher (evenTotal + sum)
    | sum -> fibonacci sum higher evenTotal

printfn "%i" (fibonacci 1 2 2)

// optimised solution coming from following observation:
// 2 (even starting point)
// 8 = 2 * 4
// 34 = 8 * 4 + 2
// 144 = 34 * 4 + 8

let rec fib2 curEven evenTotal prevEven =
    let calc = curEven * 4 + prevEven

    match calc with
    | calc when calc > 4000_000 -> evenTotal
    | calc -> fib2 calc (evenTotal + calc) curEven

printfn "%i" (fib2 2 2 0)

// Notes on second solution:
// Hopefully this will produce same results as first solution towards infinity
// but with time complexity less than O(n) (likely O(log n)).
// I can confirm it gives the same result as the brute-force answer above
// given a limiting number of 4 million, but not above as I am not a mathematician.
// There is a solution with likely lower complexity than this (using phi)
// but I would like to stick with this one as I made it myself and also because computers
// cannot store with absolute accuracy numbers that repeat forever like phi.
