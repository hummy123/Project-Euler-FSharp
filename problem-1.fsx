let SumOfMultiples firstMult secMult multiplesBelow =
    let rec loop counter accumulator =
        match counter with
        | 0 -> accumulator
        | c when (c % firstMult = 0) -> loop (counter - 1) (accumulator + c)
        | c when (c % secMult = 0) -> loop (counter - 1) (accumulator + c)
        | _ -> loop (counter - 1) accumulator

    loop (multiplesBelow - 1) 0

printfn "%A" (SumOfMultiples 3 5 1000)
