module [intToBytes]

intToBytes : Int * -> List U8
intToBytes = \int ->
    x = Num.rem int 256 |> Num.toU8
    rest = Num.divTrunc int 256

    if Num.isZero rest then
        [x]
    else
        List.concat (intToBytes rest) [x]
