module [encodeBytes, encodeStr, decodeBytes, decodeStr]

# Base32 encoding and decoding.
#
# See <https://www.crockford.com/base32.html> for the full specification.
#
# When encoding, the input array is split into 5-byte chunks, which get encoded into 8 base32
# characters. The encoding is done by selecting 5 bits at a time across the 5 bytes, and using
# these 5 bits to select one of the 32 base32 characters.

# Encode a single 8-bit byte into a base32 character.
encodeU5 : U8 -> U8
encodeU5 = \char ->
    when char is
        0 -> '0'
        1 -> '1'
        2 -> '2'
        3 -> '3'
        4 -> '4'
        5 -> '5'
        6 -> '6'
        7 -> '7'
        8 -> '8'
        9 -> '9'
        10 -> 'A'
        11 -> 'B'
        12 -> 'C'
        13 -> 'D'
        14 -> 'E'
        15 -> 'F'
        16 -> 'G'
        17 -> 'H'
        18 -> 'J'
        19 -> 'K'
        20 -> 'M'
        21 -> 'N'
        22 -> 'P'
        23 -> 'Q'
        24 -> 'R'
        25 -> 'S'
        26 -> 'T'
        27 -> 'V'
        28 -> 'W'
        29 -> 'X'
        30 -> 'Y'
        31 -> 'Z'
        n -> crash "no such base32 char `$(Inspect.toStr n)`"

# Encode a "chunk" of 5 bytes into 8 base32 characters.
encodeChunk : List U8 -> List U8
encodeChunk = \chunk ->
    # 8-bit bytes:  AAAAA AAABB BBBBB BCCCC CCCCD DDDDD DDEEE EEEEE
    # 5-bit chunks: AAAAA BBBBB CCCCC DDDDD EEEEE FFFFF GGGGG HHHHH
    when chunk is
        # Go through the five 8bit bytes in the chunk, selecting 5 bits at a time across their
        # boundaries to form the 8 base32 numbers of the encoded chunk.
        [b1, b2, b3, b4, b5] ->
            c1 = Num.shiftRightZfBy b1 3 # first 5 bits of b1

            c2 = Num.bitwiseOr
                (Num.bitwiseAnd 0b00000111 b1 |> Num.shiftLeftBy 2) # last 3 bits of b1
                (Num.bitwiseAnd 0b11000000 b2 |> Num.shiftRightZfBy 6) # first 2 bits of b2

            c3 =
                Num.bitwiseAnd 0b00111110 b2 # middle 5 bits of b2
                |> Num.shiftRightZfBy 1

            c4 = Num.bitwiseOr
                (Num.bitwiseAnd 0b00000001 b2 |> Num.shiftLeftBy 4) # last bit of b2
                (Num.bitwiseAnd 0b11110000 b3 |> Num.shiftRightZfBy 4) # first 4 bits of b3

            c5 = Num.bitwiseOr
                (Num.bitwiseAnd 0b00001111 b3 |> Num.shiftLeftBy 1) # last 4 bits of b3
                (Num.bitwiseAnd 0b10000000 b4 |> Num.shiftRightZfBy 7) # first bit of b4

            c6 = Num.bitwiseAnd 0b01111100 b4 |> Num.shiftRightZfBy 2 # middle 5 bits of b4

            c7 = Num.bitwiseOr
                (Num.bitwiseAnd 0b00000011 b4 |> Num.shiftLeftBy 3) # last 2 bits of b4
                (Num.bitwiseAnd 0b11100000 b5 |> Num.shiftRightZfBy 5) # first 3 bits of b5

            c8 = Num.bitwiseAnd 0b00011111 b5 # last 5 bits of b5

            expect c1 < 32
            expect c2 < 32
            expect c3 < 32
            expect c4 < 32
            expect c5 < 32
            expect c6 < 32
            expect c7 < 32
            expect c8 < 32

            List.map [c1, c2, c3, c4, c5, c6, c7, c8] encodeU5

        [b1, b2, b3, b4] -> encodeChunk [b1, b2, b3, b4, 0] |> List.dropLast 1
        [b1, b2, b3] -> encodeChunk [b1, b2, b3, 0, 0] |> List.dropLast 3
        [b1, b2] -> encodeChunk [b1, b2, 0, 0, 0] |> List.dropLast 4
        [b1] -> encodeChunk [b1, 0, 0, 0, 0] |> List.dropLast 6
        [] -> []
        _ -> crash "invalid chunk: $(Inspect.toStr chunk)"

expect
    input = List.repeat 0 5
    expected = List.repeat '0' 8
    actual = encodeChunk input
    actual == expected

# Encode a list of bytes into a base32 encoded string.
encodeBytes : List U8 -> Str
encodeBytes = \bytes ->
    chunks = List.chunksOf bytes 5
    encodedBytes = List.joinMap chunks encodeChunk
    when Str.fromUtf8 encodedBytes is
        Ok encodedStr -> encodedStr
        Err _ -> crash "invalid utf8"

expect
    input = Str.toUtf8 "i am a red herring"
    expected = "D4G62V90C4G74SB441M6AWKJD5Q6E"
    actual = encodeBytes input
    expected == actual

# Encode an arbitrary string into a base32 encoded string.
encodeStr : Str -> Str
encodeStr = \str ->
    encodeBytes (Str.toUtf8 str)

expect encodeStr "" == ""
expect encodeStr "f" == "CR"
expect encodeStr "fo" == "CSQG"
expect encodeStr "foo" == "CSQPY"
expect encodeStr "foob" == "CSQPYRG"
expect encodeStr "fooba" == "CSQPYRK1"
expect encodeStr "foobar" == "CSQPYRK1E8"
expect encodeStr "i am a red herring" == "D4G62V90C4G74SB441M6AWKJD5Q6E"
expect encodeStr "where are the flounders?" == "EXM6AWK541GQ4S90EHM6A836DHQQAVK4CNS76FR"
expect encodeStr "i have found an octopus" == "D4G6GRBPCMG6CVVNDSJ20RBE41QP6X3FE1TQ6"
expect encodeStr "a flip flop" == "C4G6CV39E0G6CV3FE0"

# Decodes a base32 character into a 5-bit number stored in an 8-bit unsigned int.
decodeU5 : U8 -> Result U8 [InvalidBase32Char U8]
decodeU5 = \char ->
    when char is
        '0' -> Ok 0
        '1' | 'i' | 'I' | 'l' | 'L' -> Ok 1
        '2' -> Ok 2
        '3' -> Ok 3
        '4' -> Ok 4
        '5' -> Ok 5
        '6' -> Ok 6
        '7' -> Ok 7
        '8' -> Ok 8
        '9' -> Ok 9
        'A' | 'a' -> Ok 10
        'B' | 'b' -> Ok 11
        'C' | 'c' -> Ok 12
        'D' | 'd' -> Ok 13
        'E' | 'e' -> Ok 14
        'F' | 'f' -> Ok 15
        'G' | 'g' -> Ok 16
        'H' | 'h' -> Ok 17
        'J' | 'j' -> Ok 18
        'K' | 'k' -> Ok 19
        'M' | 'm' -> Ok 20
        'N' | 'n' -> Ok 21
        'P' | 'p' -> Ok 22
        'Q' | 'q' -> Ok 23
        'R' | 'r' -> Ok 24
        'S' | 's' -> Ok 25
        'T' | 't' -> Ok 26
        'V' | 'v' -> Ok 27
        'W' | 'w' -> Ok 28
        'X' | 'x' -> Ok 29
        'Y' | 'y' -> Ok 30
        'Z' | 'z' -> Ok 31
        _ -> Err (InvalidBase32Char char)

expect decodeU5 '@' == Err (InvalidBase32Char '@')

# Decode one to eight 5-bit base32 numbers into five 8-bit base255 numbers.
# The input will be padded with zeros if it is not a full 40-bit chunk.
#
# The eight bytes that get passed in only contain 5 bits of information each,
# and so look like this:
#
#   000AAAAA 000BBBBB 000CCCCC 000DDDDD 000EEEEE 000FFFFF 000GGGGG 000HHHHH
#
# We need to extract the 5 bits from each byte and combine them into 5 bytes
# that contain 8 bits of information each:
#
#  AAAAABBB BBCCCCCD DDDDEEEE EFFFFFGG GGGHHHHH
#
decodeChunk : List U8 -> List U8
decodeChunk = \chars ->
    when chars is
        # Full 40-bit chunk
        [a, b, c, d, e, f, g, h] ->
            # AAAAABBB
            b1 = Num.bitwiseOr
                (Num.shiftLeftBy a 3)
                (Num.shiftRightZfBy b 2)

            # BBCCCCCD
            b2 = Num.bitwiseOr
                (Num.bitwiseOr (Num.shiftLeftBy b 6) (Num.shiftLeftBy c 1))
                (Num.shiftRightZfBy d 4)

            # DDDDEEEE
            b3 = Num.bitwiseOr
                (Num.shiftLeftBy d 4)
                (Num.shiftRightZfBy e 1)

            # EFFFFFGG
            b4 = Num.bitwiseOr
                (Num.bitwiseOr (Num.shiftLeftBy e 7) (Num.shiftLeftBy f 2))
                (Num.shiftRightZfBy g 3)

            # GGGHHHHH
            b5 = Num.bitwiseOr
                (Num.shiftLeftBy g 5)
                h

            [b1, b2, b3, b4, b5]

        # Only 35 bits, pad with a zero
        [a, b, c, d, e, f, g] ->
            decodeChunk [a, b, c, d, e, f, g, 0]
            |> List.dropLast 1

        # Only 30 bits, pad with two zeros
        [a, b, c, d, e, f] ->
            decodeChunk [a, b, c, d, e, f, 0, 0]
            |> List.dropLast 2

        # Only 25 bits, pad with three zeros
        [a, b, c, d, e] ->
            decodeChunk [a, b, c, d, e, 0, 0, 0]
            |> List.dropLast 2

        # Only 20 bits, pad with four zeros
        [a, b, c, d] ->
            decodeChunk [a, b, c, d, 0, 0, 0, 0]
            |> List.dropLast 3

        # Only 15 bits, pad with five zeros
        [a, b, c] ->
            decodeChunk [a, b, c, 0, 0, 0, 0, 0]
            |> List.dropLast 4

        # Only 10 bits, pad with six zeros
        [a, b] ->
            decodeChunk [a, b, 0, 0, 0, 0, 0, 0]
            |> List.dropLast 4

        # Only 5 bits, pad with seven zeros
        [a] ->
            decodeChunk [a, 0, 0, 0, 0, 0, 0, 0]
            |> List.dropLast 4

        _ -> crash "invalid chunk: $(Inspect.toStr chars)"

# Decode a base32 encoded string into a list of bytes.
decodeBytes : Str -> Result (List U8) [InvalidBase32Char U8]
decodeBytes = \encodedStr ->
    Str.toUtf8 encodedStr
    |> List.mapTry decodeU5
    |> Result.map \decodedBytes ->
        decodedBytes
        |> List.chunksOf 8
        |> List.joinMap decodeChunk

expect
    input = "D4G62V90C4G74SB441M6AWKJD5Q6E"
    expected = Ok (Str.toUtf8 "i am a red herring")
    actual = decodeBytes input
    actual == expected

expect
    cases = [
        "dklfasljfidsa",
        "984whfosehfas",
        "9fudaf",
        "5",
        "093i4rt94jfaoidjfalksdjfdsalkf",
    ]

    List.all cases \input ->
        encoded = encodeBytes (Str.toUtf8 input)
        decoded = decodeBytes encoded
        decoded == Ok (Str.toUtf8 input)

# Decode a base32 encoded string into a string.
decodeStr : Str -> Result Str [InvalidBase32Char U8]
decodeStr = \input ->
    when decodeBytes input is
        Ok bytes ->
            when Str.fromUtf8 bytes is
                Ok str -> Ok str
                Err _ -> crash "invalid utf8"

        Err err -> Err err

expect decodeStr "" == Ok ""
expect decodeStr "CR" == Ok "f"
expect decodeStr "CSQG" == Ok "fo"
expect decodeStr "CSQPY" == Ok "foo"
expect decodeStr "CSQPYRG" == Ok "foob"
expect decodeStr "CSQPYRK1" == Ok "fooba"
expect decodeStr "CSQPYRK1E8" == Ok "foobar"
