module SPN exposing (calculatePermutation, calculateSBox, sBoxInverse, calculateXor, convertBinaryToHex, convertHexToBinary, convertHexToDecimal, convertSBoxValue, decrypt, encrypt, generateRoundKey, generateRoundKeys, round, createSBox, transpose, reverseRoundKeys, reverseRound, convertDecimalToHex)

import Binary


createSBox : String -> List ( Int, Char )
createSBox sbox =
    List.indexedMap Tuple.pair (String.toList sbox)


encrypt : String -> String -> String -> String
encrypt plaintext key sbox =
    String.fromList (round 0 (String.toList plaintext) (generateRoundKeys 0 key) (createSBox sbox))


decrypt : String -> String -> String -> String
decrypt ciphertext key sbox =
    String.fromList (reverseRound 0 (String.toList ciphertext) (reverseRoundKeys (generateRoundKeys 0 key)) (sBoxInverse (createSBox sbox)))


generateRoundKeys : Int -> String -> List (List Char)
generateRoundKeys offset key =
    if offset >= 5 then
        []

    else
        [ generateRoundKey offset key ] ++ generateRoundKeys (offset + 1) key


generateRoundKey :  Int -> String -> List Char
generateRoundKey offset key =
     String.toList (String.slice (0 + offset) (4 + offset) key)


reverseRoundKeys : List (List Char) -> List (List Char)
reverseRoundKeys roundKeys =
    List.reverse roundKeys


round : Int -> List Char -> List (List Char) -> List (Int, Char) -> List Char
round n state roundKeys sBox =
    case roundKeys of
        [] ->
            []

        [ roundKey ] ->
            if n == 0 then
                calculateXor (List.map2 Tuple.pair state roundKey)

            else if n > 0 && n < 4 then
                calculateXor (List.map2 Tuple.pair (calculatePermutation (calculateSBox state sBox)) roundKey)

            else
                calculateXor (List.map2 Tuple.pair (calculateSBox state sBox) roundKey)

        x :: xs ->
            round (n + 1) (round n state [ x ] sBox) xs sBox


reverseRound : Int -> List Char -> List (List Char) -> List (Int, Char) -> List Char
reverseRound n state roundKeys sBox =
    case roundKeys of
        [] ->
            []

        [ roundKey ] ->
            if n == 0 then
                calculateSBox (calculateXor (List.map2 Tuple.pair state roundKey)) sBox

            else if n > 0 && n < 4 then
                calculateSBox (calculatePermutation (calculateXor (List.map2 Tuple.pair state roundKey))) sBox

            else
                calculateXor (List.map2 Tuple.pair state roundKey)

        x :: xs ->
            reverseRound (n + 1) (reverseRound n state [ x ] sBox) xs sBox


calculateXor : List ( Char, Char ) -> List Char
calculateXor list =
    case list of
        [] ->
            []

        [ ( x, y ) ] ->
            String.toList (Binary.toHex (Binary.xor (Binary.fromHex (String.fromChar x)) (Binary.fromHex (String.fromChar y))))

        x :: xs ->
            calculateXor [ x ] ++ calculateXor xs


calculateSBox : List Char -> List ( Int, Char )-> List Char
calculateSBox state sBox =
    List.map (\hex -> convertSBoxValue hex sBox) state


convertSBoxValue : Char -> List ( Int, Char ) -> Char
convertSBoxValue hex sBox =
    case List.unzip (List.filter (\( i, _ ) -> i == convertHexToDecimal hex) sBox) of
        ( _, [ value ] ) ->
            value

        ( _, _ ) ->
            '0'


sBoxInverse : List ( Int, Char ) -> List ( Int, Char )
sBoxInverse sBox = List.map (\(a, b) -> Tuple.pair (convertHexToDecimal b) (convertDecimalToHex a)) sBox


convertHexToDecimal : Char -> Int
convertHexToDecimal hex =
    Binary.toDecimal (Binary.fromHex (String.fromChar hex))


convertDecimalToHex : Int -> Char
convertDecimalToHex decimal =
     case String.uncons (Binary.toHex (Binary.fromDecimal decimal)) of
         Nothing -> ' '
         Just (hex, _) -> hex


calculatePermutation : List Char -> List Char
calculatePermutation state =
    convertBinaryToHex (transpose (convertHexToBinary state))


transpose : List (List Int) -> List (List Int)
transpose list =
    let
        heads =
            List.concat (List.map (List.take 1) list)

        tails =
            List.map (List.drop 1) list
    in
        if List.length heads == List.length list then
            heads :: transpose tails

        else
            []


convertHexToBinary : List Char -> List (List Int)
convertHexToBinary state =
    List.map (\hex -> Binary.toIntegers (Binary.fromHex (String.fromChar hex))) state


convertBinaryToHex : List (List Int) -> List Char
convertBinaryToHex state =
    String.toList (String.concat (List.map (\binary -> Binary.toHex (Binary.fromIntegers binary)) state))
