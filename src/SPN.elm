module SPN exposing (calculatePermutation, calculateSBox, calculateXor, convertBinaryToHex, convertHexToBinary, convertHexToDecimal, convertSBoxValue, encrypt, generateRoundKey, generateRoundKeys, round, sBox, transpose)

import Binary


encrypt : String -> String -> String
encrypt plaintext key =
    String.fromList (round 0 (String.toList plaintext) (generateRoundKeys 0 key))


generateRoundKeys : Int -> String -> List (List Char)
generateRoundKeys offset key =
    if offset >= 5 then
        []

    else
        [ generateRoundKey offset key ] ++ generateRoundKeys (offset + 1) key


generateRoundKey : Int -> String -> List Char
generateRoundKey offset key =
    String.toList (String.slice (0 + offset) (4 + offset) key)


round : Int -> List Char -> List (List Char) -> List Char
round n state roundKeys =
    case roundKeys of
        [] ->
            []

        [ roundKey ] ->
            if n == 0 then
                calculateXor (List.map2 Tuple.pair state roundKey)

            else if n > 0 && n < 4 then
                calculateXor (List.map2 Tuple.pair (calculatePermutation (calculateSBox state)) roundKey)

            else
                calculateXor (List.map2 Tuple.pair (calculateSBox state) roundKey)

        x :: xs ->
            round (n + 1) (round n state [ x ]) xs


calculateXor : List ( Char, Char ) -> List Char
calculateXor list =
    case list of
        [] ->
            []

        [ ( x, y ) ] ->
            String.toList (Binary.toHex (Binary.xor (Binary.fromHex (String.fromChar x)) (Binary.fromHex (String.fromChar y))))

        x :: xs ->
            calculateXor [ x ] ++ calculateXor xs


sBox : List ( Int, Char )
sBox =
    List.indexedMap Tuple.pair [ 'E', '4', 'D', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ]


calculateSBox : List Char -> List Char
calculateSBox state =
    List.map (\hex -> convertSBoxValue hex) state


convertSBoxValue : Char -> Char
convertSBoxValue hex =
    case List.unzip (List.filter (\( i, _ ) -> i == convertHexToDecimal hex) sBox) of
        ( _, [ value ] ) ->
            value

        ( _, _ ) ->
            '0'


convertHexToDecimal : Char -> Int
convertHexToDecimal hex =
    Binary.toDecimal (Binary.fromHex (String.fromChar hex))


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
