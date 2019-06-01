module SPNTest exposing (suite)

import Expect exposing (Expectation)
import SPN
import Test exposing (..)


suite : Test
suite =
    describe "SPN"
        [ describe "encrypt"
            [ test "encrypts a plaintext" <|
                \_ ->
                    Expect.equal "28E6" (SPN.encrypt "7EA1" "E767903D" "41E8D62BFC973A50")
            ]
        , describe "decrypt"
            [ test "decrypts a ciphertext" <|
                \_ ->
                    Expect.equal "7EA1" (SPN.encrypt "28E6" "E767903D" "41E8D62BFC973A50")
            ]
        , describe "generateRoundKeys"
            [ test "generates a round key from a key with offset 0" <|
                \_ ->
                    let
                        roundKeys =
                            [ [ '1', '2', '3', '4' ]
                            , [ '2', '3', '4', '5' ]
                            , [ '3', '4', '5', '6' ]
                            , [ '4', '5', '6', '7' ]
                            , [ '5', '6', '7', '8' ]
                            ]
                    in
                    Expect.equal roundKeys (SPN.generateRoundKeys 0 "12345678")
            ]
        , describe "generateRoundKey"
            [ test "generates a round key from a key with offset 0" <|
                \_ ->
                    Expect.equal [ '1', '2', '3', '4' ] (SPN.generateRoundKey 0 "12345678")
            , test "generates a round key from a key with offset 1" <|
                \_ ->
                    Expect.equal [ '2', '3', '4', '5' ] (SPN.generateRoundKey 1 "12345678")
            , test "generates a round key from a key with offset 2" <|
                \_ ->
                    Expect.equal [ '3', '4', '5', '6' ] (SPN.generateRoundKey 2 "12345678")
            , test "generates a round key from a key with offset 3" <|
                \_ ->
                    Expect.equal [ '4', '5', '6', '7' ] (SPN.generateRoundKey 3 "12345678")
            , test "generates a round key from a key with offset 4" <|
                \_ ->
                    Expect.equal [ '5', '6', '7', '8' ] (SPN.generateRoundKey 4 "12345678")
            ]
        , describe "reverseRoundKeys"
            [ test "reverses the round keys" <|
                \_ ->
                    let
                        roundKeys =
                            [ [ '1', '2', '3', '4' ]
                            , [ '2', '3', '4', '5' ]
                            , [ '3', '4', '5', '6' ]
                            , [ '4', '5', '6', '7' ]
                            , [ '5', '6', '7', '8' ]
                            ]

                        reversedRoundKeys =
                            [ [ '5', '6', '7', '8' ]
                            , [ '4', '5', '6', '7' ]
                            , [ '3', '4', '5', '6' ]
                            , [ '2', '3', '4', '5' ]
                            , [ '1', '2', '3', '4' ]
                            ]
                    in
                    Expect.equal reversedRoundKeys (SPN.reverseRoundKeys roundKeys)
            ]
        , describe "round"
            [ test "generates an update in state for an encryption round" <|
                \_ ->
                    let
                        roundKeys =
                            [ [ '1', '2', '3', '4' ]
                            , [ '2', '3', '4', '5' ]
                            , [ '3', '4', '5', '6' ]
                            , [ '4', '5', '6', '7' ]
                            , [ '5', '6', '7', '8' ]
                            ]
                    in
                    Expect.equal [ '0', '3', 'A', 'E' ] (SPN.round 0 [ '1', '2', '3', '4' ] roundKeys (SPN.createSBox "41E8D62BFC973A50"))
            ]
        , describe "reverseRound"
            [ test "generates an update in state for a decryption round" <|
                \_ ->
                    let
                        roundKeys =
                            [ [ '5', '6', '7', '8' ]
                            , [ '4', '5', '6', '7' ]
                            , [ '3', '4', '5', '6' ]
                            , [ '2', '3', '4', '5' ]
                            , [ '1', '2', '3', '4' ]
                            ]
                    in
                    Expect.equal [ '1', '2', '3', '4' ] (SPN.reverseRound 0 [ '0', '3', 'A', 'E' ] roundKeys (SPN.sBoxInverse (SPN.createSBox "41E8D62BFC973A50")))
            ]
        , describe "calculateXor"
            [ test "calculates the xor between a list of tuple char pairs" <|
                \_ ->
                    let
                        tupleList =
                            [ ( '4', '1' )
                            , ( 'D', '2' )
                            , ( '3', '3' )
                            , ( '4', '4' )
                            ]
                    in
                    Expect.equal [ '5', 'F', '0', '0' ] (SPN.calculateXor tupleList)
            ]
        , describe "sBoxInverse"
            [ test "inverts the sBox" <|
                \_ ->
                    Expect.equal [ ( 4, '0' ), ( 1, '1' ), ( 14, '2' ), ( 8, '3' ), ( 13, '4' ), ( 6, '5' ), ( 2, '6' ), ( 11, '7' ), ( 15, '8' ), ( 12, '9' ), ( 9, 'A' ), ( 7, 'B' ), ( 3, 'C' ), ( 10, 'D' ), ( 5, 'E' ), ( 0, 'F' ) ] (SPN.sBoxInverse (SPN.createSBox "41E8D62BFC973A50"))
            ]
        , describe "calculateSBox"
            [ test "converts a list of chars using the SBox" <|
                \_ ->
                    Expect.equal [ '1', 'E', '8', 'D' ] (SPN.calculateSBox [ '1', '2', '3', '4' ] (SPN.createSBox "41E8D62BFC973A50"))
            ]
        , describe "convertSBoxValue"
            [ test "converts a value using the SBox" <|
                \_ ->
                    Expect.equal '1' (SPN.convertSBoxValue '1' (SPN.createSBox "41E8D62BFC973A50"))
            ]
        , describe "convertHexToDecimal"
            [ test "converts a hex char to its decimal equivalent" <|
                \_ ->
                    Expect.equal 15 (SPN.convertHexToDecimal 'F')
            ]
        , describe "convertDecimalToHex"
            [ test "converts a decimal to its hex char equivalent" <|
                \_ ->
                    Expect.equal 'F' (SPN.convertDecimalToHex 15)
            ]
        , describe "calculatePermutation"
            [ test "permutes a list of chars" <|
                \_ ->
                    Expect.equal [ '0', '1', '6', 'A' ] (SPN.calculatePermutation [ '1', '2', '3', '4' ])
            ]
        , describe "transpose"
            [ test "transposes a list of a list of binary integers" <|
                \_ ->
                    let
                        binaryList =
                            [ [ 0, 0, 0, 1 ]
                            , [ 0, 0, 1, 0 ]
                            , [ 0, 0, 1, 1 ]
                            , [ 0, 1, 0, 0 ]
                            ]

                        transposedBinaryList =
                            [ [ 0, 0, 0, 0 ]
                            , [ 0, 0, 0, 1 ]
                            , [ 0, 1, 1, 0 ]
                            , [ 1, 0, 1, 0 ]
                            ]
                    in
                    Expect.equal transposedBinaryList (SPN.transpose binaryList)
            ]
        , describe "convertHexToBinary"
            [ test "converts a list of hex chars to a list of binary integers" <|
                \_ ->
                    let
                        binaryList =
                            [ [ 0, 0, 0, 1 ]
                            , [ 0, 0, 1, 0 ]
                            , [ 0, 0, 1, 1 ]
                            , [ 0, 1, 0, 0 ]
                            ]
                    in
                    Expect.equal binaryList (SPN.convertHexToBinary [ '1', '2', '3', '4' ])
            ]
        , describe "convertBinaryToHex"
            [ test "converts a list of binary integers to a list of hex chars" <|
                \_ ->
                    let
                        binaryList =
                            [ [ 0, 0, 0, 1 ]
                            , [ 0, 0, 1, 0 ]
                            , [ 0, 0, 1, 1 ]
                            , [ 0, 1, 0, 0 ]
                            ]
                    in
                    Expect.equal [ '1', '2', '3', '4' ] (SPN.convertBinaryToHex binaryList)
            ]
        ]
