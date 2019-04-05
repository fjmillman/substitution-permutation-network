module SPNTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import SPN


suite : Test
suite =
    describe "SPN"
        [ describe "encrypt"
            [ test "encrypts a plaintext" <|
              \_ ->
                  Expect.equal "C830" (SPN.encrypt "1234" "12345678")
            ]
         , describe "generateRoundKeys"
            [ test "generates a round key from a key with offset 0" <|
                \_ ->
                    let
                        roundKeys =
                            [ ['1', '2', '3', '4']
                            , ['2', '3', '4', '5']
                            , ['3', '4', '5', '6']
                            , ['4', '5', '6', '7']
                            , ['5', '6', '7', '8']
                            ]
                    in
                        Expect.equal roundKeys (SPN.generateRoundKeys 0 "12345678")
            ]
        , describe "generateRoundKey"
            [ test "generates a round key from a key with offset 0" <|
                \_ ->
                    Expect.equal ['1', '2', '3', '4'] (SPN.generateRoundKey 0 "12345678")
            , test "generates a round key from a key with offset 1" <|
                \_ ->
                    Expect.equal ['2', '3', '4', '5'] (SPN.generateRoundKey 1 "12345678")
            , test "generates a round key from a key with offset 2" <|
                \_ ->
                    Expect.equal ['3', '4', '5', '6'] (SPN.generateRoundKey 2 "12345678")
            , test "generates a round key from a key with offset 3" <|
                \_ ->
                    Expect.equal ['4', '5', '6', '7'] (SPN.generateRoundKey 3 "12345678")
            , test "generates a round key from a key with offset 4" <|
                \_ ->
                    Expect.equal ['5', '6', '7', '8'] (SPN.generateRoundKey 4 "12345678")
            ]
        , describe "round"
            [ test "generates an update in state for a round" <|
                \_ ->
                    let
                        roundKeys =
                            [ ['1', '2', '3', '4']
                            , ['2', '3', '4', '5']
                            , ['3', '4', '5', '6']
                            , ['4', '5', '6', '7']
                            , ['5', '6', '7', '8']
                            ]
                    in
                        Expect.equal ['C', '8', '3', '0'] (SPN.round 0 ['1', '2', '3', '4'] roundKeys)
            ]
        , describe "calculateXor"
            [ test "calculates the xor between a list of tuple char pairs" <|
                \_ ->
                    let
                        tupleList =
                            [ ('4', '1')
                            , ('D', '2')
                            , ('3', '3')
                            , ('4', '4')
                            ]
                    in
                        Expect.equal ['5', 'F', '0', '0'] (SPN.calculateXor tupleList)
            ]
        , describe "calculateSBox"
            [ test "converts a list of chars using the SBox" <|
                \_ ->
                    Expect.equal ['4', 'D', '3', '4'] (SPN.calculateSBox ['1', '2', '3', '4'])
            ]
        , describe "convertSBoxValue"
            [ test "converts a value using the SBox" <|
                \_ ->
                    Expect.equal '4' (SPN.convertSBoxValue '1')
            ]
        , describe "convertHexToDecimal"
            [ test "converts a hex char to its decimal equivalent" <|
                \_ ->
                    Expect.equal 15 (SPN.convertHexToDecimal 'F')
            ]
        , describe "calculatePermutation"
            [ test "permutes a list of chars" <|
                \_ ->
                    Expect.equal ['0', '1', '6', 'A'] (SPN.calculatePermutation ['1', '2', '3', '4'])
            ]
        , describe "transpose"
            [ test "transposes a list of a list of binary integers" <|
                \_ ->
                    let
                        binaryList =
                            [ [0, 0, 0, 1]
                            , [0, 0, 1, 0]
                            , [0, 0, 1, 1]
                            , [0, 1, 0, 0]
                            ]

                        transposedBinaryList =
                            [ [0, 0, 0, 0]
                            , [0, 0, 0, 1]
                            , [0, 1, 1, 0]
                            , [1, 0, 1, 0]
                            ]
                    in
                        Expect.equal transposedBinaryList (SPN.transpose binaryList)
            ]
        , describe "convertHexToBinary"
            [ test "converts a list of hex chars to a list of binary integers" <|
                \_ ->
                    let
                        binaryList =
                            [ [0, 0, 0, 1]
                            , [0, 0, 1, 0]
                            , [0, 0, 1, 1]
                            , [0, 1, 0, 0]
                            ]
                    in
                        Expect.equal binaryList (SPN.convertHexToBinary ['1', '2', '3', '4'])
            ]
        , describe "convertBinaryToHex"
            [ test "converts a list of binary integers to a list of hex chars" <|
                \_ ->
                    let
                        binaryList =
                            [ [0, 0, 0, 1]
                            , [0, 0, 1, 0]
                            , [0, 0, 1, 1]
                            , [0, 1, 0, 0]
                            ]
                    in
                        Expect.equal ['1', '2', '3', '4'] (SPN.convertBinaryToHex binaryList)
            ]
        ]
