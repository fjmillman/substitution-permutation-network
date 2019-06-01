module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex
import SPN exposing (encrypt, decrypt)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { plaintext : String
    , key : String
    , sbox : String
    , ciphertext : String
    , mode : String
    , error : Bool
    }


init : Model
init =
    Model "" "" "" "" "" False



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Plaintext" model.plaintext Plaintext
        , viewInput "text" "Key" model.key Key
        , viewInput "text" "Sbox" model.sbox Sbox
        , viewInput "text" "Ciphertext" model.ciphertext Ciphertext
        , viewValidation model
        , button [ onClick Encrypt ] [ text "Encrypt" ]
        , button [ onClick Decrypt ] [ text "Decrypt" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


hexadecimalInput : Regex.Regex
hexadecimalInput =
    Maybe.withDefault Regex.never <|
        Regex.fromString "[^0-9a-fA-F]+"


viewValidation : Model -> Html msg
viewValidation model =
    if model.error && model.mode == "Encrypt" && String.length model.plaintext /= 4 then
        div [ style "color" "red" ] [ text "Plaintext must be 4 characters long" ]

    else if model.error && model.mode == "Encrypt" && Regex.contains hexadecimalInput model.plaintext then
        div [ style "color" "red" ] [ text "Plaintext must be a hexadecimal string" ]

    else if model.error && String.length model.key /= 8 then
        div [ style "color" "red" ] [ text "Key must be 8 characters long" ]

    else if model.error && Regex.contains hexadecimalInput model.key then
        div [ style "color" "red" ] [ text "Key must be a hexadecimal string" ]

    else if model.error && String.length model.sbox /= 16 then
        div [ style "color" "red" ] [ text "sBox must be 8 characters long" ]

    else if model.error && Regex.contains hexadecimalInput model.sbox then
        div [ style "color" "red" ] [ text "sBox must be a hexadecimal string" ]

    else if model.error && model.mode == "Decrypt" && String.length model.ciphertext /= 4 then
        div [ style "color" "red" ] [ text "Ciphertext must be 4 characters long" ]

    else if model.error && model.mode == "Decrypt" && Regex.contains hexadecimalInput model.ciphertext then
        div [ style "color" "red" ] [ text "Ciphertext must be a hexadecimal string" ]

    else
        div [] []



-- UPDATE


type Msg
    = Plaintext String
    | Key String
    | Sbox String
    | Ciphertext String
    | Encrypt
    | Decrypt


update : Msg -> Model -> Model
update msg model =
    case msg of
        Plaintext plaintext ->
            { model | plaintext = plaintext, error = False, mode = "" }

        Key key ->
            { model | key = key, error = False, mode = "" }

        Sbox sbox ->
            { model | sbox = sbox, error = False, mode = "" }

        Ciphertext ciphertext ->
            { model | ciphertext = ciphertext, error = False, mode = "" }

        Encrypt ->
            if validatePlaintext model && validateKey model && validateSbox model then
                { model | ciphertext = encrypt model.plaintext model.key model.sbox }

            else
                { model | error = True, mode = "Encrypt" }

        Decrypt ->
            if validateCiphertext model && validateKey model && validateSbox model then
                { model | plaintext = decrypt model.ciphertext model.key model.sbox }

            else
                { model | error = True, mode = "Decrypt"  }


validatePlaintext : Model -> Bool
validatePlaintext model =
    (String.length model.plaintext == 4)
        && Regex.contains hexadecimalInput model.plaintext
        == False

validateCiphertext : Model -> Bool
validateCiphertext model =
    (String.length model.ciphertext == 4)
        && Regex.contains hexadecimalInput model.ciphertext
        == False

validateKey : Model -> Bool
validateKey model =
    (String.length model.key == 8)
       && Regex.contains hexadecimalInput model.key
       == False

validateSbox : Model -> Bool
validateSbox model =
    (String.length model.sbox == 16)
       && Regex.contains hexadecimalInput model.sbox
       == False
