module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Regex
import SPN exposing (encrypt)



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
    , ciphertext : String
    , error : Bool
    }


init : Model
init =
    Model "" "" "" False



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Plaintext" model.plaintext Plaintext
        , viewInput "text" "Key" model.key Key
        , viewValidation model
        , button [ onClick Encrypt ] [ text "Encrypt" ]
        , div [] [ text model.ciphertext ]
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
    if model.error && String.length model.plaintext /= 4 then
        div [ style "color" "red" ] [ text "Plaintext must be 4 characters long" ]

    else if model.error && Regex.contains hexadecimalInput model.plaintext then
        div [ style "color" "red" ] [ text "Plaintext must be a hexadecimal string" ]

    else if model.error && String.length model.key /= 8 then
        div [ style "color" "red" ] [ text "Key must be 8 characters long" ]

    else if model.error && Regex.contains hexadecimalInput model.key then
        div [ style "color" "red" ] [ text "Key must be a hexadecimal string" ]

    else
        div [] []



-- UPDATE


type Msg
    = Plaintext String
    | Key String
    | Encrypt


update : Msg -> Model -> Model
update msg model =
    case msg of
        Plaintext plaintext ->
            { model | plaintext = plaintext, error = False }

        Key key ->
            { model | key = key, error = False }

        Encrypt ->
            if validate model then
                { model | ciphertext = encrypt model.plaintext model.key }

            else
                { model | error = True }


validate : Model -> Bool
validate model =
    (String.length model.plaintext == 4)
        && Regex.contains hexadecimalInput model.plaintext
        == False
        && (String.length model.key == 8)
        && Regex.contains hexadecimalInput model.key
        == False
