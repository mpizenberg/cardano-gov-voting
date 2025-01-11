module Helper exposing (prettyAddr, shortenedHex, viewNumberInput)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address)
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Events


prettyAddr : Address -> String
prettyAddr address =
    let
        addrHex =
            Bytes.toHex (Address.toBytes address)
    in
    String.slice 0 8 addrHex
        ++ "..."
        ++ String.slice -8 (String.length addrHex) addrHex


shortenedHex : Int -> String -> String
shortenedHex width bytesHex =
    String.slice 0 width bytesHex
        ++ "..."
        ++ String.slice -width (String.length bytesHex) bytesHex



-- View


viewNumberInput : String -> Int -> (String -> msg) -> Html msg
viewNumberInput label n msgOnInput =
    Html.p []
        [ text label
        , Html.input
            [ HA.type_ "number"
            , HA.value (String.fromInt n)
            , Html.Events.onInput msgOnInput
            ]
            []
        ]
