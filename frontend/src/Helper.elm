module Helper exposing (prettyAdaLovelace, prettyAddr, shortenedHex, viewNumberInput)

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address)
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Events
import Natural exposing (Natural)
import Numeral



-- String formatting


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


prettyAdaLovelace : Natural -> String
prettyAdaLovelace n =
    Natural.divBy (Natural.fromSafeInt 1000) n
        |> Maybe.withDefault Natural.zero
        -- At this point we have /1000 the amount of lovelace
        -- so for any practical purpose we can make the assumption
        -- that it is within the JS safe integer range
        |> Natural.toInt
        |> (\millis ->
                -- if the amount is above 1 Ada we use .00 precision
                -- otherwise we use .000 precision
                if millis >= 1000 then
                    "₳" ++ Numeral.format "0.00a" (toFloat millis / 1000)

                else
                    "₳" ++ Numeral.format "0.000a" (toFloat millis / 1000)
           )



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
