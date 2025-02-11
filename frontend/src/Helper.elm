module Helper exposing (prettyAdaLovelace, prettyAddr, shortenedHex, viewNumberInput)

{-| Helper module for miscellaneous functions that didn’t fit elsewhere,
and are potentially useful in multiple places.
-}

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address)
import Html exposing (Html, text)
import Html.Attributes as HA
import Html.Events
import Natural exposing (Natural)
import Numeral



-- String formatting


{-| Display the Hex form of an address with only the first and last few characters.
-}
prettyAddr : Address -> String
prettyAddr address =
    Bytes.toHex (Address.toBytes address)
        |> shortenedHex 8


{-| Shorten some string, by only keeping the first and last few characters.
-}
shortenedHex : Int -> String -> String
shortenedHex width bytesHex =
    String.slice 0 width bytesHex
        ++ "..."
        ++ String.slice -width (String.length bytesHex) bytesHex


{-| Display a Lovelace amount as a pretty Ada (₳) amount.

  - `42000 -> "₳0.042"`
  - `69427000000 -> "₳69.43k"`

The number is formatted to automatically use the most adequate unit (k, m, ...).
For amounts below 1₳, the number is formatted with 3 decimals.
For amounts over 1₳, the number is formatted with 2 decimals.

-}
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


{-| Helper view function for a simple number input.
-}
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
