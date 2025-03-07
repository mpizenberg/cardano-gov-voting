module Helper exposing (prettyAdaLovelace, prettyAddr, shortenedHex, viewNumberInput, viewNumberInputInline, textFieldInline, viewButton, viewSelect, textField, viewTextarea, firstTextField, labeledField, formRow, formContainer, actionRow)

{-| Helper module for miscellaneous functions that didn’t fit elsewhere,
and are potentially useful in multiple places.
-}

import Bytes.Comparable as Bytes
import Cardano.Address as Address exposing (Address)
import Html exposing (Html, text, button)
import Html.Attributes as HA
import Html.Events exposing (onClick)
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
    Html.div []
        [ Html.label [ 
            HA.style "display" "block",
            HA.style "font-weight" "500",
            HA.style "color" "#374151",
            HA.style "margin-bottom" "0.25rem"
          ] 
          [ text label ]
        , Html.input
            [ HA.type_ "number"
            , HA.value (String.fromInt n)
            , HA.min "0"
            , Html.Events.onInput msgOnInput
            , HA.style "background-color" "transparent"
            , HA.style "border-top" "none"
            , HA.style "border-left" "none"
            , HA.style "border-right" "none"
            , HA.style "border-bottom" "1px solid #C6C6C6"
            , HA.style "padding-left" "0.25rem"
            , HA.style "padding-right" "0.25rem"
            , HA.style "padding-top" "0.5rem"
            , HA.style "padding-bottom" "0.5rem"
            , HA.style "margin-bottom" "0.5rem"
            , HA.style "width" "8rem"
            , HA.style "outline" "none"
            ]
            []
        ]

viewNumberInputInline : Int -> (String -> msg) -> Html msg
viewNumberInputInline value toMsg =
    Html.span [ HA.class "inline-block ml-2" ]
        [ Html.input
            [ HA.type_ "number"
            , HA.value (String.fromInt value)
            , Html.Events.onInput toMsg
            , HA.style "background-color" "#C6C6C6"
            , HA.style "padding" "0.5rem 0.75rem"
            , HA.style "border-radius" "4px"
            , HA.style "border" "1px solid #ccc"
            , HA.style "width" "80px" 
            ]
            []
        ]

textFieldInline : String -> String -> (String -> msg) -> Html msg
textFieldInline label value toMsg =
    Html.span [ HA.class "inline-block mr-2" ]
        [ Html.input
            [ HA.type_ "text"
            , HA.value value
            , Html.Events.onInput toMsg
            , HA.style "background-color" "transparent"
            , HA.style "width" "100%"
            , HA.style "padding" "0.5rem 0"
            , HA.style "border-top" "none"
            , HA.style "border-left" "none"
            , HA.style "border-right" "none"
            , HA.style "border-bottom" "1px solid #7A7A7A"
            , HA.style "border-radius" "0"
            , HA.style "outline" "none"
            , HA.style "box-shadow" "none"
            ]
            []
        ]

-- The viewSelect already looks good!

-- Add a new labeledField function for the reference form layout
labeledField : String -> Html msg -> Html msg
labeledField labelText field =
    Html.div [ HA.class "w-1/3 pl-4 first:pl-0" ]
        [ Html.label [ HA.class "block mb-1 text-sm" ] [ text labelText ]
        , field
        ]

-- Add a formRow helper for reference form rows
formRow : List (Html msg) -> Html msg
formRow fields =
    Html.div [ HA.class "flex items-center mb-4" ] fields

-- Add a formContainer for each reference item
formContainer : List (Html msg) -> Html msg
formContainer content =
    Html.div [ HA.class "py-4 border-b", HA.style "border-color" "#C6C6C6"  ] content

-- Add an actionRow for the delete button
actionRow : Html msg -> Html msg
actionRow button =
    Html.div [ HA.class "flex justify-end" ] [ button ]
viewButton : String -> msg -> Html msg
viewButton label msg =
    button
        [ onClick msg
        , HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "white-space" "nowrap"
        , HA.style "border-radius" "9999px"
        , HA.style "font-size" "0.875rem"
        , HA.style "font-weight" "500"
        , HA.style "transition" "all 0.2s"
        , HA.style "outline" "none"
        , HA.style "ring-offset" "background"
        , HA.style "focus-visible:ring" "2px"
        , HA.style "focus-visible:ring-color" "ring"
        , HA.style "focus-visible:ring-offset" "2px"
        , HA.style "background-color" "#272727" 
        , HA.style "color" "#f7fafc"  
        , HA.style "hover:bg-color" "#f9fafb"  
        , HA.style "hover:text-color" "#1a202c"
        , HA.style "height" "4rem"
        , HA.style "padding-left" "1.5rem"
        , HA.style "padding-right" "1.5rem"
        , HA.style "padding-top" "1.25rem"
        , HA.style "padding-bottom" "1.25rem"
        , HA.style "margin-top" "0.5rem"
        , HA.style "margin-bottom" "0.5em"
        ]
        [ text label ]

viewSelect : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewSelect attributes options =
    Html.select
        ([ HA.style "background-color" "transparent"
         , HA.style "height" "40px"
         , HA.style "border-top" "none"
         , HA.style "border-left" "none"
         , HA.style "border-right" "none"
         , HA.style "border-bottom" "1px solid #7A7A7A"
         , HA.style "border-radius" "0"  
         , HA.style "outline" "none"     
         , HA.style "box-shadow" "none"  
         , HA.style "margin-top" "1px"
         , HA.class "w-full"
         ] ++ attributes)
        options

firstTextField : String -> String -> (String -> msg) -> Html msg
firstTextField label value toMsg =
    Html.span [ HA.style "display" "block", HA.style "margin-bottom" "0.5rem" ]
        [ Html.label [] [ text <| label ++ " " ]
        , Html.input
            [ HA.type_ "text"
            , HA.value value
            , Html.Events.onInput toMsg
            , HA.style "background-color" "transparent"
            , HA.style "border-bottom" "1px solid #7A7A7A"
            , HA.style "border-top" "none"
            , HA.style "border-left" "none"
            , HA.style "border-right" "none"
            , HA.class "w-full"   
            , HA.style "width" "100%"
            , HA.style "padding" "0.5rem 0.75rem"
            ]
            []
        ]
textField : String -> String -> (String -> msg) -> Html msg
textField label value toMsg =
    Html.span [ HA.style "display" "block", HA.style "margin-bottom" "0.5rem" ]
        [ Html.label [] [ text <| label ++ " " ]
        , Html.input
            [ HA.type_ "text"
            , HA.value value
            , Html.Events.onInput toMsg
            , HA.style "background-color" "#C6C6C6"
            , HA.style "width" "100%"
            , HA.style "padding" "0.5rem 0.75rem"
            ]
            []
        ]

viewTextarea : String -> (String -> msg) -> Html msg
viewTextarea value onInputMsg =
    Html.textarea
        [ HA.value value
        , Html.Events.onInput onInputMsg
        , HA.style "background-color" "#C6C6C6"
        , HA.style "padding" "10px"
        , HA.style "height" "100px"
        , HA.style "border-radius" "4px"
        , HA.class "w-full rounded"
        , HA.style "margin-bottom" "40px"
        ]
        []