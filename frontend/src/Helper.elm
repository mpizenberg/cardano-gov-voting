module Helper exposing
    ( shortenedHex, prettyAdaLovelace
    , viewNumberInput, textFieldInline, firstTextField, viewSelect
    , labeledField, textField, viewTextarea, radioInput
    , formContainer, boxContainer
    , viewButton, viewWalletButton
    , applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle
    , viewActionTypeIcon
    , viewNetworkButton
    )

{-| Helper module for miscellaneous functions that didn’t fit elsewhere,
and are potentially useful in multiple places.


# String formatting

@docs shortenedHex, prettyAdaLovelace


# Form elements

@docs viewNumberInput, textFieldInline, firstTextField, viewSelect

@docs labeledField, textField, viewTextarea, radioInput


# Containers

@docs formContainer, boxContainer


# Buttons

@docs viewButton, viewWalletButton


# Wallet Styling

@docs applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle


# Governance Icons

@docs viewActionTypeIcon

-}

import Cardano.Address exposing (NetworkId(..))
import Html exposing (Html, button, text)
import Html.Attributes as HA
import Html.Events exposing (onClick, onMouseLeave, onMouseOver)
import Natural exposing (Natural)
import Numeral



-- STRING FORMATTING ###########################################################


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



-- FORM ELEMENTS ###############################################################


{-| Helper view function for a simple number input.
-}
viewNumberInput : String -> Int -> (String -> msg) -> Html msg
viewNumberInput label n msgOnInput =
    Html.div []
        [ Html.label
            [ HA.style "display" "block"
            , HA.style "font-weight" "500"
            , HA.style "color" "#374151"
            , HA.style "margin-bottom" "0.25rem"
            ]
            [ text label ]
        , Html.input
            (inputBaseStyle
                ++ [ HA.type_ "number"
                   , HA.value (String.fromInt n)
                   , HA.min "0"
                   , Html.Events.onInput msgOnInput
                   , HA.style "padding-left" "0.25rem"
                   , HA.style "padding-right" "0.25rem"
                   , HA.style "padding-top" "0.5rem"
                   , HA.style "padding-bottom" "0.5rem"
                   , HA.style "margin-bottom" "0.5rem"
                   , HA.style "width" "8rem"
                   , HA.style "outline" "none"
                   ]
            )
            []
        ]


textFieldInline : String -> (String -> msg) -> Html msg
textFieldInline value toMsg =
    Html.span [ HA.class "inline-block mr-2" ]
        [ Html.input
            (inputBaseStyle
                ++ [ HA.type_ "text"
                   , HA.value value
                   , Html.Events.onInput toMsg
                   , HA.style "width" "100%"
                   , HA.style "padding" "0.5rem 0"
                   , HA.style "border-radius" "0"
                   , HA.style "outline" "none"
                   , HA.style "box-shadow" "none"
                   ]
            )
            []
        ]


firstTextField : String -> String -> (String -> msg) -> Html msg
firstTextField placeholder value toMsg =
    Html.span [ HA.style "display" "block", HA.style "margin-bottom" "0.5rem", HA.style "width" "100%" ]
        [ Html.input
            (inputBaseStyle
                ++ [ HA.type_ "text"
                   , HA.value value
                   , HA.placeholder placeholder
                   , Html.Events.onInput toMsg
                   , HA.class "w-full"
                   , HA.style "width" "100%"
                   , HA.style "padding" "0.5rem 0.2rem"
                   ]
            )
            []
        ]


viewSelect : List (Html.Attribute msg) -> List (Html msg) -> Html msg
viewSelect attributes options =
    Html.select
        ([ HA.style "height" "40px"
         , HA.style "border-radius" "0"
         , HA.style "outline" "none"
         , HA.style "box-shadow" "none"
         , HA.style "margin-top" "1px"
         , HA.class "w-full"
         ]
            ++ inputBaseStyle
            ++ attributes
        )
        options


inputBaseStyle : List (Html.Attribute msg)
inputBaseStyle =
    [ HA.style "background-color" "transparent"
    , HA.style "border-top" "none"
    , HA.style "border-left" "none"
    , HA.style "border-right" "none"
    , HA.style "border-bottom" "1px solid #7A7A7A"
    ]


labeledField : String -> Html msg -> Html msg
labeledField labelText field =
    -- Add a new labeledField function for the reference form layout
    Html.div [ HA.class " pl-4 first:pl-0" ]
        [ Html.label [ HA.class "block mb-1 text-sm" ] [ text labelText ]
        , field
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


radioInput : { group : String, label : String, checked : Bool, onClick : msg } -> Html msg
radioInput { group, label, checked, onClick } =
    Html.div [ HA.class "flex items-center mb-4" ]
        [ Html.input
            [ HA.type_ "radio"
            , HA.name group
            , HA.id label
            , HA.value label
            , HA.checked checked
            , Html.Events.onInput (\_ -> onClick)
            , HA.class "mr-2"
            ]
            []
        , Html.label [ HA.for label, HA.class "text-base" ] [ text label ]
        ]



-- CONTAINERS ##################################################################


formContainer : List (Html msg) -> Html msg
formContainer content =
    Html.div [ HA.class "py-4" ] content


boxContainer : List (Html msg) -> Html msg
boxContainer content =
    Html.div
        [ HA.style "background-color" "#ffffff"
        , HA.style "border-radius" "0.5rem"
        , HA.style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
        , HA.style "padding" "1.5rem"
        ]
        content



-- BUTTONS #####################################################################


viewButton : String -> msg -> Html msg
viewButton label msg =
    button
        (onClick msg
            :: HA.style "margin-top" "0.5rem"
            :: HA.style "margin-bottom" "0.5em"
            :: buttonCommonStyle
        )
        [ text label ]


viewWalletButton : String -> msg -> List (Html msg) -> Html msg
viewWalletButton label msg content =
    button
        (onClick msg :: buttonCommonStyle)
        (text label :: content)


viewNetworkButton : String -> Bool -> (NetworkId -> msg) -> Html msg
viewNetworkButton label isMainnet onChangeMsg =
    let
        networkId =
            if isMainnet then
                Mainnet

            else
                Testnet

        otherNetworkId =
            if isMainnet then
                Testnet

            else
                Mainnet

        otherLabel =
            if isMainnet then
                "Preview"

            else
                "Mainnet"
    in
    Html.div
        [ HA.style "position" "relative"
        , HA.style "display" "inline-block"
        , HA.style "margin-left" "0.75rem"
        , HA.attribute "class" "network-button-container"
        ]
        [ button
            ([ HA.style "display" "flex"
             , HA.style "align-items" "center"
             , HA.style "gap" "0.5rem"
             , HA.style "background-color" "#272727"
             , HA.style "color" "#f7fafc"
             , HA.style "border-radius" "9999px"
             , HA.style "font-size" "0.875rem"
             , HA.style "font-weight" "500"
             , HA.style "padding" "0.5rem 1rem"
             , HA.style "height" "2.5rem"
             , HA.style "transition" "all 0.2s"
             , HA.attribute "data-dropdown-toggle" "network-dropdown"
             ]
                ++ buttonCommonStyle
            )
            [ Html.div
                [ HA.style "display" "flex"
                , HA.style "align-items" "center"
                ]
                [ Html.div
                    [ HA.style "width" "0.75rem"
                    , HA.style "height" "0.75rem"
                    , HA.style "border-radius" "9999px"
                    , HA.style "background-color"
                        (if isMainnet then
                            "#10b981"

                         else
                            "#3b82f6"
                        )
                    , HA.style "margin-right" "0.5rem"
                    ]
                    []
                , text label
                ]
            ]
        , Html.div
            [ HA.id "network-dropdown"
            , HA.style "position" "absolute"
            , HA.style "top" "100%"
            , HA.style "right" "0"
            , HA.style "margin-top" "0.25rem"
            , HA.style "width" "160px"
            , HA.style "background-color" "#ffffff"
            , HA.style "border" "1px solid #e2e8f0"
            , HA.style "border-radius" "0.5rem"
            , HA.style "box-shadow" "0 4px 6px -1px rgba(0, 0, 0, 0.1)"
            , HA.style "z-index" "50"
            , HA.style "display" "none"
            ]
            [ Html.div
                ([ HA.style "padding" "0.5rem 1rem"
                 , HA.style "cursor" "pointer"
                 , HA.style "display" "flex"
                 , HA.style "align-items" "center"
                 , HA.style "gap" "0.5rem"
                 , onClick (onChangeMsg otherNetworkId)
                 ]
                    ++ applyDropdownItemStyle (onChangeMsg otherNetworkId)
                )
                [ Html.div
                    [ HA.style "width" "0.75rem"
                    , HA.style "height" "0.75rem"
                    , HA.style "border-radius" "9999px"
                    , HA.style "background-color"
                        (if not isMainnet then
                            "#10b981"

                         else
                            "#3b82f6"
                        )
                    ]
                    []
                , text otherLabel
                ]
            ]
        ]


buttonCommonStyle : List (Html.Attribute msg)
buttonCommonStyle =
    [ HA.style "display" "inline-flex"
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
    , HA.style "height" "3rem"
    , HA.style "padding-left" "1.5rem"
    , HA.style "padding-right" "1.5rem"
    ]



-- WALLET STYLING ##############################################################


applyDropdownContainerStyle : List (Html.Attribute msg)
applyDropdownContainerStyle =
    HA.style "top" "100%"
        :: HA.style "right" "0"
        :: HA.style "width" "220px"
        :: baseDropdownContainerStyle


applyMobileDropdownContainerStyle : List (Html.Attribute msg)
applyMobileDropdownContainerStyle =
    HA.style "width" "100%"
        :: baseDropdownContainerStyle


baseDropdownContainerStyle : List (Html.Attribute msg)
baseDropdownContainerStyle =
    [ HA.style "position" "absolute"
    , HA.style "margin-top" "0.5rem"
    , HA.style "background-color" "#f8f9fa"
    , HA.style "border" "1px solid #e2e8f0"
    , HA.style "border-radius" "0.5rem"
    , HA.style "box-shadow" "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)"
    , HA.style "z-index" "50"
    , HA.style "padding" "0.5rem 0"
    , HA.style "max-height" "300px"
    , HA.style "overflow-y" "auto"
    ]


applyDropdownItemStyle : msg -> List (Html.Attribute msg)
applyDropdownItemStyle onClickMsg =
    [ HA.class "px-4 py-2 hover:bg-gray-100 cursor-pointer flex items-center"
    , onClick onClickMsg
    , HA.style "padding-left" "1rem"
    , HA.style "padding-right" "1rem"
    , HA.style "padding-top" "0.5rem"
    , HA.style "padding-bottom" "0.5rem"
    , HA.style "cursor" "pointer"
    , HA.style "display" "flex"
    , HA.style "align-items" "center"
    ]


applyWalletIconContainerStyle : List (Html.Attribute msg)
applyWalletIconContainerStyle =
    [ HA.style "width" "20px"
    , HA.style "height" "20px"
    , HA.style "margin-right" "0.5rem"
    , HA.style "display" "flex"
    , HA.style "align-items" "center"
    , HA.style "justify-content" "center"
    ]


applyWalletIconStyle : List (Html.Attribute msg)
applyWalletIconStyle =
    [ HA.style "max-height" "20px"
    , HA.style "max-width" "20px"
    , HA.style "object-fit" "contain"
    ]



-- GOV ICONS ###################################################################


viewActionTypeIcon : String -> Html msg
viewActionTypeIcon actionType =
    let
        iconStyle =
            [ HA.style "margin-left" "0.25rem"
            , HA.style "display" "inline-flex"
            , HA.style "align-items" "center"
            ]
    in
    case actionType of
        "treasuryWithdrawals" ->
            Html.span iconStyle [ text "💰" ]

        "constitution" ->
            Html.span iconStyle [ text "📜" ]

        "constitutionalCommittee" ->
            Html.span iconStyle [ text "👥" ]

        "information" ->
            Html.span iconStyle [ text "ℹ️" ]

        "noConfidence" ->
            Html.span iconStyle [ text "❌" ]

        "protocolParametersUpdate" ->
            Html.span iconStyle [ text "⚙️" ]

        "hardfork" ->
            Html.span iconStyle [ text "🍴" ]

        _ ->
            Html.span iconStyle [ text "🔄" ]
