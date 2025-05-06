module Helper exposing
    ( shortenedHex, prettyAdaLovelace
    , textFieldInline, viewSelect
    , textField
    , formContainer, boxContainer
    , viewButton, viewWalletButton
    , applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle
    , viewActionTypeIcon
    , cardContainer, cardContent, cardHeader, infoBox, inputStyle, markdownRenderer, renderMarkdownContent, sectionTitle, successBox, textareaStyle, viewError, viewPendingState, warningBox
    )

{-| Helper module for miscellaneous functions that didn’t fit elsewhere,
and are potentially useful in multiple places.


# String formatting

@docs shortenedHex, prettyAdaLovelace


# Form elements

@docs textFieldInline, viewSelect

@docs textField


# Containers

@docs formContainer, boxContainer


# Buttons

@docs viewButton, viewWalletButton


# Wallet Styling

@docs applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle


# Governance Icons

@docs viewActionTypeIcon

-}

import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Markdown.Block
import Markdown.Parser as Md
import Markdown.Renderer exposing (defaultHtmlRenderer)
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



-- SECTION STYLING #############################################################


{-| Standard section title with consistent styling
-}
sectionTitle : String -> Html msg
sectionTitle title =
    Html.h2
        [ HA.style "font-weight" "600"
        , HA.style "font-size" "1.875rem"
        , HA.style "color" "#1A202C"
        , HA.style "margin-top" "1rem"
        , HA.style "margin-bottom" "1rem"
        ]
        [ text title ]



-- CARD STYLING ################################################################


{-| Container for card-style UI elements with standard styling
-}
cardContainer : List (Html.Attribute msg) -> List (Html msg) -> Html msg
cardContainer attributes content =
    div
        ([ HA.style "border" "1px solid #E2E8F0"
         , HA.style "border-radius" "0.75rem"
         , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
         , HA.style "background-color" "#FFFFFF"
         , HA.style "overflow" "hidden"
         , HA.style "margin-bottom" "1.5rem"
         ]
            ++ attributes
        )
        content


{-| Standard header section for cards with title styling
-}
cardHeader : String -> List (Html msg) -> Html msg
cardHeader title extraContent =
    div
        [ HA.style "background-color" "#F7FAFC"
        , HA.style "padding" "1rem 1.25rem"
        , HA.style "border-bottom" "1px solid #EDF2F7"
        ]
        ([ Html.h3
            [ HA.style "font-weight" "600"
            , HA.style "font-size" "1.125rem"
            , HA.style "color" "#1A202C"
            , HA.style "line-height" "1.4"
            ]
            [ text title ]
         ]
            ++ extraContent
        )


{-| Standard content section for cards with consistent padding
-}
cardContent : List (Html.Attribute msg) -> List (Html msg) -> Html msg
cardContent attributes content =
    div
        ([ HA.style "padding" "1.25rem" ] ++ attributes)
        content



-- INFORMATION BOXES ###########################################################


{-| Standard information box for displaying notices
-}
infoBox : List (Html.Attribute msg) -> List (Html msg) -> Html msg
infoBox attributes content =
    div
        ([ HA.style "background-color" "#F9FAFB"
         , HA.style "border" "1px solid #E2E8F0"
         , HA.style "border-radius" "0.5rem"
         , HA.style "padding" "1rem"
         , HA.style "margin-bottom" "1rem"
         ]
            ++ attributes
        )
        content


{-| Success message box with green styling
-}
successBox : List (Html.Attribute msg) -> List (Html msg) -> Html msg
successBox attributes content =
    div
        ([ HA.style "background-color" "#F0FDF4"
         , HA.style "border" "1px solid #D1FAE5"
         , HA.style "border-radius" "0.5rem"
         , HA.style "padding" "1rem"
         , HA.style "margin-bottom" "1rem"
         , HA.style "color" "#065F46"
         ]
            ++ attributes
        )
        content


{-| Warning message box with amber styling
-}
warningBox : List (Html.Attribute msg) -> List (Html msg) -> Html msg
warningBox attributes content =
    div
        ([ HA.style "background-color" "#FFFBEB"
         , HA.style "border" "1px solid #FEF3C7"
         , HA.style "border-radius" "0.5rem"
         , HA.style "padding" "1rem"
         , HA.style "margin-bottom" "1rem"
         , HA.style "color" "#92400E"
         ]
            ++ attributes
        )
        content



-- FORM INPUT STYLING #########################################################


{-| Standard input styling for text inputs
-}
inputStyle : List (Html.Attribute msg)
inputStyle =
    [ HA.style "width" "100%"
    , HA.style "padding" "0.75rem"
    , HA.style "border" "1px solid #E2E8F0"
    , HA.style "border-radius" "0.375rem"
    , HA.style "font-size" "0.875rem"
    , HA.style "background-color" "white"
    ]


{-| Standard textarea styling
-}
textareaStyle : List (Html.Attribute msg)
textareaStyle =
    [ HA.style "width" "100%"
    , HA.style "padding" "0.75rem"
    , HA.style "border" "1px solid #E2E8F0"
    , HA.style "border-radius" "0.375rem"
    , HA.style "min-height" "120px"
    , HA.style "resize" "vertical"
    , HA.style "font-family" "inherit"
    , HA.style "font-size" "0.875rem"
    ]



-- UI COMPONENTS ##############################################################


{-| Generic error display component
-}
viewError : Maybe String -> Html msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just err ->
            div
                [ HA.style "background-color" "#FEF2F2"
                , HA.style "border" "1px solid #FEE2E2"
                , HA.style "border-radius" "0.5rem"
                , HA.style "padding" "1rem"
                , HA.style "margin-top" "1rem"
                ]
                [ Html.div
                    [ HA.style "display" "flex"
                    , HA.style "align-items" "center"
                    , HA.style "margin-bottom" "0.5rem"
                    ]
                    [ Html.span
                        [ HA.style "color" "#DC2626"
                        , HA.style "font-weight" "bold"
                        , HA.style "margin-right" "0.5rem"
                        , HA.style "font-size" "1.25rem"
                        ]
                        [ text "!" ]
                    , Html.p
                        [ HA.style "color" "#DC2626"
                        , HA.style "font-weight" "600"
                        , HA.style "margin" "0"
                        ]
                        [ text "Error" ]
                    ]
                , Html.pre
                    [ HA.style "background-color" "#FFFFFF"
                    , HA.style "border" "1px solid #FEE2E2"
                    , HA.style "border-radius" "0.25rem"
                    , HA.style "padding" "0.75rem"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "white-space" "pre-wrap"
                    , HA.style "overflow-x" "auto"
                    , HA.style "font-family" "monospace"
                    , HA.style "color" "#991B1B"
                    , HA.style "margin" "0"
                    ]
                    [ text err ]
                ]


{-| Loading/pending state indicator
-}
viewPendingState : String -> Html msg
viewPendingState loadingText =
    div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "padding" "2rem"
        ]
        [ div
            [ HA.style "width" "3rem"
            , HA.style "height" "3rem"
            , HA.style "border" "3px solid #E2E8F0"
            , HA.style "border-top" "3px solid #3B82F6"
            , HA.style "border-radius" "50%"
            , HA.style "animation" "spin 1s linear infinite"
            , HA.style "margin-bottom" "1rem"
            ]
            []
        , Html.p
            [ HA.style "color" "#4A5568"
            ]
            [ text loadingText ]
        , Html.node "style"
            []
            [ text """
                @keyframes spin {
                    0% { transform: rotate(0deg); }
                    100% { transform: rotate(360deg); }
                }
            """
            ]
        ]



-- MARKDOWN RENDERING ##########################################################


{-| Standard markdown renderer with consistent styling
-}
markdownRenderer : Markdown.Renderer.Renderer (Html msg)
markdownRenderer =
    { defaultHtmlRenderer
        | heading = customHeadingRenderer
        , link =
            \link content ->
                Html.a
                    [ HA.href link.destination
                    , HA.target "_blank"
                    , HA.rel "noopener noreferrer"
                    , HA.style "color" "#3182CE"
                    , HA.style "text-decoration" "underline"
                    ]
                    content
        , paragraph =
            \children ->
                Html.p
                    [ HA.style "margin-bottom" "1rem"
                    , HA.style "line-height" "1.6"
                    ]
                    children
        , blockQuote =
            \children ->
                Html.blockquote
                    [ HA.style "border-left" "4px solid #CBD5E0"
                    , HA.style "padding-left" "1rem"
                    , HA.style "margin-left" "0"
                    , HA.style "margin-right" "0"
                    , HA.style "color" "#4A5568"
                    ]
                    children
        , codeBlock =
            \block ->
                Html.pre
                    [ HA.style "background-color" "#F7FAFC"
                    , HA.style "padding" "1rem"
                    , HA.style "border-radius" "0.375rem"
                    , HA.style "overflow-x" "auto"
                    , HA.style "font-family" "monospace"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "margin-bottom" "1rem"
                    ]
                    [ Html.code [] [ text block.body ] ]
    }


{-| Helper to render markdown content from a string
-}
renderMarkdownContent : String -> Html msg
renderMarkdownContent content =
    case Md.parse content of
        Err deadEnds ->
            let
                deadEndsString =
                    List.map Md.deadEndToString deadEnds
                        |> String.join "\n"
            in
            Html.div []
                [ Html.p [ HA.style "color" "#EF4444" ] [ text "Error parsing markdown:" ]
                , Html.pre
                    [ HA.style "overflow-x" "auto"
                    , HA.style "font-size" "0.75rem"
                    , HA.style "padding" "0.5rem"
                    , HA.style "background" "#F1F5F9"
                    , HA.style "border-radius" "0.25rem"
                    ]
                    [ text deadEndsString ]
                ]

        Ok blocks ->
            case Markdown.Renderer.render markdownRenderer blocks of
                Err errors ->
                    Html.p [ HA.style "color" "#EF4444" ] [ text errors ]

                Ok rendered ->
                    Html.div
                        [ HA.style "line-height" "1.6"
                        , HA.style "color" "#374151"
                        ]
                        rendered


customHeadingRenderer : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Html msg) } -> Html msg
customHeadingRenderer { level, children } =
    case level of
        Markdown.Block.H1 ->
            Html.h1
                [ HA.style "font-size" "2rem"
                , HA.style "font-weight" "700"
                , HA.style "margin-top" "1.5rem"
                , HA.style "margin-bottom" "1rem"
                ]
                children

        Markdown.Block.H2 ->
            Html.h2
                [ HA.style "font-size" "1.5rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "1.5rem"
                , HA.style "margin-bottom" "1rem"
                ]
                children

        Markdown.Block.H3 ->
            Html.h3
                [ HA.style "font-size" "1.25rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "1rem"
                , HA.style "margin-bottom" "0.75rem"
                ]
                children

        Markdown.Block.H4 ->
            Html.h4
                [ HA.style "font-size" "1.125rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "1rem"
                , HA.style "margin-bottom" "0.75rem"
                ]
                children

        Markdown.Block.H5 ->
            Html.h5
                [ HA.style "font-size" "1rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "0.75rem"
                , HA.style "margin-bottom" "0.5rem"
                ]
                children

        Markdown.Block.H6 ->
            Html.h6
                [ HA.style "font-size" "0.875rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "0.75rem"
                , HA.style "margin-bottom" "0.5rem"
                ]
                children
