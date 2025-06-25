module Helper exposing
    ( shortenedHex, prettyAdaLovelace
    , textFieldInline
    , formContainer, boxContainer, viewGrid, cardContainer, cardHeader, cardContent
    , viewButton, viewWalletButton, externalLinkButton
    , applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle
    , viewActionTypeIcon
    , sectionTitle, infoBox, viewError
    , renderMarkdownContent
    , viewStepWithCircle, viewPageHeader
    , PreconfVoter, viewVoterGrid, viewVoterCard, voterCustomCard, votingPowerDisplay, scriptInfoContainer, viewVoterCredDetails, viewVoterDetailsItem, viewCredInfo
    , viewUtxoRefForm, scriptSignerSection, scriptSignerCheckbox, viewIdentifiedVoterCard, viewVoterInfoItem
    , proposalListContainer, showMoreButton, proposalCard, selectedProposalCard, proposalDetailsItem
    , storageConfigCard, storageMethodOption, storageProviderForm, storageProviderCard, storageConfigItem, addHeaderButton, storageHeaderInput
    , storageHeaderForm, storageInfoGrid, storageNotAvailableCard, storageUploadCard, uploadingSpinner, storageSuccessCard, fileInfoItem, externalLinkDisplay
    , rationaleCard, rationaleMarkdownInput, rationaleTextArea, pdfAutogenCheckbox, voteNumberInput, referenceCard, referenceForm
    , rationaleCompletedCard, optionalSection, formattedInternalVote, formattedReferences
    , stepNotAvailableCard, downloadJSONButton, authorsCard, addAuthorButton, codeSnippetBox, noAuthorsPlaceholder
    , signerCard, authorForm, labeledField, readOnlyField, signatureField, formButtonsRow, secondaryButton, primaryButton, loadSignatureButton
    , stepCard, txResultCard, voteButton, txDetailsContainer, txPreContainer, missingStepsList, missingStepItem, loadingSpinner
    , signingStepCard, keyListItem, signingButton
    )

{-| Helper module for miscellaneous functions that didn't fit elsewhere,
and are potentially useful in multiple places.


# String formatting

@docs shortenedHex, prettyAdaLovelace


# Form elements

@docs textFieldInline


# Containers

@docs formContainer, boxContainer, viewGrid, cardContainer, cardHeader, cardContent


# Buttons

@docs viewButton, viewWalletButton, externalLinkButton


# Wallet Styling

@docs applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle


# Governance Icons

@docs viewActionTypeIcon


# UI Structure Components

@docs sectionTitle, infoBox, viewError


# Markdown Processing

@docs renderMarkdownContent


# Page Structure Components

@docs viewStepWithCircle, viewPageHeader


# Voter Identification Components

@docs PreconfVoter, viewVoterGrid, viewVoterCard, voterCustomCard, votingPowerDisplay, scriptInfoContainer, viewVoterCredDetails, viewVoterDetailsItem, viewCredInfo
@docs viewUtxoRefForm, scriptSignerSection, scriptSignerCheckbox, viewIdentifiedVoterCard, viewVoterInfoItem


# Proposal Selection Components

@docs proposalListContainer, showMoreButton, proposalCard, selectedProposalCard, proposalDetailsItem


# Storage Configuration Components

@docs storageConfigCard, storageMethodOption, storageProviderForm, storageProviderCard, storageConfigItem, addHeaderButton, storageHeaderInput
@docs storageHeaderForm, storageInfoGrid, storageNotAvailableCard, storageUploadCard, uploadingSpinner, storageSuccessCard, fileInfoItem, externalLinkDisplay


# Rationale Components

@docs rationaleCard, rationaleMarkdownInput, rationaleTextArea, pdfAutogenCheckbox, voteNumberInput, referenceCard, referenceForm
@docs rationaleCompletedCard, optionalSection, formattedInternalVote, formattedReferences


# Document Creation Components

@docs stepNotAvailableCard, downloadJSONButton, authorsCard, addAuthorButton, codeSnippetBox, noAuthorsPlaceholder
@docs signerCard, authorForm, labeledField, readOnlyField, signatureField, formButtonsRow, secondaryButton, primaryButton, loadSignatureButton


# Transaction Components

@docs stepCard, txResultCard, voteButton, txDetailsContainer, txPreContainer, missingStepsList, missingStepItem, loadingSpinner


# Signing Components

@docs signingStepCard, keyListItem, signingButton

-}

import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onCheck, onClick)
import Markdown.Block
import Markdown.Parser as Md
import Markdown.Renderer exposing (defaultHtmlRenderer)
import Natural exposing (Natural)
import Numeral
import RemoteData
import Url



-- STRING FORMATTING ###########################################################


{-| Shorten some string, by only keeping the first and last few characters.
-}
shortenedHex : Int -> String -> String
shortenedHex visibleChars str =
    let
        strLength =
            String.length str
    in
    if strLength <= visibleChars * 2 then
        str

    else
        String.slice 0 visibleChars str
            ++ "..."
            ++ String.slice (strLength - visibleChars) strLength str


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


squareButton : List (Html.Attribute msg) -> String -> msg -> Html msg
squareButton otherAttributes label msg =
    Html.button
        (HA.style "font-weight" "500"
            :: HA.style "border" "1px solid #e2e8f0"
            :: HA.style "border-radius" "0.5rem"
            :: HA.style "padding" "0.75rem 1.5rem"
            :: HA.style "cursor" "pointer"
            :: HA.style "font-size" "0.875rem"
            :: onClick msg
            :: otherAttributes
        )
        [ text label ]


blackSquareButton : List (Html.Attribute msg) -> String -> msg -> Html msg
blackSquareButton otherAttributes label msg =
    squareButton
        (HA.style "background-color" "#272727"
            :: HA.style "color" "white"
            :: otherAttributes
        )
        label
        msg


viewButton : String -> msg -> Html msg
viewButton label msg =
    Html.button
        (onClick msg
            :: HA.style "margin-top" "0.5rem"
            :: HA.style "margin-bottom" "0.5em"
            :: buttonCommonStyle
        )
        [ text label ]


viewWalletButton : String -> msg -> List (Html msg) -> Html msg
viewWalletButton label msg content =
    Html.button
        (onClick msg :: buttonCommonStyle)
        (text label :: content)


externalLinkButton : { url : String, label : String } -> Html msg
externalLinkButton { url, label } =
    Html.a
        [ HA.href url
        , HA.target "_blank"
        , HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "white-space" "nowrap"
        , HA.style "border-radius" "9999px"
        , HA.style "font-size" "0.875rem"
        , HA.style "font-weight" "500"
        , HA.style "transition" "all 0.2s"
        , HA.style "outline" "none"
        , HA.style "background-color" "#272727"
        , HA.style "color" "#f7fafc"
        , HA.style "padding" "0.75rem 1.5rem"
        ]
        [ text <| label ++ " ↗" ]


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
        [ HA.style "font-weight" "500"
        , HA.style "font-size" "1.875rem"
        , HA.style "color" "#1A202C"
        , HA.style "margin-top" "0.7rem"
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
cardHeader : List (Html.Attribute msg) -> String -> String -> List (Html msg) -> Html msg
cardHeader attributes title subtitle extraContent =
    div
        ([ HA.style "background-color" "#F7FAFC"
         , HA.style "padding" "1rem 1.25rem"
         , HA.style "border-bottom" "1px solid #EDF2F7"
         ]
            ++ attributes
        )
        (div []
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1.125rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                ]
                [ text title ]
            , Html.p [ HA.style "font-size" "0.875rem" ] [ text subtitle ]
            ]
            :: extraContent
        )


{-| Standard content section for cards with consistent padding
-}
cardContent : List (Html.Attribute msg) -> List (Html msg) -> Html msg
cardContent attributes content =
    div
        (HA.style "padding" "1.25rem" :: attributes)
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



-- FORM INPUT STYLING #########################################################
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



-- Stepper with circle and header #########################################################


{-| Renders a step with a numbered circle on the left side
-}
viewStepWithCircle : Int -> String -> Html msg -> Html msg
viewStepWithCircle stepNumber stepId content =
    div
        [ HA.id stepId
        , HA.style "position" "relative"
        , HA.style "padding-left" "5rem"
        , HA.style "padding-top" "2rem"
        , HA.style "padding-bottom" "2rem"
        ]
        [ -- Circle with step number
          div
            [ HA.style "position" "absolute"
            , HA.style "left" "-0.10rem"
            , HA.style "top" "2.9rem"
            , HA.style "width" "2.5rem"
            , HA.style "height" "2.5rem"
            , HA.style "border-radius" "50%"
            , HA.style "background-color" "#272727"
            , HA.style "color" "white"
            , HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "justify-content" "center"
            , HA.style "font-weight" "bold"
            , HA.style "font-size" "1.125rem"
            , HA.style "z-index" "3"
            , HA.style "box-shadow" "0 0 0 4px white"
            ]
            [ text (String.fromInt stepNumber) ]
        , content
        ]


{-| Renders the page header with title and description
-}
viewPageHeader : Html msg
viewPageHeader =
    div
        [ HA.style "position" "relative"
        , HA.style "overflow" "hidden"
        , HA.style "padding-top" "6rem"
        , HA.style "padding-bottom" "6rem"
        , HA.style "margin-bottom" "2rem"
        ]
        [ div
            [ HA.style "position" "relative"
            , HA.style "z-index" "10"
            , HA.style "max-width" "840px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1.5rem"
            ]
            [ Html.h1
                [ HA.style "font-size" "3.5rem"
                , HA.style "font-weight" "600"
                , HA.style "line-height" "1.1"
                , HA.style "margin-bottom" "1.5rem"
                ]
                [ text "Vote Preparation" ]
            , Html.p
                [ HA.style "font-size" "1.25rem"
                , HA.style "line-height" "1.6"
                , HA.style "max-width" "640px"
                , HA.style "margin-bottom" "2rem"
                ]
                [ text "This page helps you prepare and submit votes for governance proposals. You can identify yourself as a voter, select a proposal, create a rationale for your vote, and build the transaction."
                ]
            ]
        , viewHeaderBackground
        ]


{-| Renders the gradient background for the header
-}
viewHeaderBackground : Html msg
viewHeaderBackground =
    div
        [ HA.style "position" "absolute"
        , HA.style "z-index" "1"
        , HA.style "top" "-13rem"
        , HA.style "right" "0"
        , HA.style "left" "0"
        , HA.style "overflow" "hidden"
        , HA.style "transform" "translateZ(0)"
        , HA.style "filter" "blur(64px)"
        ]
        [ div
            [ HA.style "position" "relative"
            , HA.style "width" "100%"
            , HA.style "padding-bottom" "58.7%"
            , HA.style "background" "linear-gradient(90deg, #00E0FF, #0084FF)"
            , HA.style "opacity" "0.8"
            , HA.style "clip-path" "polygon(19% 5%, 36% 8%, 55% 15%, 76% 5%, 100% 16%, 100% 100%, 0 100%, 0 14%)"
            ]
            []
        ]



-- VOTER IDENTIFICATION STEP STYLING #########################################################


{-| Grid layout for voter role cards
-}
viewVoterGrid : List (Html msg) -> Html msg
viewVoterGrid cards =
    if List.isEmpty cards then
        text ""

    else
        viewGrid 220 cards


viewGrid : Int -> List (Html msg) -> Html msg
viewGrid minmaxWidth elems =
    div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" ("repeat(auto-fill, minmax(" ++ String.fromInt minmaxWidth ++ "px, 1fr))")
        , HA.style "gap" "1.5rem"
        , HA.style "margin-bottom" "1.5rem"
        ]
        elems


type alias PreconfVoter =
    { voterType : String, description : String, govId : String }


{-| Card for displaying a voter role option
-}
viewVoterCard : (String -> msg) -> String -> PreconfVoter -> Html msg
viewVoterCard selectMsg currentSelection { voterType, description, govId } =
    let
        isSelected =
            currentSelection == govId
    in
    div
        [ HA.style "border"
            (if isSelected then
                "2px solid #272727"

             else
                "1px solid #E2E8F0"
            )
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100%"
        , HA.style "transition" "all 0.3s ease"
        , HA.style "transform-origin" "center"
        , HA.style "position" "relative"
        , HA.style "overflow" "hidden"
        , HA.style "cursor" "pointer"
        , onClick (selectMsg govId)
        ]
        [ div
            [ HA.style "background-color"
                (if isSelected then
                    "#F1F5F9"

                 else
                    "#F7FAFC"
                )
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            , HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                ]
                [ text voterType ]
            ]
        , div
            [ HA.style "padding" "1.25rem"
            , HA.style "flex-grow" "1"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            ]
            [ Html.p
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "line-height" "1.6"
                , HA.style "margin-bottom" "1rem"
                ]
                [ text description ]
            , div
                [ HA.style "font-size" "0.75rem"
                , HA.style "color" "#718096"
                , HA.style "margin-top" "auto"
                ]
                [ div
                    [ HA.style "overflow" "hidden"
                    , HA.style "text-overflow" "ellipsis"
                    , HA.style "white-space" "nowrap"
                    , HA.style "font-family" "monospace"
                    ]
                    [ text (String.left 8 govId ++ "..." ++ String.right 8 govId) ]
                ]
            ]
        , if isSelected then
            div
                [ HA.style "position" "absolute"
                , HA.style "top" "0.5rem"
                , HA.style "right" "0.5rem"
                , HA.style "width" "1.5rem"
                , HA.style "height" "1.5rem"
                , HA.style "border-radius" "9999px"
                , HA.style "background-color" "#272727"
                , HA.style "color" "white"
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                ]
                [ text "✓" ]

          else
            text ""
        ]


{-| Card for custom voter ID input
-}
voterCustomCard : { currentValue : String, onInputMsg : String -> msg } -> Html msg
voterCustomCard { currentValue, onInputMsg } =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100%"
        , HA.style "transition" "all 0.3s ease"
        ]
        [ div
            [ HA.style "background-color" "#F7FAFC"
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                ]
                [ text "Custom" ]
            ]
        , div
            [ HA.style "padding" "1.25rem"
            , HA.style "flex-grow" "1"
            ]
            [ Html.p
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "line-height" "1.6"
                , HA.style "margin-bottom" "1rem"
                ]
                [ text "Enter your own governance ID" ]
            , Html.div
                [ HA.style "position" "relative"
                , HA.style "width" "100%"
                ]
                [ Html.input
                    [ HA.type_ "text"
                    , HA.value currentValue
                    , HA.placeholder "Paste drep/pool/cc_hot ID"
                    , Html.Events.onInput onInputMsg
                    , HA.style "width" "100%"
                    , HA.style "padding" "0.5rem"
                    , HA.style "border" "1px solid #CBD5E0"
                    , HA.style "border-radius" "0.375rem"
                    , HA.style "font-family" "monospace"
                    , HA.style "font-size" "0.75rem"
                    , HA.style "box-sizing" "border-box"
                    ]
                    []
                ]
            ]
        ]


{-| Display voting power information for a voter
-}
votingPowerDisplay : (a -> Int) -> RemoteData.WebData a -> Html msg
votingPowerDisplay accessor webData =
    case webData of
        RemoteData.NotAsked ->
            text "not querried"

        RemoteData.Loading ->
            text "loading ..."

        RemoteData.Failure _ ->
            text "Is this the correct network? If yes, then most likely, this voter is inactive, not registered yet, or was just registered this epoch."

        RemoteData.Success success ->
            text <| prettyAdaLovelace <| Natural.fromSafeInt <| accessor success


{-| Container for script information
-}
scriptInfoContainer : List (Html msg) -> Html msg
scriptInfoContainer content =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "padding" "1.25rem"
        , HA.style "margin-top" "1.5rem"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "gap" "0.75rem"
            ]
            content
        ]


{-| Display details about a credential (key or script)
-}
viewVoterCredDetails : String -> String -> Html msg
viewVoterCredDetails label hashValue =
    div [ HA.style "display" "flex", HA.style "align-items" "center" ]
        [ Html.span
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "margin-right" "0.5rem"
            , HA.style "min-width" "12rem"
            ]
            [ text label ]
        , Html.span
            [ HA.style "font-family" "monospace"
            , HA.style "background-color" "#EDF2F7"
            , HA.style "padding" "0.25rem 0.5rem"
            , HA.style "border-radius" "0.25rem"
            ]
            [ text hashValue ]
        ]


{-| Display details for a voter item
-}
viewVoterDetailsItem : String -> Html msg -> Html msg
viewVoterDetailsItem label content =
    div [ HA.style "display" "flex", HA.style "align-items" "center" ]
        [ Html.span
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "margin-right" "0.5rem"
            , HA.style "min-width" "12rem"
            ]
            [ text label ]
        , content
        ]


{-| Container for constitutional committee information
-}
viewCredInfo : List (Html msg) -> Html msg
viewCredInfo content =
    div [] content


{-| Form for a UTxO reference field
-}
viewUtxoRefForm : String -> (String -> msg) -> Html msg
viewUtxoRefForm utxoRef onInputMsg =
    Html.p [] [ textField "Reference UTxO" utxoRef onInputMsg ]


{-| Section for script signers with fee info
-}
scriptSignerSection : Int -> Int -> List (Html msg) -> Html msg
scriptSignerSection additionalBytesPerSig feePerByte content =
    let
        additionalSignerCost =
            Natural.fromSafeInt <| feePerByte * additionalBytesPerSig
    in
    div []
        [ Html.p []
            [ text <| "Expected signers: (each adds " ++ prettyAdaLovelace additionalSignerCost ++ " to the Tx fees)" ]
        , div [] content
        ]


{-| Checkbox for script signers
-}
scriptSignerCheckbox : String -> Bool -> (Bool -> msg) -> Html msg
scriptSignerCheckbox keyHex isChecked onCheckMsg =
    Html.p []
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.id keyHex
            , HA.name keyHex
            , HA.checked isChecked
            , onCheck onCheckMsg
            ]
            []
        , Html.label [ HA.for keyHex ] [ text <| " key hash: " ++ keyHex ]
        ]


{-| Container for voter identification card
-}
viewVoterIdentificationCard : String -> List (Html msg) -> Html msg
viewVoterIdentificationCard title content =
    cardContainer []
        [ cardHeader [] title "" []
        , cardContent [] content
        ]


{-| Display identified voter card with information
-}
viewIdentifiedVoterCard : String -> List (Html msg) -> Html msg -> Html msg
viewIdentifiedVoterCard title content changeButton =
    div []
        [ sectionTitle "Voter Information"
        , viewVoterIdentificationCard title content
        , Html.p
            [ HA.style "margin-top" "1rem" ]
            [ changeButton ]
        ]


{-| Display voter info item
-}
viewVoterInfoItem : String -> String -> Html msg
viewVoterInfoItem label value =
    Html.p [] [ text <| label ++ ": " ++ value ]



-- PROPOSAL SELECTION STEP STYLING #########################################################


{-| Container for the proposal list with styling
-}
proposalListContainer : String -> Int -> List (Html msg) -> Html msg
proposalListContainer title totalCount content =
    div []
        [ proposalListHeader title totalCount
        , viewGrid 250 content
        ]


{-| Header for the proposal list section
-}
proposalListHeader : String -> Int -> Html msg
proposalListHeader title count =
    Html.p
        [ HA.style "margin-bottom" "1rem" ]
        [ text <| title ++ " (" ++ String.fromInt count ++ " available):" ]


{-| Show more button for paginated proposals
-}
showMoreButton : Bool -> Int -> Int -> msg -> Html msg
showMoreButton hasMore visibleCount totalCount clickMsg =
    if hasMore then
        div
            [ HA.style "text-align" "center"
            , HA.style "margin-top" "2rem"
            ]
            [ squareButton
                [ HA.style "background-color" "#f9fafb"
                , HA.style "color" "#272727"
                ]
                ("Show More (" ++ String.fromInt (min 10 (totalCount - visibleCount)) ++ " of " ++ String.fromInt (totalCount - visibleCount) ++ " remaining)")
                clickMsg
            ]

    else
        text ""


{-| Card for an individual proposal
-}
proposalCard : { id : String, hashIsValid : Bool, title : String, abstract : String, actionType : String, actionId : String, linkUrl : String, linkHex : String, index : Int } -> msg -> Html msg -> Html msg
proposalCard { title, hashIsValid, abstract, actionType, linkUrl, linkHex, index } selectMsg actionIcon =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100%"
        , HA.style "transition" "all 0.3s ease"
        , HA.style "transform-origin" "center"
        , HA.style "position" "relative"
        , HA.style "overflow" "hidden"
        ]
        [ div
            [ HA.style "background-color" "#F7FAFC"
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            , HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                , HA.style "word-wrap" "break-word"
                , HA.style "flex" "1"
                ]
                (if hashIsValid then
                    [ text title ]

                 else
                    [ text title
                    , Html.span
                        [ HA.style "display" "inline-flex"
                        , HA.style "align-items" "center"
                        , HA.style "background-color" "#FEF2F2"
                        , HA.style "color" "#DC2626"
                        , HA.style "font-weight" "bold"
                        , HA.style "padding" "0.25rem 0.5rem"
                        , HA.style "border-radius" "0.375rem"
                        , HA.style "font-size" "0.875rem"
                        , HA.style "gap" "0.25rem"
                        ]
                        [ Html.span [] [ text "⚠️" ]
                        , text "INVALID HASH"
                        ]
                    ]
                )
            ]
        , div
            [ HA.style "padding" "1.25rem"
            , HA.style "flex-grow" "1"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            ]
            [ Html.p
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "line-height" "1.6"
                , HA.style "display" "-webkit-box"
                , HA.style "-webkit-line-clamp" "4"
                , HA.style "-webkit-box-orient" "vertical"
                , HA.style "overflow" "hidden"
                , HA.style "margin-bottom" "1.5rem"
                ]
                [ renderMarkdownContent abstract ]
            , div
                [ HA.style "font-size" "0.75rem"
                , HA.style "color" "#718096"
                , HA.style "margin-top" "auto"
                , HA.style "padding-top" "1rem"
                , HA.style "border-top" "1px solid #EDF2F7"
                ]
                [ Html.div
                    [ HA.style "display" "flex"
                    , HA.style "flex-wrap" "wrap"
                    , HA.style "justify-content" "space-between"
                    , HA.style "align-items" "center"
                    , HA.style "margin-bottom" "0.3rem"
                    ]
                    [ Html.div [ HA.style "display" "flex", HA.style "align-items" "center" ]
                        [ Html.span [ HA.style "font-weight" "500", HA.style "color" "#4A5568", HA.style "margin-right" "0.5rem" ] [ text "ID:" ]
                        , Html.a
                            [ HA.href linkUrl
                            , HA.target "_blank"
                            , HA.rel "noopener noreferrer"
                            , HA.style "display" "inline-flex"
                            , HA.style "align-items" "center"
                            , HA.style "color" "#3182CE"
                            , HA.style "text-decoration" "underline"
                            , HA.style "cursor" "pointer"
                            , HA.title "View on Cardanoscan (opens in new tab)"
                            ]
                            [ text linkHex
                            , text <| "#" ++ String.fromInt index
                            , Html.span
                                [ HA.style "margin-left" "0.25rem"
                                , HA.style "font-size" "0.8rem"
                                ]
                                [ text "↗" ]
                            ]
                        ]
                    , div
                        [ HA.style "font-size" "0.75rem"
                        , HA.style "font-weight" "500"
                        , HA.style "color" "#4A5568"
                        , HA.style "background-color" "#EDF2F7"
                        , HA.style "padding" "0.25rem 0.5rem"
                        , HA.style "border-radius" "9999px"
                        , HA.style "margin-left" "0.5rem"
                        ]
                        [ text actionType
                        , actionIcon
                        ]
                    ]
                ]
            , blackSquareButton
                [ HA.style "width" "100%"
                , HA.style "margin-top" "1rem"
                ]
                "Select Proposal"
                selectMsg
            ]
        ]


{-| Card showing selected proposal details
-}
selectedProposalCard : List (Html msg) -> Html msg
selectedProposalCard content =
    cardContainer []
        [ cardHeader [] "Selected Proposal" "" []
        , cardContent
            [ HA.style "padding" "1.25rem"
            , HA.style "display" "grid"
            , HA.style "gap" "0.75rem"
            ]
            content
        ]


{-| Style for individual details in selected proposal
-}
proposalDetailsItem : String -> Html msg -> Html msg
proposalDetailsItem label content =
    div
        [ HA.style "display" "flex"
        , HA.style "align-items" "flex-start"
        ]
        [ Html.span
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "margin-right" "0.5rem"
            , HA.style "min-width" "6rem"
            ]
            [ text (label ++ ":") ]
        , content
        ]



-- STORAGE CONFIGURATION STEP STYLING #########################################################


{-| Main container for storage configuration section
-}
storageConfigCard : String -> List (Html msg) -> Html msg
storageConfigCard title bodyContent =
    cardContainer []
        [ cardHeader [] title "" []
        , cardContent [] bodyContent
        ]


{-| Option card for a storage method
-}
storageMethodOption : String -> Bool -> msg -> Html msg
storageMethodOption label isSelected selectMsg =
    div
        [ HA.style "border"
            (if isSelected then
                "2px solid #272727"

             else
                "1px solid #E2E8F0"
            )
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "0.75rem"
        , HA.style "cursor" "pointer"
        , HA.style "background-color"
            (if isSelected then
                "#F1F5F9"

             else
                "#FFFFFF"
            )
        , HA.style "position" "relative"
        , onClick selectMsg
        ]
        [ div
            [ HA.style "font-weight" "500"
            , HA.style "font-size" "0.9375rem"
            ]
            [ text label ]
        , if isSelected then
            div
                [ HA.style "position" "absolute"
                , HA.style "top" "0.5rem"
                , HA.style "right" "0.5rem"
                , HA.style "width" "1rem"
                , HA.style "height" "1rem"
                , HA.style "border-radius" "9999px"
                , HA.style "background-color" "#272727"
                , HA.style "color" "white"
                , HA.style "display" "flex"
                , HA.style "align-items" "center"
                , HA.style "justify-content" "center"
                , HA.style "font-size" "0.75rem"
                ]
                [ text "✓" ]

          else
            text ""
        ]


{-| Container for provider form
-}
storageProviderForm : List (Html msg) -> Html msg
storageProviderForm content =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "1rem"
        , HA.style "margin-top" "1rem"
        ]
        content


{-| Card for displaying selected storage configuration
-}
storageProviderCard : List (Html msg) -> Html msg
storageProviderCard content =
    div [] content


{-| Grid for displaying storage configuration details
-}
storageConfigItem : String -> Html msg -> Html msg
storageConfigItem label content =
    Html.div
        [ HA.style "margin-top" "0.75rem"
        , HA.style "padding" "0.75rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "border-radius" "0.375rem"
        , HA.style "border" "1px solid #EDF2F7"
        ]
        [ Html.p
            [ HA.style "font-weight" "500"
            , HA.style "margin-bottom" "0.25rem"
            ]
            [ text (label ++ ": ") ]
        , content
        ]


{-| Helper button to add an HTTP header in custom IPFS configs.
-}
addHeaderButton : msg -> Html msg
addHeaderButton addHeaderMsg =
    blackSquareButton [] "+ Add Header" addHeaderMsg


{-| Input for HTTP header
-}
storageHeaderInput : { label : String, value : String, onInputMsg : String -> msg } -> Html msg
storageHeaderInput { label, value, onInputMsg } =
    div []
        [ Html.label
            [ HA.style "display" "block"
            , HA.style "font-size" "0.875rem"
            , HA.style "font-weight" "500"
            , HA.style "margin-bottom" "0.25rem"
            ]
            [ text label ]
        , Html.input
            [ HA.type_ "text"
            , HA.value value
            , Html.Events.onInput onInputMsg
            , HA.style "width" "100%"
            , HA.style "padding" "0.5rem"
            , HA.style "border" "1px solid #E2E8F0"
            , HA.style "border-radius" "0.375rem"
            ]
            []
        ]


{-| Form for a header item with name and value inputs
-}
storageHeaderForm : Int -> String -> String -> msg -> (String -> msg) -> (String -> msg) -> Html msg
storageHeaderForm _ name value deleteMsg nameChangeMsg valueChangeMsg =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "1rem"
        , HA.style "margin-bottom" "0.75rem"
        , HA.style "background-color" "#F9FAFB"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "gap" "1rem"
            , HA.style "align-items" "flex-end"
            ]
            [ div [ HA.style "flex" "1" ]
                [ storageHeaderInput
                    { label = "Header Name"
                    , value = name
                    , onInputMsg = nameChangeMsg
                    }
                ]
            , div [ HA.style "flex" "1" ]
                [ storageHeaderInput
                    { label = "Header Value"
                    , value = value
                    , onInputMsg = valueChangeMsg
                    }
                ]
            , iconButton "🗑" deleteMsg
            ]
        ]


iconButton : String -> msg -> Html msg
iconButton icon msg =
    Html.button
        [ HA.style "background-color" "black"
        , HA.style "color" "white"
        , HA.style "font-size" "1.25rem"
        , HA.style "width" "2.2rem"
        , HA.style "height" "2.2rem"
        , HA.style "border" "none"
        , HA.style "border-radius" "0.375rem"
        , onClick msg
        ]
        [ text icon ]



-- VOTE RATIONALE STEP STYLING  #########################################################


{-| Card for rationale form items with consistent styling
-}
rationaleCard : String -> String -> Html msg -> Html msg
rationaleCard title description content =
    cardContainer []
        [ cardHeader [] title description []
        , cardContent [] [ content ]
        ]


{-| Textarea input for rationale markdown fields
-}
rationaleMarkdownInput : String -> (String -> msg) -> Html msg
rationaleMarkdownInput content msgOnInput =
    div []
        [ div
            [ HA.style "margin-bottom" "0.5rem"
            , HA.style "color" "#4A5568"
            , HA.style "font-size" "0.75rem"
            ]
            [ text "Markdown formatting is supported. Use ## or deeper heading levels." ]
        , Html.textarea
            [ HA.value content
            , Html.Events.onInput msgOnInput
            , HA.style "width" "100%"
            , HA.style "padding" "0.75rem"
            , HA.style "border" "1px solid #E2E8F0"
            , HA.style "border-radius" "0.375rem"
            , HA.style "min-height" "180px"
            , HA.style "resize" "vertical"
            , HA.style "font-family" "monospace"
            , HA.style "font-size" "0.875rem"
            , HA.style "line-height" "1.5"
            ]
            []
        ]


{-| Textarea specifically for the summary field with character count
-}
rationaleTextArea : (String -> msg) -> Maybe Int -> String -> Html msg
rationaleTextArea msgOnInput maybeCharLimit summary =
    let
        summaryLength =
            String.length summary

        ( areaDescription, limitInfo, limitColor ) =
            case maybeCharLimit of
                Nothing ->
                    ( "Plain text only. No size limit."
                    , ""
                    , "#4B5563"
                    )

                Just charLimit ->
                    ( "Plain text only. Limited to " ++ String.fromInt charLimit ++ " characters."
                    , String.fromInt summaryLength ++ "/" ++ String.fromInt charLimit ++ " characters"
                    , if summaryLength > charLimit then
                        "#EF4444"

                      else if summaryLength > (charLimit * 3 // 4) then
                        "#F59E0B"

                      else
                        "#4B5563"
                    )
    in
    div []
        [ div
            [ HA.style "margin-bottom" "0.5rem"
            , HA.style "color" "#4A5568"
            , HA.style "font-size" "0.75rem"
            ]
            [ text areaDescription ]
        , Html.textarea
            [ HA.value summary
            , Html.Events.onInput msgOnInput
            , HA.style "width" "100%"
            , HA.style "padding" "0.75rem"
            , HA.style "border" "1px solid #E2E8F0"
            , HA.style "border-radius" "0.375rem"
            , HA.style "min-height" "80px"
            , HA.style "resize" "vertical"
            , HA.style "font-family" "inherit"
            , HA.style "font-size" "0.875rem"
            ]
            []
        , Html.p
            [ HA.style "color" limitColor
            , HA.style "font-size" "0.75rem"
            , HA.style "margin-top" "0.5rem"
            , HA.style "text-align" "right"
            ]
            [ text limitInfo ]
        ]


{-| Checkbox for PDF auto-generation option
-}
pdfAutogenCheckbox : Bool -> (Bool -> msg) -> Html msg
pdfAutogenCheckbox hasAutoGen toggleMsg =
    div
        [ HA.class "flex items-center mb-3" ]
        [ div
            [ HA.class "relative flex items-center" ]
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.id "autogen-checkbox"
                , HA.name "autogen-checkbox"
                , HA.checked hasAutoGen
                , onCheck toggleMsg
                , HA.class "h-4 w-4 cursor-pointer border-gray-300 rounded"
                , HA.style "accent-color" "#272727"
                ]
                []
            , Html.label
                [ HA.for "autogen-checkbox"
                , HA.class "ml-2 text-sm font-medium text-gray-700 cursor-pointer flex items-center"
                ]
                [ Html.span [ HA.class "mr-1" ] [ text "Auto-generate PDF and add it to the rationale" ]
                ]
            ]
        ]


{-| Input for vote numbers with label
-}
voteNumberInput : String -> Int -> (String -> msg) -> Html msg
voteNumberInput label value onInputMsg =
    div []
        [ Html.label
            [ HA.style "display" "block"
            , HA.style "font-size" "0.875rem"
            , HA.style "font-weight" "500"
            , HA.style "color" "#4B5563"
            , HA.style "margin-bottom" "0.25rem"
            ]
            [ text label ]
        , Html.input
            [ HA.type_ "number"
            , HA.value (String.fromInt value)
            , HA.min "0"
            , Html.Events.onInput onInputMsg
            , HA.style "width" "100%"
            , HA.style "padding" "0.5rem"
            , HA.style "border" "1px solid #E2E8F0"
            , HA.style "border-radius" "0.375rem"
            , HA.style "font-size" "0.875rem"
            ]
            []
        ]


{-| Card for references with add button in header
-}
referenceCard : List (Html msg) -> msg -> Html msg
referenceCard content addMsg =
    cardContainer []
        [ cardHeader
            [ HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            "References"
            "Add links and references to support your rationale."
            [ blackSquareButton [] "+ Add Reference" addMsg ]
        , cardContent []
            [ if List.isEmpty content then
                Html.p
                    [ HA.style "text-align" "center"
                    , HA.style "color" "#6B7280"
                    , HA.style "font-style" "italic"
                    , HA.style "padding" "1rem 0"
                    ]
                    [ text "No references added yet." ]

              else
                div [ HA.class "space-y-4" ] content
            ]
        ]


{-| Form for a single reference
-}
referenceForm : Int -> String -> String -> String -> msg -> (String -> msg) -> (String -> msg) -> (String -> msg) -> Html msg
referenceForm index typeName label uri deleteMsg typeChangeMsg labelChangeMsg uriChangeMsg =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "1rem"
        , HA.style "background-color" "#F9FAFB"
        ]
        [ div [ HA.style "display" "flex", HA.style "justify-content" "space-between", HA.style "align-items" "center", HA.style "margin-bottom" "0.75rem" ]
            [ Html.h4
                [ HA.style "font-weight" "500"
                , HA.style "font-size" "0.9375rem"
                , HA.style "color" "#374151"
                ]
                [ text ("Reference " ++ String.fromInt (index + 1)) ]
            , iconButton "🗑" deleteMsg
            ]
        , div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "repeat(auto-fit, minmax(200px, 1fr))"
            , HA.style "gap" "1rem"
            ]
            [ div []
                [ Html.label
                    [ HA.style "display" "block"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "font-weight" "500"
                    , HA.style "color" "#4B5563"
                    , HA.style "margin-bottom" "0.25rem"
                    ]
                    [ text "Type" ]
                , viewSelect
                    [ HA.value typeName
                    , Html.Events.onInput typeChangeMsg
                    , HA.style "width" "100%"
                    , HA.style "padding" "0.5rem"
                    , HA.style "border" "1px solid #E2E8F0"
                    , HA.style "border-radius" "0.375rem"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "background-color" "white"
                    ]
                    [ Html.option [ HA.value "relevant articles" ] [ text "Relevant Articles" ]
                    , Html.option [ HA.value "governance metadata" ] [ text "Governance Metadata" ]
                    , Html.option [ HA.value "other" ] [ text "Other" ]
                    ]
                ]
            , div []
                [ Html.label
                    [ HA.style "display" "block"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "font-weight" "500"
                    , HA.style "color" "#4B5563"
                    , HA.style "margin-bottom" "0.25rem"
                    ]
                    [ text "Label" ]
                , Html.input
                    [ HA.type_ "text"
                    , HA.value label
                    , Html.Events.onInput labelChangeMsg
                    , HA.style "width" "100%"
                    , HA.style "padding" "0.5rem"
                    , HA.style "border" "1px solid #E2E8F0"
                    , HA.style "border-radius" "0.375rem"
                    , HA.style "font-size" "0.875rem"
                    ]
                    []
                ]
            , div []
                [ Html.label
                    [ HA.style "display" "block"
                    , HA.style "font-size" "0.875rem"
                    , HA.style "font-weight" "500"
                    , HA.style "color" "#4B5563"
                    , HA.style "margin-bottom" "0.25rem"
                    ]
                    [ text "URI" ]
                , Html.input
                    [ HA.type_ "text"
                    , HA.value uri
                    , Html.Events.onInput uriChangeMsg
                    , HA.style "width" "100%"
                    , HA.style "padding" "0.5rem"
                    , HA.style "border" "1px solid #E2E8F0"
                    , HA.style "border-radius" "0.375rem"
                    , HA.style "font-size" "0.875rem"
                    ]
                    []
                ]
            ]
        ]


{-| Card for completed rationale
-}
rationaleCompletedCard : String -> List (Html msg) -> msg -> Html msg
rationaleCompletedCard summary content editMsg =
    div []
        [ sectionTitle "Vote Rationale"
        , cardContainer []
            [ cardHeader [] "Rationale Summary" "" []
            , cardContent []
                [ div [ HA.style "display" "grid", HA.style "gap" "1.5rem" ]
                    (div []
                        [ Html.p
                            [ HA.style "font-size" "0.9375rem"
                            , HA.style "line-height" "1.6"
                            , HA.style "color" "#4A5568"
                            ]
                            [ text summary ]
                        ]
                        :: content
                    )
                ]
            ]
        , Html.p [ HA.style "margin-top" "1rem" ]
            [ viewButton "Edit rationale" editMsg ]
        ]


{-| Display optional section if content exists
-}
optionalSection : String -> Maybe String -> Html msg
optionalSection title maybeContent =
    case maybeContent of
        Just content ->
            div [ HA.style "border-top" "1px solid #EDF2F7", HA.style "padding-top" "1.5rem" ]
                [ Html.h4
                    [ HA.style "font-size" "1rem"
                    , HA.style "font-weight" "600"
                    , HA.style "margin-bottom" "1rem"
                    , HA.style "color" "#1A202C"
                    ]
                    [ text title ]
                , div
                    [ HA.style "border" "1px solid #E2E8F0"
                    , HA.style "border-radius" "0.5rem"
                    , HA.style "padding" "1.25rem"
                    , HA.style "background-color" "#F9FAFB"
                    ]
                    [ renderMarkdownContent content ]
                ]

        Nothing ->
            text ""


{-| Format internal votes display
-}
formattedInternalVote : { constitutional : Int, unconstitutional : Int, abstain : Int, didNotVote : Int, against : Int } -> Html msg
formattedInternalVote { constitutional, unconstitutional, abstain, didNotVote, against } =
    div
        [ HA.style "display" "grid"
        , HA.style "grid-template-columns" "repeat(auto-fill, minmax(180px, 1fr))"
        , HA.style "gap" "1rem"
        , HA.style "padding" "1rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        ]
        [ voteItem "Constitutional" constitutional
        , voteItem "Unconstitutional" unconstitutional
        , voteItem "Abstain" abstain
        , voteItem "Did not vote" didNotVote
        , voteItem "Against voting" against
        ]


{-| Single vote item with count
-}
voteItem : String -> Int -> Html msg
voteItem label count =
    div []
        [ Html.span
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "display" "block"
            ]
            [ text label ]
        , Html.span
            [ HA.style "font-size" "1.25rem"
            , HA.style "font-weight" "600"
            , HA.style "color" "#1A202C"
            ]
            [ text (String.fromInt count) ]
        ]


{-| Format references list
-}
formattedReferences : (a -> String) -> List { type_ : a, label : String, uri : String } -> Html msg
formattedReferences typeToString references =
    div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "gap" "0.75rem"
        ]
        (List.map (formattedReference typeToString) references)


{-| Format single reference
-}
formattedReference : (a -> String) -> { type_ : a, label : String, uri : String } -> Html msg
formattedReference typeToString ref =
    div
        [ HA.style "padding" "0.75rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "align-items" "baseline"
            , HA.style "gap" "0.5rem"
            , HA.style "margin-bottom" "0.25rem"
            ]
            [ Html.span
                [ HA.style "font-weight" "600"
                , HA.style "color" "#1A202C"
                ]
                [ text ref.label ]
            , Html.span
                [ HA.style "font-size" "0.75rem"
                , HA.style "color" "#718096"
                , HA.style "padding" "0.125rem 0.375rem"
                , HA.style "background-color" "#EDF2F7"
                , HA.style "border-radius" "9999px"
                ]
                [ text (typeToString ref.type_) ]
            ]
        , if String.startsWith "ipfs://" ref.uri then
            let
                cid =
                    String.dropLeft 7 ref.uri

                gatewayUrl =
                    "https://ipfs.io/ipfs/" ++ cid
            in
            Html.a
                [ HA.href gatewayUrl
                , HA.target "_blank"
                , HA.rel "noopener noreferrer"
                , HA.style "color" "#3182CE"
                , HA.style "text-decoration" "none"
                , HA.style "display" "inline-flex"
                , HA.style "align-items" "center"
                , HA.style "font-size" "0.875rem"
                ]
                [ text ref.uri
                , Html.span
                    [ HA.style "margin-left" "0.25rem"
                    ]
                    [ text "↗" ]
                ]

          else
            Html.span
                [ HA.style "word-break" "break-all"
                , HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                ]
                [ text ref.uri ]
        ]



-- RATIONALE SIGNATURE STEP STYLING  #########################################################


{-| Standard card for when a step is not available
-}
stepNotAvailableCard : List (Html msg) -> Html msg
stepNotAvailableCard content =
    cardContainer []
        [ cardHeader [] "Step Not Available" "" []
        , cardContent [] content
        ]


{-| Download JSON button
-}
downloadJSONButton : String -> { filename : String, rawJson : String } -> Html msg
downloadJSONButton buttonText { filename, rawJson } =
    Html.a
        [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode rawJson
        , HA.download filename
        , HA.style "text-decoration" "none"
        ]
        [ Html.button
            [ HA.style "background-color" "#272727"
            , HA.style "color" "white"
            , HA.style "font-weight" "500"
            , HA.style "font-size" "0.875rem"
            , HA.style "padding" "0.5rem 1rem"
            , HA.style "border" "none"
            , HA.style "border-radius" "0.375rem"
            , HA.style "cursor" "pointer"
            , HA.style "display" "flex"
            , HA.style "align-items" "center"
            ]
            [ Html.span
                [ HA.style "margin-right" "0.5rem"
                ]
                [ text "📥" ]
            , text buttonText
            ]
        ]


{-| Card for authors section
-}
authorsCard : List (Html msg) -> Html msg -> Html msg
authorsCard headerContent content =
    cardContainer []
        [ div
            [ HA.style "background-color" "#F7FAFC"
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            , HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            headerContent
        , cardContent [] [ content ]
        ]


{-| Add author button
-}
addAuthorButton : msg -> Html msg
addAuthorButton clickMsg =
    blackSquareButton [] "+ Add Author" clickMsg


{-| Code snippet box
-}
codeSnippetBox : String -> Html msg
codeSnippetBox codeContent =
    div
        [ HA.style "background-color" "#F9FAFB"
        , HA.style "border" "1px solid #EDF2F7"
        , HA.style "border-radius" "0.375rem"
        , HA.style "padding" "1rem"
        , HA.style "font-family" "monospace"
        , HA.style "font-size" "0.75rem"
        , HA.style "overflow-x" "auto"
        , HA.style "white-space" "pre"
        , HA.style "margin-bottom" "1.5rem"
        ]
        [ text codeContent ]


{-| No authors placeholder
-}
noAuthorsPlaceholder : Html msg
noAuthorsPlaceholder =
    Html.p
        [ HA.style "text-align" "center"
        , HA.style "color" "#718096"
        , HA.style "font-style" "italic"
        , HA.style "padding" "1.5rem 0"
        ]
        [ text "No authors added yet." ]


{-| Signer card for displaying author information
-}
signerCard : String -> Maybe String -> String -> String -> String -> Html msg
signerCard name signature witnessAlgorithm publicKey sig =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "1rem"
        , HA.style "background-color" "#F9FAFB"
        ]
        [ Html.h4
            [ HA.style "font-weight" "600"
            , HA.style "font-size" "1rem"
            , HA.style "color" "#1A202C"
            , HA.style "margin-bottom" "0.75rem"
            ]
            [ text name ]
        , case signature of
            Nothing ->
                Html.p
                    [ HA.style "color" "#718096"
                    , HA.style "font-style" "italic"
                    ]
                    [ text "No signature provided" ]

            Just _ ->
                div
                    [ HA.style "display" "grid"
                    , HA.style "gap" "0.5rem"
                    , HA.style "font-size" "0.875rem"
                    ]
                    [ div []
                        [ Html.span
                            [ HA.style "font-weight" "500"
                            , HA.style "color" "#4A5568"
                            , HA.style "display" "inline-block"
                            , HA.style "width" "9rem"
                            ]
                            [ text "Witness algorithm:" ]
                        , Html.span
                            [ HA.style "font-family" "monospace"
                            ]
                            [ text witnessAlgorithm ]
                        ]
                    , div []
                        [ Html.span
                            [ HA.style "font-weight" "500"
                            , HA.style "color" "#4A5568"
                            , HA.style "display" "inline-block"
                            , HA.style "width" "9rem"
                            ]
                            [ text "Public key:" ]
                        , Html.span
                            [ HA.style "font-family" "monospace"
                            ]
                            [ text (String.left 10 publicKey ++ "..." ++ String.right 6 publicKey) ]
                        ]
                    , div []
                        [ Html.span
                            [ HA.style "font-weight" "500"
                            , HA.style "color" "#4A5568"
                            , HA.style "display" "inline-block"
                            , HA.style "width" "9rem"
                            ]
                            [ text "Signature:" ]
                        , Html.span
                            [ HA.style "font-family" "monospace"
                            ]
                            [ text (String.left 10 sig ++ "..." ++ String.right 6 sig) ]
                        ]
                    ]
        ]


{-| Author form with header and content
-}
authorForm : Int -> msg -> List (Html msg) -> Html msg
authorForm index deleteMsg content =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "padding" "1rem"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            , HA.style "margin-bottom" "0.75rem"
            ]
            [ Html.h4
                [ HA.style "font-weight" "500"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                ]
                [ text ("Author " ++ String.fromInt (index + 1)) ]
            , iconButton "🗑" deleteMsg
            ]
        , div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "1fr"
            , HA.style "gap" "1rem"
            ]
            content
        ]


{-| Labeled field with consistent styling
-}
labeledField : String -> Html msg -> Html msg
labeledField label content =
    div []
        [ Html.label
            [ HA.style "display" "block"
            , HA.style "font-weight" "500"
            , HA.style "font-size" "0.875rem"
            , HA.style "color" "#4B5563"
            , HA.style "margin-bottom" "0.5rem"
            ]
            [ text label ]
        , content
        ]


{-| Read-only field
-}
readOnlyField : String -> Html msg
readOnlyField value =
    Html.div
        [ HA.style "padding" "0.75rem"
        , HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.375rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "font-family" "monospace"
        , HA.style "font-size" "0.875rem"
        , HA.style "overflow" "hidden"
        , HA.style "text-overflow" "ellipsis"
        , HA.style "white-space" "nowrap"
        ]
        [ text value ]


{-| Signature field with replace button
-}
signatureField : String -> msg -> Html msg
signatureField signature replaceMsg =
    div []
        [ Html.div
            [ HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            , HA.style "margin-bottom" "0.5rem"
            ]
            [ Html.label
                [ HA.style "font-weight" "500"
                , HA.style "font-size" "0.875rem"
                , HA.style "color" "#4B5563"
                ]
                [ text "Signature" ]
            , Html.button
                [ HA.style "font-size" "0.75rem"
                , HA.style "color" "#4B5563"
                , HA.style "background" "none"
                , HA.style "border" "none"
                , HA.style "cursor" "pointer"
                , HA.style "text-decoration" "underline"
                , onClick replaceMsg
                ]
                [ text "Replace" ]
            ]
        , readOnlyField signature
        ]


{-| Form buttons row
-}
formButtonsRow : List (Html msg) -> Html msg
formButtonsRow content =
    div
        [ HA.style "display" "flex"
        , HA.style "gap" "1rem"
        , HA.style "margin-top" "1.5rem"
        , HA.style "justify-content" "space-between"
        ]
        content


{-| Secondary button
-}
secondaryButton : String -> msg -> Html msg
secondaryButton label clickMsg =
    squareButton
        [ HA.style "background-color" "#F3F4F6"
        , HA.style "color" "#4B5563"
        ]
        label
        clickMsg


{-| Primary button
-}
primaryButton : String -> msg -> Html msg
primaryButton label clickMsg =
    blackSquareButton [] label clickMsg


{-| Load signature file button
-}
loadSignatureButton : msg -> Html msg
loadSignatureButton msg =
    Html.button
        [ HA.style "background-color" "#F9FAFB"
        , HA.style "color" "#272727"
        , HA.style "font-weight" "500"
        , HA.style "font-size" "0.875rem"
        , HA.style "padding" "0.75rem"
        , HA.style "border" "1px dashed #CBD5E0"
        , HA.style "border-radius" "0.375rem"
        , HA.style "width" "100%"
        , HA.style "cursor" "pointer"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "gap" "0.5rem"
        , onClick msg
        ]
        [ Html.span [] [ text "📄" ]
        , text "Load signature file"
        ]



-- RATIONALE STORAGE STEP STYLING  #########################################################


{-| Card used for various storage screens with consistent styling
-}
storageStepCard : String -> String -> List (Html msg) -> Html msg
storageStepCard title subtitle content =
    cardContainer []
        [ cardHeader [] title subtitle []
        , cardContent [] content
        ]


{-| Card specifically for when the storage step is not available yet
-}
storageNotAvailableCard : Html msg
storageNotAvailableCard =
    storageStepCard
        "Step Not Available"
        ""
        [ Html.p
            [ HA.style "color" "#4A5568"
            , HA.style "font-size" "0.9375rem"
            ]
            [ text "Please complete the storage configuration and rationale signature steps first." ]
        ]


{-| Card for the initial upload state
-}
storageUploadCard : msg -> Html msg -> Html msg
storageUploadCard uploadMsg errorDisplay =
    div []
        [ storageStepCard
            "Store on IPFS"
            "Store your rationale on IPFS to ensure it's accessible when your vote is recorded on chain."
            [ Html.p
                [ HA.style "margin-bottom" "1.5rem"
                , HA.style "color" "#4A5568"
                ]
                [ text "Your document will be stored using the configuration you selected earlier. The CID (content identifier) will be included with your vote transaction." ]
            , errorDisplay
            ]
        , viewButton "📤 Upload to IPFS" uploadMsg
        ]


{-| Reusable spinner animation for IPFS uploads
-}
uploadingSpinner : String -> Html msg
uploadingSpinner message =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "padding" "2rem"
        , HA.style "text-align" "center"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "gap" "1rem"
            ]
            [ div
                [ HA.style "width" "3rem"
                , HA.style "height" "3rem"
                , HA.style "border" "3px solid #E2E8F0"
                , HA.style "border-top" "3px solid #3B82F6"
                , HA.style "border-radius" "50%"
                , HA.style "animation" "spin 1s linear infinite"
                ]
                []
            , Html.p
                [ HA.style "font-size" "1rem"
                , HA.style "color" "#4A5568"
                ]
                [ text message ]
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
        ]


{-| Success card with upload badge for IPFS storage
-}
storageSuccessCard : List (Html msg) -> Html msg
storageSuccessCard content =
    cardContainer []
        [ cardHeader
            [ HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            "Upload Successful"
            "Your rationale has been successfully uploaded to IPFS"
            [ successBadge "Uploaded" ]
        , cardContent [] content
        ]


{-| Grid container for file information
-}
storageInfoGrid : List (Html msg) -> Html msg
storageInfoGrid content =
    div
        [ HA.style "background-color" "#F9FAFB"
        , HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "1.25rem"
        , HA.style "margin-bottom" "1.5rem"
        ]
        [ div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "auto 1fr"
            , HA.style "gap" "0.75rem 1.5rem"
            , HA.style "align-items" "start"
            ]
            content
        ]


{-| Single file information item with label and value
-}
fileInfoItem : String -> Html msg -> List (Html msg)
fileInfoItem label content =
    [ Html.span
        [ HA.style "font-weight" "500"
        , HA.style "color" "#4A5568"
        ]
        [ text label ]
    , content
    ]


{-| External link with consistent styling for IPFS links
-}
externalLinkDisplay : String -> String -> Html msg
externalLinkDisplay url displayText =
    Html.a
        [ HA.href url
        , HA.target "_blank"
        , HA.rel "noopener noreferrer"
        , HA.style "color" "#3182CE"
        , HA.style "text-decoration" "none"
        , HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "font-family" "monospace"
        ]
        [ text displayText
        , Html.span
            [ HA.style "margin-left" "0.25rem"
            ]
            [ text "↗" ]
        ]



-- TX BUILDING STEP STYLING #########################################################


{-| Creates a standard step card with consistent styling
-}
stepCard : List (Html msg) -> Html msg
stepCard content =
    cardContainer []
        [ cardContent [] content ]


{-| Creates a transaction result card with header and status badge
-}
txResultCard : String -> String -> List (Html msg) -> Html msg
txResultCard title subtitle content =
    cardContainer []
        [ cardHeader
            [ HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            title
            subtitle
            [ successBadge "Ready" ]
        , cardContent [] content
        ]


{-| Success badge used to show completed status
-}
successBadge : String -> Html msg
successBadge label =
    div
        [ HA.style "background-color" "#F0FDF4"
        , HA.style "color" "#16A34A"
        , HA.style "padding" "0.375rem 0.75rem"
        , HA.style "border-radius" "9999px"
        , HA.style "font-size" "0.875rem"
        , HA.style "font-weight" "500"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "gap" "0.375rem"
        ]
        [ text <| "✓ " ++ label ]


{-| Vote button with consistent styling
-}
voteButton : String -> String -> msg -> Html msg
voteButton label color clickMsg =
    Html.button
        [ HA.style "background-color" color
        , HA.style "color" "white"
        , HA.style "font-weight" "500"
        , HA.style "font-size" "0.9375rem"
        , HA.style "padding" "0.75rem 2rem"
        , HA.style "border" "none"
        , HA.style "border-radius" "0.5rem"
        , HA.style "cursor" "pointer"
        , HA.style "display" "inline-flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        , HA.style "min-width" "120px"
        , onClick clickMsg
        ]
        [ text label ]


{-| Monospaced container for transaction details
-}
txDetailsContainer : List (Html msg) -> Html msg
txDetailsContainer content =
    div
        [ HA.style "background-color" "#F9FAFB"
        , HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        , HA.style "padding" "1rem"
        , HA.style "margin-bottom" "1.5rem"
        , HA.style "overflow-x" "auto"
        ]
        content


{-| Pre-formatted text container for code/json
-}
txPreContainer : String -> Html msg
txPreContainer content =
    Html.pre
        [ HA.style "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
        , HA.style "font-size" "0.875rem"
        , HA.style "white-space" "pre-wrap"
        , HA.style "word-break" "break-all"
        , HA.style "max-height" "300px"
        , HA.style "overflow-y" "auto"
        , HA.style "margin" "0"
        ]
        [ text content ]


{-| Missing steps list container
-}
missingStepsList : List (Html msg) -> Html msg
missingStepsList items =
    div
        [ HA.style "padding" "0.75rem"
        , HA.style "background-color" "#F9FAFB"
        , HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.5rem"
        ]
        [ Html.ul
            [ HA.style "list-style-type" "none"
            , HA.style "padding" "0"
            , HA.style "margin" "0"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "gap" "0.5rem"
            ]
            items
        ]


{-| Missing step item - either complete or incomplete
-}
missingStepItem : String -> Bool -> Maybe String -> Html msg
missingStepItem stepName isMissing maybeAnchorId =
    if isMissing then
        Html.li
            [ HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "gap" "0.5rem"
            ]
            [ Html.span
                [ HA.style "color" "#F59E0B"
                , HA.style "font-size" "1rem"
                ]
                [ text "⚠️" ]
            , case maybeAnchorId of
                Nothing ->
                    text stepName

                Just anchorId ->
                    Html.a
                        [ HA.href ("#" ++ anchorId)
                        , HA.style "color" "#3182CE"
                        , HA.style "text-decoration" "underline"
                        , HA.style "cursor" "pointer"
                        ]
                        [ text stepName ]
            ]

    else
        Html.li
            [ HA.style "display" "flex"
            , HA.style "align-items" "center"
            , HA.style "gap" "0.5rem"
            ]
            [ Html.span
                [ HA.style "color" "#10B981"
                , HA.style "font-size" "1rem"
                ]
                [ text "✓" ]
            , Html.span
                [ HA.style "color" "#4A5568"
                , HA.style "text-decoration" "line-through"
                ]
                [ text stepName ]
            ]


{-| Loading spinner with animation
-}
loadingSpinner : String -> Html msg
loadingSpinner message =
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "padding" "2rem"
        , HA.style "text-align" "center"
        ]
        [ div
            [ HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            , HA.style "align-items" "center"
            , HA.style "gap" "1rem"
            ]
            [ div
                [ HA.style "width" "3rem"
                , HA.style "height" "3rem"
                , HA.style "border" "3px solid #E2E8F0"
                , HA.style "border-top" "3px solid #3B82F6"
                , HA.style "border-radius" "50%"
                , HA.style "animation" "spin 1s linear infinite"
                ]
                []
            , Html.p
                [ HA.style "font-size" "1rem"
                , HA.style "color" "#4A5568"
                ]
                [ text message ]
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
        ]



-- TX SIGNING STEP STYLING #########################################################


{-| Renders a card for the signing step with consistent styling
-}
signingStepCard : String -> String -> List (Html msg) -> Html msg
signingStepCard title description content =
    cardContainer []
        [ cardHeader [] title "" []
        , cardContent []
            [ Html.p
                [ HA.style "color" "#4A5568"
                , HA.style "font-size" "0.9375rem"
                , HA.style "margin-bottom" "1rem"
                ]
                [ text description ]
            , div [] content
            ]
        ]


{-| Renders a styled list item for a key/signature in the signing step
-}
keyListItem : String -> String -> Html msg
keyListItem keyName hashHex =
    Html.li
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "padding-bottom" "0.75rem"
        , HA.style "border-bottom" "1px solid #EDF2F7"
        , HA.style "last:border-bottom" "none"
        , HA.style "last:padding-bottom" "0"
        ]
        [ Html.div
            [ HA.style "font-weight" "500"
            , HA.style "color" "#4A5568"
            , HA.style "margin-bottom" "0.25rem"
            ]
            [ text keyName ]
        , Html.div
            [ HA.style "font-family" "monospace"
            , HA.style "font-size" "0.875rem"
            , HA.style "color" "#718096"
            , HA.style "word-break" "break-all"
            ]
            [ text hashHex ]
        ]


{-| Renders a standardized signing button with consistent styling
-}
signingButton : String -> Html msg
signingButton buttonText =
    div [] [ Html.button buttonCommonStyle [ text buttonText ] ]
