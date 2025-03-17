module Footer exposing (ViewContext, view)

import Html exposing (Html, a, div, footer, p, text)
import Html.Attributes exposing (class, href, style, target)


type alias ViewContext msg =
    { copyright : String
    , githubLink : String
    , disclaimerLink : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    }


{-| Footer view component that displays copyright information and a disclaimer link.
The component takes a record with:

  - copyright: String - The copyright text to display
  - disclaimerLink: String - The URL to the disclaimer page

-}
view : ViewContext msg -> Html msg
view { copyright, githubLink, disclaimerLink } =
    footer
        [ class "bg-transparent py-4 px-6 border-t"
        , style "border-color" "#C6C6C6"
        , style "position" "absolute"
        , style "bottom" "0"
        , style "width" "100%"
        ]
        [ div [ class "container mx-auto flex flex-col md:flex-row justify-between items-center space-y-2 md:space-y-0" ]
            [ p [ class "text-gray-700 text-sm" ]
                [ text copyright ]
            , div [ class "flex flex-col md:flex-row items-center" ]
                [ p [ class "text-gray-600 text-sm mb-2", style "margin-right" "8px" ]
                    [ text "Powered by "
                    , a
                        [ href "https://api.koios.rest/"
                        , class "text-gray-700 underline"
                        , target "_blank"
                        ]
                        [ text "Koios API" ]
                    ]
                , disclaimerLink
                    [ class "text-sm mb-2", style "margin-right" "8px" ]
                    [ text "Legal Disclaimer" ]
                , a
                    [ href githubLink
                    , class "text-sm mb-2"
                    , target "_blank"
                    ]
                    [ text "Github" ]
                ]
            ]
        ]
