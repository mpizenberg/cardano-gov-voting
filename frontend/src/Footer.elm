module Footer exposing (view)

import Html exposing (Html, a, div, footer, p, text)
import Html.Attributes exposing (class, href, style)


{-| Footer view component that displays copyright information and a disclaimer link.
The component takes a record with:

  - copyright: String - The copyright text to display
  - disclaimerLink: String - The URL to the disclaimer page

-}
view : { copyright : String, disclaimerLink : String, githubLink : String } -> Html msg
view { copyright, disclaimerLink, githubLink } =
    footer
        [ class "bg-transparent py-4 px-6 border-t"
        , style "border-color" "#C6C6C6"
        , style "position" "absolute"
        , style "bottom" "0"
        , style "width" "100%"
        ]
        [ div [ class "container mx-auto flex justify-between items-center" ]
            [ p [ class "text-gray-700 text-sm" ]
                [ text copyright ]
            , div [ class "flex space-x-8" ]
                -- Changed from space-x-4 to space-x-8 for more spacing
                [ a
                    [ href disclaimerLink
                    , class "text-sm transition-colors duration-200"
                    ]
                    [ text "Legal Disclaimer" ]
                , a
                    [ href githubLink
                    , class "text-sm transition-colors duration-200"
                    ]
                    [ text "Github" ]
                ]
            ]
        ]
