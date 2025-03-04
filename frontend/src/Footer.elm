module Footer exposing (view)

import Html exposing (Html, a, div, footer, p, text)
import Html.Attributes exposing (class, href)


{-| Footer view component that displays copyright information and a disclaimer link.
The component takes a record with:
- copyright: String - The copyright text to display
- disclaimerLink: String - The URL to the disclaimer page
-}
view : { copyright : String, disclaimerLink : String } -> Html msg
view { copyright, disclaimerLink } =
    footer [ class "bg-gray-100 py-4 px-6 border-t border-gray-200 mt-auto" ]
        [ div [ class "container mx-auto flex flex-col md:flex-row justify-between items-center" ]
            [ p [ class "text-gray-700 text-sm mb-2 md:mb-0" ]
                [ text copyright ]
            , a 
                [ href disclaimerLink
                , class "text-blue-600 hover:text-blue-800 text-sm transition-colors duration-200"
                ]
                [ text "Legal Disclaimer" ]
            ]
        ]

