module Header exposing
    ( ViewContext
    , view
    )

import Html exposing (Html, a, button, div, img, li, nav, span, text, ul)
import Html.Attributes exposing (alt, class, href, src, style)
import Html.Events exposing (onClick)
import WalletConnector


type alias ViewContext msg =
    { mobileMenuIsOpen : Bool
    , toggleMobileMenu : msg

    -- Wallet connector stuff
    , walletConnector : WalletConnector.State
    , walletConnectorMsgs : WalletConnector.Msgs msg
    }


view :
    ViewContext msg
    -> List { label : String, url : String, isActive : Bool }
    -> Html msg
view { mobileMenuIsOpen, toggleMobileMenu, walletConnector, walletConnectorMsgs } items =
    nav [ class "relative z-10 w-full bg-transparent" ]
        [ div [ class "container mx-auto py-6", style "padding-right" "6px" ]
            [ div [ class "flex items-center justify-between" ]
                -- Logo section
                [ div [ style "flex-shrink" "0" ]
                    [ a
                        [ href "/"
                        , style "display" "flex"
                        , style "align-items" "center"
                        ]
                        [ img
                            [ src "/logo/foundation-logo.svg"
                            , alt "Logo"
                            , style "width" "auto"
                            , style "margin-right" "8px"
                            , style "margin-left" "8px"
                            , style "max-height" "44px"
                            ]
                            []
                        , span
                            [ style "font-weight" "700"
                            , style "font-size" "1.125rem"
                            ]
                            [ text "" ]
                        ]
                    ]

                -- Desktop menu
                , div [ class "hidden md:flex space-x-8" ]
                    (List.map viewDesktopMenuItem items)

                -- Wallet section (on the right)
                , div [ class "hidden md:flex" ]
                    [ div [ class "flex-shrink-0 min-w-[150px]" ]
                        [ WalletConnector.view walletConnectorMsgs walletConnector ]
                    ]

                -- Mobile menu button
                , div [ class "md:hidden" ]
                    [ button
                        [ class "text-gray-600 hover:text-gray-900 focus:outline-none"
                        , onClick toggleMobileMenu
                        ]
                        [ -- Hamburger icon
                          div [ class "w-6 h-6 flex flex-col justify-around" ]
                            [ span
                                [ class
                                    ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 "
                                        ++ (if mobileMenuIsOpen then
                                                "rotate-45 translate-y-1.5"

                                            else
                                                ""
                                           )
                                    )
                                ]
                                []
                            , span
                                [ class
                                    ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 "
                                        ++ (if mobileMenuIsOpen then
                                                "opacity-0"

                                            else
                                                ""
                                           )
                                    )
                                ]
                                []
                            , span
                                [ class
                                    ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 "
                                        ++ (if mobileMenuIsOpen then
                                                "-rotate-45 -translate-y-1.5"

                                            else
                                                ""
                                           )
                                    )
                                ]
                                []
                            ]
                        ]
                    ]
                ]

            -- Mobile menu (responsive)
            , div
                [ class
                    ("md:hidden transition-all duration-300 ease-in-out overflow-hidden "
                        ++ (if mobileMenuIsOpen then
                                "max-h-64"

                            else
                                "max-h-0"
                           )
                    )
                ]
                [ div [ class "py-2" ]
                    [ ul [ class "flex flex-col space-y-4 mt-4" ]
                        (List.map viewMobileMenuItem items)
                    , div [ class "mt-4 px-4" ]
                        [ div [ class "flex-shrink-0 min-w-[150px]" ]
                            [ WalletConnector.view walletConnectorMsgs walletConnector ]
                        ]
                    ]
                ]
            ]
        ]


viewDesktopMenuItem : { label : String, url : String, isActive : Bool } -> Html msg
viewDesktopMenuItem item =
    a
        [ href item.url
        , class
            ("px-4 py-2 font-medium transition-all duration-200 "
                ++ (if item.isActive then
                        "text-gray-900 border-b-2 border-gray-800"

                    else
                        "text-gray-800 hover:text-gray-900 hover:border-b-2 hover:border-gray-800"
                   )
            )
        ]
        [ text item.label ]


viewMobileMenuItem : { label : String, url : String, isActive : Bool } -> Html msg
viewMobileMenuItem item =
    li []
        [ a
            [ href item.url
            , class
                ("block px-4 py-2 rounded-md transition-colors duration-200 "
                    ++ (if item.isActive then
                            "text-gray-900 bg-gray-100"

                        else
                            "text-gray-800 hover:bg-gray-100"
                       )
                )
            ]
            [ text item.label ]
        ]
