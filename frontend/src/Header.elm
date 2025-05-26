module Header exposing (ViewContext, view)

import Cardano.Address exposing (NetworkId(..))
import Helper
import Html exposing (Html, button, div, img, li, nav, span, text, ul)
import Html.Attributes exposing (alt, class, src, style)
import Html.Events exposing (onClick)
import WalletConnector


type alias ViewContext msg =
    { mobileMenuIsOpen : Bool
    , toggleMobileMenu : msg
    , networkDropdownIsOpen : Bool
    , toggleNetworkDropdown : msg
    , walletConnector : WalletConnector.State
    , walletConnectorMsgs : WalletConnector.Msgs msg
    , logoLink : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , navigationItems :
        List
            { link : List (Html.Attribute msg) -> List (Html msg) -> Html msg
            , label : String
            , isActive : Bool
            }
    , networkId : NetworkId
    , onNetworkChange : NetworkId -> msg
    }


view : ViewContext msg -> Html msg
view { mobileMenuIsOpen, toggleMobileMenu, networkDropdownIsOpen, toggleNetworkDropdown, walletConnector, walletConnectorMsgs, logoLink, navigationItems, networkId, onNetworkChange } =
    nav [ class "relative z-10 w-full bg-transparent" ]
        [ div [ class "container mx-auto py-6 overflow-visible" ]
            [ div [ class "flex items-center justify-between" ]
                -- Logo section
                [ div [ style "flex-shrink" "0" ]
                    [ logoLink
                        [ style "display" "flex"
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
                , div [ class "hidden md:flex space-x-12" ]
                    (List.map viewDesktopMenuItem navigationItems)

                -- Wallet and network selector
                , div [ class "hidden md:flex items-center" ]
                    [ WalletConnector.view walletConnectorMsgs walletConnector
                    , viewNetworkSelector
                        networkId
                        networkDropdownIsOpen
                        toggleNetworkDropdown
                        onNetworkChange
                    ]

                -- Mobile menu button
                , div [ class "md:hidden" ]
                    [ button
                        [ class "text-gray-600 hover:text-gray-900 focus:outline-none"
                        , onClick toggleMobileMenu
                        ]
                        [ -- Hamburger icon
                          div [ class "mr-4 w-6 h-6 flex flex-col justify-around" ]
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

            -- Mobile menu
            , div
                [ class
                    ("md:hidden transition-all duration-300 ease-in-out "
                        ++ (if mobileMenuIsOpen then
                                "max-h-[500px] overflow-visible"

                            else
                                "max-h-0 overflow-hidden"
                           )
                    )
                ]
                [ div [ class "py-2" ]
                    [ ul [ class "flex flex-col space-y-4 mt-4" ]
                        (List.map viewMobileMenuItem navigationItems)
                    , div [ class "mt-4 px-4 z-20 relative" ]
                        [ div [ class "w-full space-y-2" ]
                            [ WalletConnector.viewMobile walletConnectorMsgs walletConnector
                            , viewMobileNetworkSelector networkId onNetworkChange
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewDesktopMenuItem :
    { link : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , label : String
    , isActive : Bool
    }
    -> Html msg
viewDesktopMenuItem item =
    item.link
        [ class
            ("px-4 py-2 font-medium transition-all duration-200 "
                ++ (if item.isActive then
                        "text-gray-900 border-b-2 border-gray-800"

                    else
                        "text-gray-800 hover:text-gray-900 hover:border-b-2 hover:border-gray-800"
                   )
            )
        ]
        [ text item.label ]


viewMobileMenuItem :
    { link : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , label : String
    , isActive : Bool
    }
    -> Html msg
viewMobileMenuItem item =
    li []
        [ item.link
            [ class
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


viewNetworkSelector : NetworkId -> Bool -> msg -> (NetworkId -> msg) -> Html msg
viewNetworkSelector currentNetwork dropdownOpen toggleDropdown onNetworkChange =
    let
        isMainnet =
            currentNetwork == Mainnet

        networkLabel =
            if isMainnet then
                "Mainnet"

            else
                "Preview"

        otherNetwork =
            if isMainnet then
                Testnet

            else
                Mainnet

        otherNetworkLabel =
            if isMainnet then
                "Preview"

            else
                "Mainnet"

        networkColor =
            if isMainnet then
                "#10b981"

            else
                "#3b82f6"

        -- Green for Mainnet, Blue for Preview
    in
    div [ style "position" "relative", style "margin-left" "0.75rem" ]
        [ Helper.viewWalletButton networkLabel
            toggleDropdown
            [ -- Network indicator dot
              div
                [ style "width" "0.75rem"
                , style "height" "0.75rem"
                , style "border-radius" "9999px"
                , style "background-color" networkColor
                , style "margin-left" "0.2rem"
                ]
                []
            , span
                [ style "transition-transform" "0.2s"
                , style "margin-left" "4px"
                , style "transform"
                    (if dropdownOpen then
                        "rotate(180deg)"

                     else
                        "rotate(0)"
                    )
                ]
                [ text "▼" ]
            ]
        , if dropdownOpen then
            div Helper.applyDropdownContainerStyle
                [ ul []
                    [ li (Helper.applyDropdownItemStyle (onNetworkChange otherNetwork))
                        [ -- Network indicator dot for other network
                          div
                            [ style "width" "0.75rem"
                            , style "height" "0.75rem"
                            , style "border-radius" "9999px"
                            , style "background-color"
                                (if not isMainnet then
                                    "#10b981"

                                 else
                                    "#3b82f6"
                                )
                            , style "margin-right" "0.5rem"
                            ]
                            []
                        , text otherNetworkLabel
                        ]
                    ]
                ]

          else
            text ""
        ]


viewMobileNetworkSelector : NetworkId -> (NetworkId -> msg) -> Html msg
viewMobileNetworkSelector currentNetwork onNetworkChange =
    let
        ( networkAsText, networkColor, otherNetwork ) =
            case currentNetwork of
                Mainnet ->
                    ( "Mainnet", "#10b981", Testnet )

                Testnet ->
                    ( "Preview", "#3b82f6", Mainnet )
    in
    div
        [ style "display" "inline-flex"
        , style "align-items" "center"
        , style "padding" "0.75rem 2.25rem"
        , style "margin-top" "0.5rem"
        , style "border-radius" "9999px"
        , style "background-color" "#272727"
        , style "color" "white"
        , style "cursor" "pointer"
        , style "font-size" "0.875rem"
        , style "font-weight" "500"
        , style "box-shadow" "0 1px 3px rgba(0,0,0,0.1)"
        , style "transition" "background-color 0.2s"
        , onClick (onNetworkChange otherNetwork)
        ]
        [ div
            [ style "width" "0.75rem"
            , style "height" "0.75rem"
            , style "border-radius" "9999px"
            , style "background-color" networkColor
            , style "margin-right" "0.5rem"
            ]
            []
        , text networkAsText
        , div
            [ style "margin-left" "0.5rem"
            , style "font-size" "0.75rem"
            ]
            [ text "↺" ]
        ]
