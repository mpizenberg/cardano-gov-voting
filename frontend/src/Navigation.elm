module Navigation exposing
    ( Msg(..)
    , NavState
    , init
    , update
    , view
    )

import Cardano.Address exposing (Address)
import Cardano.Cip30 as Cip30
import Helper exposing (applyDropdownContainerStyle, applyDropdownItemStyle, applyWalletIconContainerStyle, applyWalletIconStyle, prettyAddr, viewWalletButton)
import Html exposing (Html, a, button, div, img, li, nav, span, text, ul)
import Html.Attributes exposing (alt, class, href, src, style)
import Html.Events exposing (onClick)


type alias NavState =
    { isOpen : Bool
    , walletDropdownOpen : Bool
    }


type Msg
    = ToggleMobileMenu
    | ToggleWalletDropdown
    | ConnectWalletClicked { id : String }
    | DisconnectWalletClicked


init : NavState
init =
    { isOpen = False
    , walletDropdownOpen = False
    }


update : Msg -> NavState -> ( NavState, Cmd Msg )
update msg state =
    case msg of
        ToggleMobileMenu ->
            ( { state | isOpen = not state.isOpen }, Cmd.none )

        ToggleWalletDropdown ->
            ( { state | walletDropdownOpen = not state.walletDropdownOpen }, Cmd.none )

        ConnectWalletClicked _ ->
            ( { state | walletDropdownOpen = False }, Cmd.none )

        DisconnectWalletClicked ->
            ( state, Cmd.none )


view :
    { state : NavState
    , toMsg : Msg -> msg
    , brand : String
    , items : List { label : String, url : String, isActive : Bool }
    , wallet : Maybe Cip30.Wallet
    , walletsDiscovered : List Cip30.WalletDescriptor
    , walletChangeAddress : Maybe Address
    }
    -> Html msg
view config =
    nav [ class "relative z-10 w-full bg-transparent" ]
        [ div [ class "container mx-auto py-6", style "padding-right" "6px" ]
            [ div [ class "flex items-center justify-between" ]
                -- Logo/Brand section
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
                            [ text config.brand ]
                        ]
                    ]

                -- Desktop menu
                , div [ class "hidden md:flex space-x-8" ]
                    (List.map viewDesktopMenuItem config.items)

                -- Wallet section (on the right)
                , div [ class "hidden md:flex" ]
                    [ viewWalletSection config ]

                -- Mobile menu button
                , div [ class "md:hidden" ]
                    [ button
                        [ class "text-gray-600 hover:text-gray-900 focus:outline-none"
                        , onClick (config.toMsg ToggleMobileMenu)
                        ]
                        [ -- Hamburger icon
                          div [ class "w-6 h-6 flex flex-col justify-around" ]
                            [ span
                                [ class
                                    ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 "
                                        ++ (if config.state.isOpen then
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
                                        ++ (if config.state.isOpen then
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
                                        ++ (if config.state.isOpen then
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
                        ++ (if config.state.isOpen then
                                "max-h-64"

                            else
                                "max-h-0"
                           )
                    )
                ]
                [ div [ class "py-2" ]
                    [ ul [ class "flex flex-col space-y-4 mt-4" ]
                        (List.map viewMobileMenuItem config.items)
                    , div [ class "mt-4 px-4" ]
                        [ viewWalletSection config ]
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


viewWalletSection : { a | wallet : Maybe Cip30.Wallet, walletsDiscovered : List Cip30.WalletDescriptor, walletChangeAddress : Maybe Address, state : NavState, toMsg : Msg -> msg } -> Html msg
viewWalletSection config =
    div [ class "flex-shrink-0 min-w-[150px]" ]
        [ case config.wallet of
            Nothing ->
                viewAvailableWallets config.walletsDiscovered config.state.walletDropdownOpen config.toMsg

            Just wallet ->
                viewConnectedWallet wallet config.walletChangeAddress config.toMsg
        ]


viewConnectedWallet : Cip30.Wallet -> Maybe Address -> (Msg -> msg) -> Html msg
viewConnectedWallet wallet maybeChangeAddress toMsg =
    div [ class "flex items-center" ]
        [ div [ class "text-xs mr-3" ]
            [ div [ class "font-medium" ] [ text (Cip30.walletDescriptor wallet).name ]
            , case maybeChangeAddress of
                Just addr ->
                    div [ class "text-xs text-gray-500" ]
                        [ text (String.left 4 (prettyAddr addr) ++ "...") ]

                Nothing ->
                    text ""
            ]
        , viewWalletButton "Disconnect" (toMsg DisconnectWalletClicked) []
        ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Bool -> (Msg -> msg) -> Html msg
viewAvailableWallets wallets dropdownOpen toMsg =
    if List.isEmpty wallets then
        div [ class "text-xs text-gray-600" ] [ text "No wallets" ]

    else
        div [ class "relative" ]
            [ viewWalletButton "Connect Wallet"
                (toMsg ToggleWalletDropdown)
                [ span
                    [ class "transition-transform"
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
                div applyDropdownContainerStyle
                    [ ul []
                        (List.map (viewWalletDropdownItem toMsg) wallets)
                    ]

              else
                text ""
            ]


viewWalletDropdownItem : (Msg -> msg) -> Cip30.WalletDescriptor -> Html msg
viewWalletDropdownItem toMsg wallet =
    li (applyDropdownItemStyle (toMsg (ConnectWalletClicked { id = wallet.id })))
        [ if not (String.isEmpty wallet.icon) then
            div applyWalletIconContainerStyle
                [ img
                    ([ src wallet.icon
                     , alt wallet.name
                     ]
                        ++ applyWalletIconStyle
                    )
                    []
                ]

          else
            div applyWalletIconContainerStyle []
        , text wallet.name
        ]
