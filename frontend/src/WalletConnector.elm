module WalletConnector exposing (Msgs, State, view, viewMobile)

import Cardano.Address as Address
import Cardano.Cip30 as Cip30
import Helper exposing (applyDropdownContainerStyle, applyDropdownItemStyle, applyMobileDropdownContainerStyle, applyWalletIconContainerStyle, applyWalletIconStyle, viewWalletButton)
import Html exposing (Html, div, img, li, span, text, ul)
import Html.Attributes exposing (alt, class, src, style)


type alias State =
    { walletDropdownIsOpen : Bool
    , walletsDiscovered : List Cip30.WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    }


type alias Msgs msg =
    { toggleWalletDropdown : msg
    , connectWalletClicked : { id : String, supportedExtensions : List Int } -> msg
    , disconnectWalletClicked : msg
    }


view : Msgs msg -> State -> Html msg
view msg state =
    case state.wallet of
        Nothing ->
            viewAvailableWallets state.walletsDiscovered state.walletDropdownIsOpen msg False

        Just wallet ->
            viewConnectedWallet wallet msg


viewMobile : Msgs msg -> State -> Html msg
viewMobile msg state =
    case state.wallet of
        Nothing ->
            viewAvailableWallets state.walletsDiscovered state.walletDropdownIsOpen msg True

        Just wallet ->
            viewConnectedWallet wallet msg


viewConnectedWallet : Cip30.Wallet -> Msgs msg -> Html msg
viewConnectedWallet wallet msg =
    div [ class "flex items-center" ]
        [ div [ class "text-xs mr-3" ]
            [ div [ class "font-medium" ] [ text (Cip30.walletDescriptor wallet).name ]
            , div [ class "text-xs text-gray-500" ]
                [ text (Helper.shortenedHex 9 (Address.toBech32 <| Cip30.walletChangeAddress wallet)) ]
            ]
        , viewWalletButton "Disconnect" msg.disconnectWalletClicked []
        ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Bool -> Msgs msg -> Bool -> Html msg
viewAvailableWallets wallets dropdownOpen msg isMobile =
    if List.isEmpty wallets then
        div [ class "text-xs text-gray-600" ] [ text "No wallets" ]

    else
        div [ class "relative" ]
            [ viewWalletButton "Connect Wallet"
                msg.toggleWalletDropdown
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
                div
                    (if isMobile then
                        applyMobileDropdownContainerStyle

                     else
                        applyDropdownContainerStyle
                    )
                    [ ul []
                        (List.map (viewWalletDropdownItem msg) wallets)
                    ]

              else
                text ""
            ]


viewWalletDropdownItem : Msgs msg -> Cip30.WalletDescriptor -> Html msg
viewWalletDropdownItem msg wallet =
    li (applyDropdownItemStyle (msg.connectWalletClicked { id = wallet.id, supportedExtensions = wallet.supportedExtensions }))
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
