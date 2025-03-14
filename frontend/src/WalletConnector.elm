module WalletConnector exposing (Msgs, State, view)

import Cardano.Address exposing (Address)
import Cardano.Cip30 as Cip30
import Helper exposing (applyDropdownContainerStyle, applyDropdownItemStyle, applyWalletIconContainerStyle, applyWalletIconStyle, prettyAddr, viewWalletButton)
import Html exposing (Html, div, img, li, span, text, ul)
import Html.Attributes exposing (alt, class, src, style)


type alias State =
    { walletDropdownIsOpen : Bool
    , walletsDiscovered : List Cip30.WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    , walletChangeAddress : Maybe Address
    }


type alias Msgs msg =
    { toggleWalletDropdown : msg
    , connectWalletClicked : { id : String } -> msg
    , disconnectWalletClicked : msg
    }


view : Msgs msg -> State -> Html msg
view msg state =
    case state.wallet of
        Nothing ->
            viewAvailableWallets state.walletsDiscovered state.walletDropdownIsOpen msg

        Just wallet ->
            viewConnectedWallet wallet state.walletChangeAddress msg


viewConnectedWallet : Cip30.Wallet -> Maybe Address -> Msgs msg -> Html msg
viewConnectedWallet wallet maybeChangeAddress msg =
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
        , viewWalletButton "Disconnect" msg.disconnectWalletClicked []
        ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Bool -> Msgs msg -> Html msg
viewAvailableWallets wallets dropdownOpen msg =
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
                div applyDropdownContainerStyle
                    [ ul []
                        (List.map (viewWalletDropdownItem msg) wallets)
                    ]

              else
                text ""
            ]


viewWalletDropdownItem : Msgs msg -> Cip30.WalletDescriptor -> Html msg
viewWalletDropdownItem msg wallet =
    li (applyDropdownItemStyle (msg.connectWalletClicked { id = wallet.id }))
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
