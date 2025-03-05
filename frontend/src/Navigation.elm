module Navigation exposing 
    ( NavState
    , Msg(..)
    , init
    , update
    , view
    , Route(..)
    , routeToAppUrl
    , locationHrefToRoute
    , link
    )

import AppUrl exposing (AppUrl)
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Address, CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction)
import Dict exposing (Dict)
import Helper exposing (prettyAddr)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, preventDefaultOn)
import Json.Decode as JD exposing (Decoder)
import Url
type alias NavState =
    { isOpen : Bool
    }

type Msg
    = ToggleMobileMenu
    | CloseMenu
    | ConnectWalletClicked { id : String }
    | DisconnectWalletClicked

type Route
    = RouteLanding
    | RoutePreparation
    | RouteSigning { expectedSigners : List (Bytes CredentialHash), tx : Maybe Transaction }
    | RouteMultisigRegistration
    | RoutePdf
    | RouteDisclaimer
    | Route404

init : NavState
init =
    { isOpen = False
    }

update : Msg -> NavState -> (NavState, Cmd Msg)
update msg state =
    case msg of
        ToggleMobileMenu ->
            ({ state | isOpen = not state.isOpen }, Cmd.none)

        CloseMenu ->
            ({ state | isOpen = False }, Cmd.none)
        
        -- These will be handled by Main.elm
        ConnectWalletClicked _ ->
            (state, Cmd.none)
            
        DisconnectWalletClicked ->
            (state, Cmd.none)

link : (Route -> msg) -> Route -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
link urlChangedMsg route attrs children =
    Html.a
        (preventDefaultOn "click" (linkClickDecoder urlChangedMsg route)
            :: href (AppUrl.toString <| routeToAppUrl <| route)
            :: attrs
        )
        children

linkClickDecoder : (Route -> msg) -> Route -> Decoder ( msg, Bool )
linkClickDecoder urlChangedMsg route =
    -- Custom decoder on link clicks to not overwrite expected behaviors for click with modifiers.
    -- For example, Ctrl+Click should open in a new tab, and Shift+Click in a new window.
    JD.map4
        (\ctrl meta shift wheel ->
            if ctrl || meta || shift || wheel /= 0 then
                ( urlChangedMsg route, False )
            else
                ( urlChangedMsg route, True )
        )
        (JD.field "ctrlKey" JD.bool)
        (JD.field "metaKey" JD.bool)
        (JD.field "shiftKey" JD.bool)
        (JD.field "button" JD.int)

locationHrefToRoute : String -> Route
locationHrefToRoute locationHref =
    case Url.fromString locationHref |> Maybe.map AppUrl.fromUrl of
        Nothing ->
            Route404

        Just { path, queryParameters, fragment } ->
            case path of
                [] ->
                    RouteLanding

                [ "page", "preparation" ] ->
                    RoutePreparation

                [ "page", "signing" ] ->
                    RouteSigning
                        { expectedSigners =
                            Dict.get "signer" queryParameters
                                |> Maybe.withDefault []
                                |> List.filterMap Bytes.fromHex
                        , tx =
                            Maybe.andThen Bytes.fromHex fragment
                                |> Maybe.andThen Transaction.deserialize
                        }

                [ "page", "registration" ] ->
                    RouteMultisigRegistration

                [ "page", "pdf" ] ->
                    RoutePdf
                    
                [ "page", "disclaimer" ] ->
                    RouteDisclaimer

                _ ->
                    Route404

routeToAppUrl : Route -> AppUrl
routeToAppUrl route =
    case route of
        Route404 ->
            AppUrl.fromPath [ "404" ]

        RouteLanding ->
            AppUrl.fromPath []

        RoutePreparation ->
            AppUrl.fromPath [ "page", "preparation" ]

        RouteSigning { expectedSigners, tx } ->
            { path = [ "page", "signing" ]
            , queryParameters = Dict.singleton "signer" <| List.map Bytes.toHex expectedSigners
            , fragment = Maybe.map (Bytes.toHex << Transaction.serialize) tx
            }

        RouteMultisigRegistration ->
            AppUrl.fromPath [ "page", "registration" ]
            
        RoutePdf ->
            AppUrl.fromPath [ "page", "pdf" ]
            
        RouteDisclaimer ->
            AppUrl.fromPath [ "page", "disclaimer" ]

view : { 
    state : NavState, 
    toMsg : Msg -> msg, 
    brand : String, 
    items : List { label : String, url : String, isActive : Bool },
    wallet : Maybe Cip30.Wallet,
    walletsDiscovered : List Cip30.WalletDescriptor,
    walletChangeAddress : Maybe Address
    } -> Html msg
view config =
    nav [ class "relative z-10 w-full bg-transparent" ]
        [ div [ class "container mx-auto py-6" ]
            [ div [ class "flex items-center justify-between" ]
                -- Logo/Brand section
                [ div [ class "flex-shrink-0" ]
                    [ a [ href "/", class "flex items-center" ]
                        [ img [ src "/logo/foundation-logo.png", alt "Logo", class "h-8 w-auto mr-2" ] []
                        , span [ class "font-bold text-lg" ] [ text config.brand ]
                        ]
                    ]
                
                -- Desktop menu
                , div [ class "hidden md:flex space-x-8" ] 
                    (List.map (viewDesktopMenuItem config.toMsg) config.items)
                
                -- Wallet section (on the right)
                , div [ class "hidden md:flex" ]
                    [ viewWalletSection config ]
                
                -- Mobile menu button
                , div [ class "md:hidden" ]
                    [ button 
                        [ class "text-gray-600 hover:text-gray-900 focus:outline-none", 
                          onClick (config.toMsg ToggleMobileMenu) 
                        ]
                        [ -- Hamburger icon
                          div [ class "w-6 h-6 flex flex-col justify-around" ]
                            [ span [ class ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 " ++ 
                                if config.state.isOpen then "rotate-45 translate-y-1.5" else "") 
                              ] []
                            , span [ class ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 " ++ 
                                if config.state.isOpen then "opacity-0" else "") 
                              ] []
                            , span [ class ("block w-6 h-0.5 bg-gray-800 transition-all duration-300 " ++ 
                                if config.state.isOpen then "-rotate-45 -translate-y-1.5" else "") 
                              ] []
                            ]
                        ]
                    ]
                ]
            
            -- Mobile menu (responsive)
            , div 
                [ class ("md:hidden transition-all duration-300 ease-in-out overflow-hidden " ++ 
                    if config.state.isOpen then "max-h-64" else "max-h-0")
                ]
                [ div [ class "py-2" ]
                    [ ul [ class "flex flex-col space-y-4 mt-4" ]
                        (List.map (viewMobileMenuItem config.toMsg) config.items)
                    , div [ class "mt-4 px-4" ]
                        [ viewWalletSection config ]
                    ]
                ]
            ]
        ]

viewDesktopMenuItem : (Msg -> msg) -> { label : String, url : String, isActive : Bool } -> Html msg
viewDesktopMenuItem toMsg item =
    a [ href item.url, 
        class ("px-4 py-2 font-medium transition-all duration-200 " ++
            if item.isActive then 
                "text-gray-900 border-b-2 border-gray-800" 
            else 
                "text-gray-800 hover:text-gray-900 hover:border-b-2 hover:border-gray-800")
      ]
      [ text item.label ]

viewMobileMenuItem : (Msg -> msg) -> { label : String, url : String, isActive : Bool } -> Html msg
viewMobileMenuItem toMsg item =
    li []
        [ a [ href item.url, 
              class ("block px-4 py-2 rounded-md transition-colors duration-200 " ++
                if item.isActive then 
                    "text-gray-900 bg-gray-100" 
                else 
                    "text-gray-800 hover:bg-gray-100")
            ]
            [ text item.label ]
        ]

viewWalletSection : { a | wallet : Maybe Cip30.Wallet, walletsDiscovered : List Cip30.WalletDescriptor, walletChangeAddress : Maybe Address, toMsg : Msg -> msg } -> Html msg
viewWalletSection config =
    div [ class "flex-shrink-0 max-w-[120px]" ]
        [ case config.wallet of
            Nothing ->
                viewAvailableWallets config.walletsDiscovered config.toMsg

            Just wallet ->
                viewConnectedWallet wallet config.walletChangeAddress config.toMsg
        ]

viewConnectedWallet : Cip30.Wallet -> Maybe Address -> (Msg -> msg) -> Html msg
viewConnectedWallet wallet maybeChangeAddress toMsg =
    div [ class "flex items-center" ]
        [ div [ class "text-xs mr-1" ]
            [ div [ class "font-medium" ] [ text (Cip30.walletDescriptor wallet).name ]
            , case maybeChangeAddress of
                Just addr ->
                    div [ class "text-xs text-gray-500" ] 
                        [ text (String.left 4 (prettyAddr addr) ++ "...") ]  

                Nothing ->
                    text ""
            ]
        , button
            [ onClick (toMsg DisconnectWalletClicked)
            , style "display" "inline-flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "white-space" "nowrap"
            , style "border-radius" "9999px"
            , style "font-size" "0.7rem" 
            , style "font-weight" "500"
            , style "transition" "all 0.2s"
            , style "outline" "none"
            , style "background-color" "#272727"
            , style "color" "#f7fafc"
            , style "height" "3rem" 
            , style "padding-left" "2rem"  
            , style "padding-right" "2rem" 
            ]
            [ text "Disconnect" ]
        ]

viewAvailableWallets : List Cip30.WalletDescriptor -> (Msg -> msg) -> Html msg
viewAvailableWallets wallets toMsg =
    if List.isEmpty wallets then
        div [ class "text-xs text-gray-600" ] [ text "No wallets" ]
    else
        div [ class "flex items-center space-x-1" ] 
            (List.map (viewWalletConnectButton toMsg) wallets)

viewWalletConnectButton : (Msg -> msg) -> Cip30.WalletDescriptor -> Html msg
viewWalletConnectButton toMsg wallet =
    button
        [ onClick (toMsg (ConnectWalletClicked { id = wallet.id }))
        , style "display" "inline-flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "white-space" "nowrap"
        , style "border-radius" "9999px"
        , style "font-size" "0.7rem"
        , style "font-weight" "500"
        , style "transition" "all 0.2s"
        , style "outline" "none"
        , style "background-color" "#272727"
        , style "color" "#f7fafc"
        , style "height" "3rem"
        , style "padding-left" "2rem"
        , style "padding-right" "2rem"
        ]
        [ if not (String.isEmpty wallet.icon) then
            img [ src wallet.icon, 
                 style "height" "20px", 
                 style "margin-right" "0.25rem", 
                 alt wallet.name 
               ] []
          else
            text ""
        , text wallet.name
        ]