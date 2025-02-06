port module Main exposing (main)

import Api exposing (ActiveProposal, DrepInfo, ProposalMetadata, ProtocolParams, ScriptInfo)
import AppUrl exposing (AppUrl)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, CredentialHash)
import Cardano.Cip30 as Cip30 exposing (WalletDescriptor)
import Cardano.Gov as Gov
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output)
import Dict exposing (Dict)
import Helper exposing (prettyAddr)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA exposing (height, src)
import Html.Events exposing (onClick, preventDefaultOn)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Page.MultisigRegistration
import Page.Pdf
import Page.Preparation exposing (JsonLdContexts)
import Page.Signing
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)
import Url


main : Program { url : String, jsonLdContexts : JsonLdContexts } Model Msg
main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ fromWallet WalletMsg
                    , onUrlChange (locationHrefToRoute >> UrlChanged)
                    , gotRationaleAsFile GotRationaleAsFile
                    ]
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


port jsonRationaleToFile : { fileContent : String, fileName : String } -> Cmd msg


port gotRationaleAsFile : (Value -> msg) -> Sub msg



-- #########################################################
-- MODEL
-- #########################################################


type alias Model =
    { page : Page
    , walletsDiscovered : List WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    , walletChangeAddress : Maybe Address
    , walletUtxos : Maybe (Utxo.RefDict Output)
    , protocolParams : Maybe ProtocolParams
    , proposals : WebData (Dict String ActiveProposal)
    , scriptsInfo : Dict String ScriptInfo
    , drepsInfo : Dict String DrepInfo
    , jsonLdContexts : JsonLdContexts
    , errors : List String
    }


type Page
    = LandingPage
    | PreparationPage Page.Preparation.Model
    | SigningPage Page.Signing.Model
    | MultisigRegistrationPage Page.MultisigRegistration.Model
    | PdfPage Page.Pdf.Model


init : { url : String, jsonLdContexts : JsonLdContexts } -> ( Model, Cmd Msg )
init { url, jsonLdContexts } =
    handleUrlChange (locationHrefToRoute url)
        { page = LandingPage
        , walletsDiscovered = []
        , wallet = Nothing
        , walletUtxos = Nothing
        , walletChangeAddress = Nothing
        , protocolParams = Nothing
        , proposals = RemoteData.NotAsked
        , scriptsInfo = Dict.empty
        , drepsInfo = Dict.empty
        , jsonLdContexts = jsonLdContexts
        , errors = []
        }
        |> (\( model, cmd ) ->
                ( model
                , Cmd.batch
                    [ cmd
                    , toWallet (Cip30.encodeRequest Cip30.discoverWallets)
                    , Api.defaultApiProvider.loadProtocolParams GotProtocolParams
                    ]
                )
           )



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = NoMsg
    | UrlChanged Route
    | WalletMsg Value
    | ConnectButtonClicked { id : String }
    | DisconnectWalletButtonClicked
    | GotProtocolParams (Result Http.Error ProtocolParams)
    | GotProposals (Result Http.Error (List ActiveProposal))
    | GotProposalMetadata String (Result String ProposalMetadata)
      -- Preparation page
    | PreparationPageMsg Page.Preparation.Msg
    | GotRationaleAsFile Value
      -- Signing page
    | SigningPageMsg Page.Signing.Msg
      -- Multisig DRep registration page
    | MultisigPageMsg Page.MultisigRegistration.Msg
      -- PDF page
    | PdfPageMsg Page.Pdf.Msg


type Route
    = RouteLanding
    | RoutePreparation
    | RouteSigning { expectedSigners : List (Bytes CredentialHash), tx : Maybe Transaction }
    | RouteMultisigRegistration
    | RoutePdf
    | Route404


link : Route -> List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
link route attrs children =
    Html.a
        (preventDefaultOn "click" (linkClickDecoder route)
            :: HA.href (AppUrl.toString <| routeToAppUrl <| route)
            :: attrs
        )
        children


linkClickDecoder : Route -> Decoder ( Msg, Bool )
linkClickDecoder route =
    -- Custom decoder on link clicks to not overwrite expected behaviors for click with modifiers.
    -- For example, Ctrl+Click should open in a new tab, and Shift+Click in a new window.
    JD.map4
        (\ctrl meta shift wheel ->
            if ctrl || meta || shift || wheel /= 0 then
                ( NoMsg, False )

            else
                ( UrlChanged route, True )
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( NoMsg, _ ) ->
            ( model, Cmd.none )

        ( UrlChanged route, _ ) ->
            handleUrlChange route model

        ( GotProtocolParams result, _ ) ->
            case result of
                Ok params ->
                    ( { model | protocolParams = Just params }, Cmd.none )

                Err err ->
                    ( { model | errors = Debug.toString err :: model.errors }, Cmd.none )

        ( ConnectButtonClicked { id }, _ ) ->
            ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [] })) )

        ( DisconnectWalletButtonClicked, _ ) ->
            ( { model | wallet = Nothing, walletChangeAddress = Nothing, walletUtxos = Nothing }
            , Cmd.none
            )

        ( WalletMsg value, _ ) ->
            case JD.decodeValue Cip30.responseDecoder value of
                Ok response ->
                    handleWalletResponse response model

                Err decodingError ->
                    ( { model | errors = JD.errorToString decodingError :: model.errors }
                    , Cmd.none
                    )

        ( PreparationPageMsg pageMsg, { page } ) ->
            case page of
                PreparationPage pageModel ->
                    let
                        loadedWallet =
                            case ( model.wallet, model.walletChangeAddress, model.walletUtxos ) of
                                ( Just wallet, Just address, Just utxos ) ->
                                    Just { wallet = wallet, changeAddress = address, utxos = utxos }

                                _ ->
                                    Nothing

                        ctx =
                            { wrapMsg = PreparationPageMsg
                            , proposals = model.proposals
                            , scriptsInfo = model.scriptsInfo
                            , drepsInfo = model.drepsInfo
                            , loadedWallet = loadedWallet
                            , feeProviderAskUtxosCmd = Cmd.none -- TODO
                            , jsonLdContexts = model.jsonLdContexts
                            , jsonRationaleToFile = jsonRationaleToFile
                            , costModels = Maybe.map .costModels model.protocolParams
                            , walletSignTx =
                                \tx ->
                                    case model.wallet of
                                        Nothing ->
                                            Cmd.none

                                        Just wallet ->
                                            toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = True } tx))
                            }

                        ( newPageModel, cmds, msgToParent ) =
                            Page.Preparation.update ctx pageMsg pageModel
                    in
                    ( updateModelWithPrepToParentMsg msgToParent { model | page = PreparationPage newPageModel }
                    , cmds
                    )

                _ ->
                    ( model, Cmd.none )

        ( GotRationaleAsFile file, { page } ) ->
            case page of
                PreparationPage pageModel ->
                    Page.Preparation.pinRationaleFile file pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = PreparationPage newPageModel })
                        |> Tuple.mapSecond (Cmd.map PreparationPageMsg)

                _ ->
                    ( model, Cmd.none )

        ( SigningPageMsg pageMsg, { page } ) ->
            case page of
                SigningPage pageModel ->
                    let
                        ( walletSignTx, walletSubmitTx ) =
                            case model.wallet of
                                Nothing ->
                                    ( \_ -> Cmd.none
                                    , \_ -> Cmd.none
                                    )

                                Just wallet ->
                                    ( \tx -> toWallet (Cip30.encodeRequest (Cip30.signTx wallet { partialSign = True } tx))
                                    , \tx -> toWallet (Cip30.encodeRequest (Cip30.submitTx wallet tx))
                                    )

                        ctx =
                            { wrapMsg = SigningPageMsg
                            , wallet = model.wallet
                            , walletSignTx = walletSignTx
                            , walletSubmitTx = walletSubmitTx
                            }
                    in
                    Page.Signing.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = SigningPage newPageModel })

                _ ->
                    ( model, Cmd.none )

        ( MultisigPageMsg pageMsg, { page } ) ->
            case page of
                MultisigRegistrationPage pageModel ->
                    let
                        ctx =
                            { wrapMsg = MultisigPageMsg
                            , wallet =
                                case ( model.wallet, model.walletChangeAddress, model.walletUtxos ) of
                                    ( Just wallet, Just address, Just utxos ) ->
                                        Just { wallet = wallet, changeAddress = address, utxos = utxos }

                                    _ ->
                                        Nothing
                            , costModels = Maybe.map .costModels model.protocolParams
                            }
                    in
                    Page.MultisigRegistration.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = MultisigRegistrationPage newPageModel })

                _ ->
                    ( model, Cmd.none )

        ( PdfPageMsg pageMsg, { page } ) ->
            case page of
                PdfPage pageModel ->
                    let
                        ctx =
                            { wrapMsg = PdfPageMsg
                            }
                    in
                    Page.Pdf.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = PdfPage newPageModel })

                _ ->
                    ( model, Cmd.none )

        -- Result Http.Error (List Page.Preparation.ActiveProposal)
        ( GotProposals result, _ ) ->
            case result of
                Err httpError ->
                    ( { model | proposals = RemoteData.Failure httpError }
                    , Cmd.none
                    )

                Ok activeProposals ->
                    let
                        proposalsList =
                            List.map (\p -> ( Gov.actionIdToString p.id, p )) activeProposals

                        metadataRequest ( id, p ) =
                            Api.defaultApiProvider.loadProposalMetadata p.metadataUrl (GotProposalMetadata id)
                    in
                    ( { model | proposals = RemoteData.Success <| Dict.fromList proposalsList }
                      -- Load proposals metadata
                    , Cmd.batch <| List.map metadataRequest proposalsList
                    )

        -- Result String ProposalMetadata
        ( GotProposalMetadata id result, _ ) ->
            let
                updateMetadata maybeProposal =
                    case ( maybeProposal, result ) of
                        ( Nothing, _ ) ->
                            Nothing

                        ( Just p, Ok metadata ) ->
                            Just { p | metadata = RemoteData.Success metadata }

                        ( Just p, Err error ) ->
                            Just { p | metadata = RemoteData.Failure error }
            in
            ( { model | proposals = RemoteData.map (\ps -> Dict.update id updateMetadata ps) model.proposals }
            , Cmd.none
            )


handleUrlChange : Route -> Model -> ( Model, Cmd Msg )
handleUrlChange route model =
    case route of
        Route404 ->
            Debug.todo "Handle 404 page"

        RouteLanding ->
            ( { model
                | errors = []
                , page = LandingPage
              }
            , pushUrl <| AppUrl.toString <| routeToAppUrl route
            )

        RoutePreparation ->
            let
                newModel =
                    { model | errors = [], page = PreparationPage Page.Preparation.init }

                updateUrlCmd =
                    pushUrl <| AppUrl.toString <| routeToAppUrl route
            in
            if RemoteData.isSuccess model.proposals then
                ( newModel, updateUrlCmd )

            else
                ( { newModel | proposals = RemoteData.Loading }
                , Cmd.batch
                    [ updateUrlCmd
                    , Api.defaultApiProvider.loadGovProposals GotProposals
                    ]
                )

        RouteSigning { expectedSigners, tx } ->
            ( { model
                | errors = []
                , page = SigningPage <| Page.Signing.initialModel expectedSigners tx
              }
            , pushUrl <| AppUrl.toString <| routeToAppUrl route
            )

        RouteMultisigRegistration ->
            ( { model
                | errors = []
                , page = MultisigRegistrationPage Page.MultisigRegistration.initialModel
              }
            , pushUrl <| AppUrl.toString <| routeToAppUrl route
            )

        RoutePdf ->
            ( { model
                | errors = []
                , page = PdfPage Page.Pdf.initialModel
              }
            , pushUrl <| AppUrl.toString <| routeToAppUrl route
            )


handleWalletResponse : Cip30.Response -> Model -> ( Model, Cmd Msg )
handleWalletResponse response model =
    case response of
        -- We just discovered available wallets
        Cip30.AvailableWallets wallets ->
            ( { model | walletsDiscovered = wallets }
            , Cmd.none
            )

        -- We just connected to the wallet, let’s ask for all that is still missing
        Cip30.EnabledWallet wallet ->
            ( { model | wallet = Just wallet }
            , Cmd.batch
                -- Retrieve the wallet change address
                [ toWallet (Cip30.encodeRequest (Cip30.getChangeAddress wallet))

                -- Retrieve UTXOs from the main wallet
                , Cip30.getUtxos wallet { amount = Nothing, paginate = Nothing }
                    |> Cip30.encodeRequest
                    |> toWallet
                ]
            )

        -- Received the wallet change address
        Cip30.ApiResponse _ (Cip30.ChangeAddress address) ->
            ( { model | walletChangeAddress = Just address }
            , Cmd.none
            )

        -- We just received the utxos
        Cip30.ApiResponse _ (Cip30.WalletUtxos utxos) ->
            ( { model | walletUtxos = Just (Utxo.refDictFromList utxos) }
            , Cmd.none
            )

        -- The wallet just signed a Tx
        Cip30.ApiResponse _ (Cip30.SignedTx vkeyWitnesses) ->
            case model.page of
                SigningPage pageModel ->
                    ( { model | page = SigningPage <| Page.Signing.addWalletSignatures vkeyWitnesses pageModel }
                    , Cmd.none
                    )

                -- No other page expects to receive a Tx signature
                _ ->
                    ( model, Cmd.none )

        -- The wallet just submitted a Tx
        Cip30.ApiResponse _ (Cip30.SubmittedTx txId) ->
            case model.page of
                SigningPage pageModel ->
                    ( { model | page = SigningPage <| Page.Signing.recordSubmittedTx txId pageModel }
                    , Cmd.none
                    )

                -- No other page expects to submit a Tx
                _ ->
                    ( model, Cmd.none )

        Cip30.ApiResponse _ _ ->
            ( { model | errors = "TODO: unhandled CIP30 response yet" :: model.errors }
            , Cmd.none
            )

        -- Received an error message from the wallet
        Cip30.ApiError { info } ->
            ( { model
                -- TODO: ideally, each port to wallet should know
                -- how to redirect to a new message in fromWallet.
                -- That would need a bit of thinking to figure out a good way.
                -- For now, I mostly observed errors at Tx signing/submission
                | page = resetSigningStep info model.page
              }
            , Cmd.none
            )

        -- Unknown type of message received from the wallet
        Cip30.UnhandledResponseType error ->
            ( { model | errors = error :: model.errors }
            , Cmd.none
            )


updateModelWithPrepToParentMsg : Maybe Page.Preparation.MsgToParent -> Model -> Model
updateModelWithPrepToParentMsg msgToParent model =
    case msgToParent of
        Nothing ->
            model

        Just (Page.Preparation.CacheScriptInfo scriptInfo) ->
            { model | scriptsInfo = Dict.insert (Bytes.toHex scriptInfo.scriptHash) scriptInfo model.scriptsInfo }

        Just (Page.Preparation.CacheDrepInfo drepInfo) ->
            { model | drepsInfo = Dict.insert (Bytes.toHex <| Address.extractCredentialHash drepInfo.credential) drepInfo model.drepsInfo }


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SigningPage pageModel ->
            SigningPage <| Page.Signing.resetSubmission error pageModel

        _ ->
            page



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewContent model
        , viewErrors model.errors
        , Html.hr [] []
        , Html.p [] [ text "Built with <3 by the CF, using elm-cardano, data provided by Koios" ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    div []
        [ Html.h1 [] [ text "Cardano Governance Voting" ]
        , viewWalletSection model
        ]


viewWalletSection : Model -> Html Msg
viewWalletSection model =
    case model.wallet of
        Nothing ->
            viewAvailableWallets model.walletsDiscovered

        Just wallet ->
            viewConnectedWallet wallet model.walletChangeAddress


viewConnectedWallet : Cip30.Wallet -> Maybe Address -> Html Msg
viewConnectedWallet wallet maybeChangeAddress =
    div []
        [ text <| "Connected Wallet: " ++ (Cip30.walletDescriptor wallet).name
        , case maybeChangeAddress of
            Just addr ->
                text <| " (" ++ prettyAddr addr ++ ") "

            Nothing ->
                text " "
        , button [ onClick DisconnectWalletButtonClicked ] [ text "Disconnect" ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    case model.page of
        LandingPage ->
            viewLandingPage

        PreparationPage prepModel ->
            Page.Preparation.view
                { wrapMsg = PreparationPageMsg
                , walletChangeAddress = model.walletChangeAddress
                , proposals = model.proposals
                , jsonLdContexts = model.jsonLdContexts
                , costModels = Maybe.map .costModels model.protocolParams
                , signingLink =
                    \tx expectedSigners ->
                        link (RouteSigning { tx = Just tx, expectedSigners = expectedSigners }) []
                }
                prepModel

        SigningPage signingModel ->
            Page.Signing.view
                { wrapMsg = SigningPageMsg
                , wallet = model.wallet
                }
                signingModel

        MultisigRegistrationPage pageModel ->
            Page.MultisigRegistration.view
                { wrapMsg = MultisigPageMsg
                , wallet = model.wallet
                , signingLink =
                    \tx expectedSigners ->
                        link (RouteSigning { tx = Just tx, expectedSigners = expectedSigners }) []
                }
                pageModel

        PdfPage pageModel ->
            Page.Pdf.view
                { wrapMsg = PdfPageMsg
                }
                pageModel


viewLandingPage : Html Msg
viewLandingPage =
    div []
        [ Html.h2 [] [ text "Welcome to the Voting App" ]
        , Html.p [] [ link RoutePreparation [] [ text "Start vote preparation" ] ]
        , Html.p [] [ link (RouteSigning { expectedSigners = [], tx = Nothing }) [] [ text "Sign an already prepared Tx" ] ]
        , Html.p [] [ link RouteMultisigRegistration [] [ text "Register as a Multisig DRep" ] ]
        , Html.p [] [ link RoutePdf [] [ text "Generate PDFs for governance metadata" ] ]
        ]



-- Helpers


viewErrors : List String -> Html Msg
viewErrors errors =
    if List.isEmpty errors then
        text ""

    else
        div [ HA.class "errors" ]
            [ Html.h3 [] [ text "Errors" ]
            , Html.ul [] (List.map (\err -> Html.li [] [ Html.pre [] [ text err ] ]) errors)
            ]


viewAvailableWallets : List Cip30.WalletDescriptor -> Html Msg
viewAvailableWallets wallets =
    if List.isEmpty wallets then
        Html.p [] [ text "It seems like you don’t have any CIP30 wallet?" ]

    else
        let
            walletDescription : Cip30.WalletDescriptor -> String
            walletDescription w =
                "id: " ++ w.id ++ ", name: " ++ w.name

            walletIcon : Cip30.WalletDescriptor -> Html Msg
            walletIcon { icon } =
                Html.img [ src icon, height 32 ] []

            connectButton { id } =
                Html.button [ onClick (ConnectButtonClicked { id = id }) ] [ text "connect" ]

            walletRow w =
                div [] [ walletIcon w, text (walletDescription w), text " ", connectButton w ]
        in
        div [] (List.map walletRow wallets)
