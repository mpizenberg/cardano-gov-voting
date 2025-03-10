port module Main exposing (Model, Msg(..), Page, Route(..), TaskCompleted, main)

{-| Main application module for the Cardano Governance Voting web app.


# Architecture Overview

This app follows a single-page application (SPA) architecture with client-side routing.
The main components are:

  - Wallet integration via CIP-30 ports for connecting to Cardano wallets
  - Multiple pages for different governance actions (vote preparation, signing, DRep registration)
  - Concurrent task handling for asynchronous operations
  - In-browser caching of proposal metadata
  - PDF generation capabilities


# Key Design Decisions

  - Uses ports for all wallet interactions to maintain type safety while talking to JS
  - Implements client-side routing to enable sharing direct links to specific actions
  - Caches proposal metadata locally to improve performance and reduce API load
  - Uses a "task port"-like pattern from elm-concurrent-task for advanced task composition
  - Maintains separation between pages while sharing common wallet state


# Data Flow

1.  App initializes with protocol parameters and wallet discovery
2.  User connects wallet to enable transaction signing
3.  Pages can request wallet operations via ports
4.  Task system handles advanced operations like metadata fetching with caching


# State Management

The main model contains:

  - Global application state (wallet connection, protocol params)
  - Page-specific state
  - Cached governance data (proposals, DReps, etc)
  - Task pool for elm-concurrent-task operations
  - Error tracking

Each page maintains its own state and can communicate up through MsgToParent patterns.

-}

import Api exposing (ActiveProposal, CcInfo, DrepInfo, PoolInfo, ProtocolParams)
import AppUrl exposing (AppUrl)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address, CredentialHash)
import Cardano.Cip30 as Cip30 exposing (WalletDescriptor)
import Cardano.Gov as Gov
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import Dict exposing (Dict)
import Footer
import Helper
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (preventDefaultOn)
import Http
import Json.Decode as JD exposing (Decoder, Value)
import Navigation
import Page.Disclaimer
import Page.MultisigRegistration
import Page.Pdf
import Page.Preparation exposing (JsonLdContexts)
import Page.Signing
import Platform.Cmd as Cmd
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (WebData)
import ScriptInfo exposing (ScriptInfo)
import Storage
import Url


main : Program { url : String, jsonLdContexts : JsonLdContexts, db : Value } Model Msg
main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ fromWallet WalletMsg
                    , onUrlChange (locationHrefToRoute >> UrlChanged)
                    , gotRationaleAsFile GotRationaleAsFile
                    , ConcurrentTask.onProgress
                        { send = sendTask
                        , receive = receiveTask
                        , onProgress = OnTaskProgress
                        }
                        model.taskPool
                    ]
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg


port jsonRationaleToFile : { fileContent : String, fileName : String } -> Cmd msg


port gotRationaleAsFile : (Value -> msg) -> Sub msg



-- Task port thingy


port sendTask : Value -> Cmd msg


port receiveTask : (Value -> msg) -> Sub msg



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
    , ccsInfo : Dict String CcInfo
    , poolsInfo : Dict String PoolInfo
    , jsonLdContexts : JsonLdContexts
    , taskPool : ConcurrentTask.Pool Msg String TaskCompleted
    , db : Value
    , errors : List String
    , navigationState : Navigation.NavState
    }


type Page
    = LandingPage
    | PreparationPage Page.Preparation.Model
    | SigningPage Page.Signing.Model
    | MultisigRegistrationPage Page.MultisigRegistration.Model
    | PdfPage Page.Pdf.Model
    | DisclaimerPage


type TaskCompleted
    = GotProposalMetadataTask String (Result String ProposalMetadata)
    | PreparationTaskCompleted Page.Preparation.TaskCompleted


init : { url : String, jsonLdContexts : JsonLdContexts, db : Value } -> ( Model, Cmd Msg )
init { url, jsonLdContexts, db } =
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
        , ccsInfo = Dict.empty
        , poolsInfo = Dict.empty
        , jsonLdContexts = jsonLdContexts
        , taskPool = ConcurrentTask.pool
        , db = db
        , errors = []
        , navigationState = Navigation.init
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
    | GotProtocolParams (Result Http.Error ProtocolParams)
    | GotProposals (Result Http.Error (List ActiveProposal))
      -- Preparation page
    | PreparationPageMsg Page.Preparation.Msg
    | GotRationaleAsFile Value
      -- Signing page
    | SigningPageMsg Page.Signing.Msg
      -- Multisig DRep registration page
    | MultisigPageMsg Page.MultisigRegistration.Msg
      -- PDF page
    | PdfPageMsg Page.Pdf.Msg
      -- Navigation
    | NavigationMsg Navigation.Msg
      -- Task port
    | OnTaskProgress ( ConcurrentTask.Pool Msg String TaskCompleted, Cmd Msg )
    | OnTaskComplete (ConcurrentTask.Response String TaskCompleted)


type Route
    = RouteLanding
    | RoutePreparation
    | RouteSigning { expectedSigners : List (Bytes CredentialHash), tx : Maybe Transaction }
    | RouteMultisigRegistration
    | RoutePdf
    | RouteDisclaimer
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
                            , db = model.db
                            , proposals = model.proposals
                            , scriptsInfo = model.scriptsInfo
                            , drepsInfo = model.drepsInfo
                            , ccsInfo = model.ccsInfo
                            , poolsInfo = model.poolsInfo
                            , loadedWallet = loadedWallet
                            , feeProviderAskUtxosCmd = Cmd.none -- TODO
                            , jsonLdContexts = model.jsonLdContexts
                            , jsonRationaleToFile = jsonRationaleToFile
                            , costModels = Maybe.map .costModels model.protocolParams
                            }

                        ( newPageModel, cmds, msgToParent ) =
                            Page.Preparation.update ctx pageMsg pageModel
                    in
                    updateModelWithPrepToParentMsg msgToParent { model | page = PreparationPage newPageModel }
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, cmds ])

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

                        completeReadProposalMetadataTask : ActiveProposal -> ConcurrentTask x TaskCompleted
                        completeReadProposalMetadataTask { id, metadataHash, metadataUrl } =
                            Api.defaultApiProvider.loadProposalMetadata metadataUrl
                                |> Storage.cacheWrap
                                    { db = model.db, storeName = "proposalMetadata" }
                                    ProposalMetadata.decoder
                                    ProposalMetadata.encode
                                    { key = metadataHash }
                                |> ConcurrentTask.Extra.toResult
                                |> ConcurrentTask.map (GotProposalMetadataTask <| Gov.actionIdToString id)

                        ( newPool, cmds ) =
                            ConcurrentTask.Extra.attemptEach { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                                (List.map completeReadProposalMetadataTask activeProposals)
                    in
                    ( { model
                        | taskPool = newPool
                        , proposals = RemoteData.Success <| Dict.fromList proposalsList
                      }
                    , Cmd.batch cmds
                    )

        -- Task port thingy
        ( OnTaskProgress ( taskPool, cmd ), _ ) ->
            ( { model | taskPool = taskPool }, cmd )

        ( OnTaskComplete taskCompleted, _ ) ->
            handleCompletedTask taskCompleted model

        ( NavigationMsg navMsg, _ ) ->
            case navMsg of
                Navigation.ConnectWalletClicked { id } ->
                    -- Reuse your existing wallet connection code
                    ( model, toWallet (Cip30.encodeRequest (Cip30.enableWallet { id = id, extensions = [] })) )

                Navigation.DisconnectWalletClicked ->
                    -- Reuse your existing disconnect code
                    ( { model | wallet = Nothing, walletChangeAddress = Nothing, walletUtxos = Nothing }
                    , Cmd.none
                    )

                _ ->
                    -- Handle other navigation messages
                    let
                        ( newNavState, navCmd ) =
                            Navigation.update navMsg model.navigationState
                    in
                    ( { model | navigationState = newNavState }
                    , Cmd.map NavigationMsg navCmd
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

        RouteDisclaimer ->
            ( { model
                | errors = []
                , page = DisclaimerPage
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


updateModelWithPrepToParentMsg : Maybe Page.Preparation.MsgToParent -> Model -> ( Model, Cmd Msg )
updateModelWithPrepToParentMsg msgToParent model =
    case msgToParent of
        Nothing ->
            ( model, Cmd.none )

        Just (Page.Preparation.CacheScriptInfo scriptInfo) ->
            ( { model | scriptsInfo = Dict.insert (Bytes.toHex scriptInfo.scriptHash) scriptInfo model.scriptsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.CacheDrepInfo drepInfo) ->
            ( { model | drepsInfo = Dict.insert (Bytes.toHex <| Address.extractCredentialHash drepInfo.credential) drepInfo model.drepsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.CacheCcInfo ccInfo) ->
            ( { model | ccsInfo = Dict.insert (Bytes.toHex <| Address.extractCredentialHash ccInfo.hotCred) ccInfo model.ccsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.CachePoolInfo poolInfo) ->
            ( { model | poolsInfo = Dict.insert (Bytes.toHex poolInfo.pool) poolInfo model.poolsInfo }
            , Cmd.none
            )

        Just (Page.Preparation.RunTask task) ->
            ConcurrentTask.attempt { pool = model.taskPool, send = sendTask, onComplete = OnTaskComplete }
                (ConcurrentTask.map PreparationTaskCompleted task)
                |> Tuple.mapFirst (\newTaskPool -> { model | taskPool = newTaskPool })


{-| Helper function to reset the signing step of the Preparation.
-}
resetSigningStep : String -> Page -> Page
resetSigningStep error page =
    case page of
        SigningPage pageModel ->
            SigningPage <| Page.Signing.resetSubmission error pageModel

        _ ->
            page


handleCompletedTask : ConcurrentTask.Response String TaskCompleted -> Model -> ( Model, Cmd Msg )
handleCompletedTask response model =
    case ( response, model.page ) of
        ( ConcurrentTask.Error error, _ ) ->
            ( { model | errors = error :: model.errors }, Cmd.none )

        ( ConcurrentTask.UnexpectedError error, _ ) ->
            ( { model | errors = Debug.toString error :: model.errors }, Cmd.none )

        ( ConcurrentTask.Success (GotProposalMetadataTask id result), _ ) ->
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

        ( ConcurrentTask.Success (PreparationTaskCompleted taskCompleted), PreparationPage pageModel ) ->
            let
                ( newPageModel, cmds, msgToParent ) =
                    Page.Preparation.handleTaskCompleted taskCompleted pageModel
            in
            updateModelWithPrepToParentMsg msgToParent { model | page = PreparationPage newPageModel }
                |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Cmd.map PreparationPageMsg cmds ])

        ( ConcurrentTask.Success (PreparationTaskCompleted _), _ ) ->
            ( model, Cmd.none )



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    let
        backgroundStyle =
            if model.page == LandingPage then
                -- Use transparent background on landing page to show gradients
                HA.style "background" "transparent"

            else
                -- Use gray background on other pages
                HA.style "background" "#d9d9d9"
    in
    div
        [ HA.style "min-height" "100vh"
        , HA.style "position" "relative"
        , HA.style "padding-bottom" "100px"
        , backgroundStyle -- Apply the conditional background
        ]
        [ -- Gradient circles (only on landing page)
          if model.page == LandingPage then
            viewGradientBackgrounds

          else
            text ""
        , viewHeader model
        , viewContent model
        , viewErrors model.errors
        , Footer.view
            { copyright = "© 2025 Cardano Stiftung"
            , disclaimerLink = "/page/disclaimer"
            , githubLink = "https://github.com/mpizenberg/cardano-gov-voting"
            }
        ]



-- Separate function for gradient backgrounds


viewGradientBackgrounds : Html Msg
viewGradientBackgrounds =
    div
        [ HA.style "position" "absolute"
        , HA.style "right" "0"
        , HA.style "top" "0"
        , HA.style "z-index" "-1"
        ]
        [ -- Blue gradient circle
          div
            [ HA.style "width" "75vw"
            , HA.style "height" "75vh"
            , HA.style "border-radius" "75rem"
            , HA.style "background" "linear-gradient(270deg, #00e0ff, #0084ff 100%)"
            , HA.style "filter" "blur(128px)"
            , HA.style "transform-origin" "center center"
            , HA.style "position" "absolute"
            , HA.style "right" "-7.5vw"
            , HA.style "top" "-30vh"
            , HA.class "blue-gradient-animation"
            ]
            []

        -- Red-Yellow gradient circle
        , div
            [ HA.style "width" "22rem"
            , HA.style "height" "22rem"
            , HA.style "border-radius" "22rem"
            , HA.style "background" "linear-gradient(90deg, #d1085c -0.01%, #ffad0f 55.09%)"
            , HA.style "filter" "blur(4rem)"
            , HA.style "transform-origin" "center center"
            , HA.style "position" "absolute"
            , HA.style "right" "0vh"
            , HA.style "top" "5vh"
            , HA.class "red-yellow-gradient-animation"
            ]
            []
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Navigation.view
        { state = model.navigationState
        , toMsg = NavigationMsg
        , brand = ""
        , items =
            [ { label = "Home", url = AppUrl.toString <| routeToAppUrl RouteLanding, isActive = model.page == LandingPage }
            , { label = "Vote Preparation"
              , url = AppUrl.toString <| routeToAppUrl RoutePreparation
              , isActive =
                    case model.page of
                        PreparationPage _ ->
                            True

                        _ ->
                            False
              }

            -- Remove Multisig registration for now
            -- , { label = "Multisig Registration"
            --   , url = AppUrl.toString <| routeToAppUrl RouteMultisigRegistration
            --   , isActive =
            --         case model.page of
            --             MultisigRegistrationPage _ ->
            --                 True
            --             _ ->
            --                 False
            --   }
            , { label = "PDFs"
              , url = AppUrl.toString <| routeToAppUrl RoutePdf
              , isActive =
                    case model.page of
                        PdfPage _ ->
                            True

                        _ ->
                            False
              }
            ]
        , wallet = model.wallet
        , walletsDiscovered = model.walletsDiscovered
        , walletChangeAddress = model.walletChangeAddress
        }


viewContent : Model -> Html Msg
viewContent model =
    case model.page of
        LandingPage ->
            viewLandingPage

        DisclaimerPage ->
            Page.Disclaimer.view

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
    div [ HA.class "container mx-auto px-4" ]
        [ Html.h2
            [ HA.style "font-size" "2.5rem"
            , HA.style "font-size" "min(5.5rem, 8vw)"
            , HA.style "line-height" "1.2"
            , HA.style "max-width" "50rem"
            , HA.style "margin-top" "3rem"
            , HA.style "margin-bottom" "0.5rem"
            ]
            [ text "Welcome to Cardano Governance Voting" ]
        , Html.p
            [ HA.style "font-size" "1rem"
            , HA.style "font-size" "min(1.3rem, 4.5vw)"
            , HA.style "max-width" "45rem"
            , HA.class "mb-2"
            ]
            [ text "Leverage blockchain to build future-proof solutions. This page aims to help generate pretty PDFs for different kinds of governance metadata JSON files."
            ]
        , Html.p [ HA.class "mt-4" ]
            [ link RoutePreparation
                [ HA.class "inline-block" ]
                [ Helper.viewButton "Get Started" NoMsg ]
            ]
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
