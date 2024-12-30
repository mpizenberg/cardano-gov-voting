port module Main exposing (main)

import Blake2b exposing (blake2b224)
import Browser
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CertificateIntent(..), CredentialWitness(..), Fee(..), ScriptWitness(..), SpendSource(..), TxIntent(..), VoterWitness(..), WitnessSource(..), dummyBytes)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..), StakeCredential(..))
import Cardano.Cip30 as Cip30 exposing (WalletDescriptor)
import Cardano.CoinSelection as CoinSelection
import Cardano.Data as Data
import Cardano.Gov as Gov exposing (ActionId, CostModels, Drep(..), Vote(..), Voter)
import Cardano.Script as Script exposing (NativeScript(..), PlutusVersion(..), ScriptCbor)
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (DatumOption(..), Output, OutputReference, TransactionId)
import Cardano.Value
import Cbor.Encode
import Dict exposing (Dict)
import Hex.Convert
import Html exposing (Html, button, div, text, wbr)
import Html.Attributes as HA exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Integer
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Natural exposing (Natural)
import Page.Preparation exposing (ActiveProposal, JsonLdContexts)
import Platform.Cmd as Cmd
import RemoteData exposing (WebData)


main =
    -- The main entry point of our app
    -- More info about that in the Browser package docs:
    -- https://package.elm-lang.org/packages/elm/browser/latest/
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.batch [ fromWallet WalletMsg ]
        , view = view
        }


port toWallet : Value -> Cmd msg


port fromWallet : (Value -> msg) -> Sub msg


port toExternalApp : Value -> Cmd msg


port fromExternalApp : (Value -> msg) -> Sub msg



-- #########################################################
-- MODEL
-- #########################################################
-- SigningModel


type SigningModel
    = SigningLandingPage { errors : String }
    | SigningTx Transaction
    | TxSigned Transaction



-- SubmissionModel


type SubmissionModel
    = SubmissionLandingPage
    | TxToSubmitLoaded Transaction (List VKeyWitness)
    | SubmittingTx Transaction
    | VoteTxSubmitted { txId : Bytes TransactionId }


type alias Model =
    { page : Page
    , walletsDiscovered : List WalletDescriptor
    , wallet : Maybe Cip30.Wallet
    , walletChangeAddress : Maybe Address
    , walletUtxos : Maybe (Utxo.RefDict Output)
    , protocolParams : Maybe ProtocolParams
    , proposals : WebData (Dict String ActiveProposal)
    , jsonLdContexts : JsonLdContexts
    , errors : List String
    }


type Page
    = LandingPage
    | PreparationPage Page.Preparation.Model
    | SigningPage SigningModel
    | SubmissionPage SubmissionModel


type alias ProtocolParams =
    { costModels : CostModels
    , drepDeposit : Natural
    }


init : { jsonLdContexts : JsonLdContexts } -> ( Model, Cmd Msg )
init { jsonLdContexts } =
    ( { page = LandingPage
      , walletsDiscovered = []
      , wallet = Nothing
      , walletUtxos = Nothing
      , walletChangeAddress = Nothing
      , protocolParams = Nothing
      , proposals = RemoteData.NotAsked
      , jsonLdContexts = jsonLdContexts
      , errors = []
      }
    , Cmd.batch
        [ toWallet (Cip30.encodeRequest Cip30.discoverWallets)
        , loadProtocolParams
        ]
    )


loadProtocolParams : Cmd Msg
loadProtocolParams =
    Http.post
        { url = "https://preview.koios.rest/api/v1/ogmios"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "jsonrpc", JE.string "2.0" )
                    , ( "method", JE.string "queryLedgerState/protocolParameters" )
                    ]
                )
        , expect = Http.expectJson GotProtocolParams protocolParamsDecoder
        }


loadGovernanceProposals : Cmd Msg
loadGovernanceProposals =
    Http.post
        { url = "https://preview.koios.rest/api/v1/ogmios"
        , body =
            Http.jsonBody
                (JE.object
                    [ ( "jsonrpc", JE.string "2.0" )
                    , ( "method", JE.string "queryLedgerState/governanceProposals" )
                    ]
                )
        , expect = Http.expectJson GotProposals proposalsDecoder
        }


proposalsDecoder : Decoder (List ActiveProposal)
proposalsDecoder =
    JD.field "result" <|
        JD.list <|
            JD.map3 ActiveProposal
                (JD.map2
                    (\id index ->
                        { transactionId = Bytes.fromHexUnchecked id
                        , govActionIndex = index
                        }
                    )
                    (JD.at [ "proposal", "transaction", "id" ] JD.string)
                    (JD.at [ "proposal", "index" ] JD.int)
                )
                (JD.at [ "action", "type" ] JD.string)
                (JD.succeed RemoteData.Loading)



-- Helper


encodeCborHex : Cbor.Encode.Encoder -> Value
encodeCborHex cborEncoder =
    Cbor.Encode.encode cborEncoder
        |> Hex.Convert.toString
        |> JE.string



-- #########################################################
-- UPDATE
-- #########################################################


type Msg
    = WalletMsg Value
    | ConnectButtonClicked { id : String }
    | DisconnectWalletButtonClicked
    | GotProtocolParams (Result Http.Error ProtocolParams)
    | GotProposals (Result Http.Error (List Page.Preparation.ActiveProposal))
    | StartPreparation
      -- Preparation page
    | PreparationPageMsg Page.Preparation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
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

        ( StartPreparation, { protocolParams } ) ->
            -- ( { model
            --     | errors = []
            --     , page = PreparationPage Page.Preparation.init
            --   }
            -- , Cmd.none
            -- )
            case protocolParams of
                Just _ ->
                    ( { model
                        | errors = []
                        , page = PreparationPage Page.Preparation.init
                        , proposals = RemoteData.Loading
                      }
                    , loadGovernanceProposals
                    )

                Nothing ->
                    ( { model | errors = "Protocol parameters not loaded yet" :: model.errors }
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
                            , loadedWallet = loadedWallet
                            , feeProviderAskUtxosCmd = Cmd.none -- TODO
                            , jsonLdContexts = model.jsonLdContexts
                            }
                    in
                    Page.Preparation.update ctx pageMsg pageModel
                        |> Tuple.mapFirst (\newPageModel -> { model | page = PreparationPage newPageModel })

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
                        proposalsDict =
                            activeProposals
                                |> List.map (\p -> ( Gov.actionIdToString p.id, p ))
                                |> Dict.fromList
                    in
                    ( { model | proposals = RemoteData.Success proposalsDict }
                      -- TODO: load proposals metadata
                    , Cmd.none
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

        -- TODO
        Cip30.ApiResponse _ _ ->
            ( { model | errors = "TODO: unhandled CIP30 response yet" :: model.errors }
            , Cmd.none
            )

        -- Received an error message from the wallet
        Cip30.ApiError { info } ->
            ( { model | errors = info :: model.errors }
            , Cmd.none
            )

        -- Unknown type of message received from the wallet
        Cip30.UnhandledResponseType error ->
            ( { model | errors = error :: model.errors }
            , Cmd.none
            )


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map4
        (\v1 v2 v3 drepDeposit ->
            { costModels = CostModels (Just v1) (Just v2) (Just v3)
            , drepDeposit = drepDeposit
            }
        )
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
        (JD.at [ "result", "delegateRepresentativeDeposit", "ada", "lovelace" ] <| JD.map Natural.fromSafeInt JD.int)


stringToVote : String -> Vote
stringToVote str =
    -- Helper function to convert String to Vote
    case str of
        "yes" ->
            VoteYes

        "no" ->
            VoteNo

        _ ->
            VoteAbstain


voteToString : Vote -> String
voteToString vote =
    case vote of
        VoteYes ->
            "yes"

        VoteNo ->
            "no"

        VoteAbstain ->
            "abstain"



-- #########################################################
-- VIEW
-- #########################################################


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , viewContent model
        , viewErrors model.errors
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
                text <| " (" ++ prettyAddr addr ++ ")"

            Nothing ->
                text ""
        , button [ onClick DisconnectWalletButtonClicked ] [ text "Disconnect" ]
        ]


prettyAddr : Address -> String
prettyAddr address =
    let
        addrHex =
            Bytes.toHex (Address.toBytes address)
    in
    String.slice 0 8 addrHex
        ++ "..."
        ++ String.slice -8 (String.length addrHex) addrHex


viewContent : Model -> Html Msg
viewContent model =
    case model.page of
        LandingPage ->
            viewLandingPage model.walletsDiscovered

        PreparationPage prepModel ->
            Page.Preparation.view
                { wrapMsg = PreparationPageMsg
                , proposals = model.proposals
                }
                prepModel

        SigningPage signingModel ->
            viewSigningPage signingModel

        SubmissionPage submissionModel ->
            viewSubmissionPage submissionModel


viewLandingPage : List WalletDescriptor -> Html Msg
viewLandingPage wallets =
    div []
        [ Html.h2 [] [ text "Welcome to the Voting App" ]
        , div []
            [ button
                [ onClick StartPreparation ]
                [ text "Start Vote Preparation" ]
            ]
        ]



-- Signing Page


viewSigningPage : SigningModel -> Html Msg
viewSigningPage signingModel =
    Debug.todo "viewSigningPage"



-- Submission Page


viewSubmissionPage : SubmissionModel -> Html Msg
viewSubmissionPage submissionModel =
    Debug.todo "viewSubmissionPage"



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
                div [] [ walletIcon w, text (walletDescription w), connectButton w ]
        in
        div [] (List.map walletRow wallets)
