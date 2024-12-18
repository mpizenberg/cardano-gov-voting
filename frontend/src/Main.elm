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
import Dict.Any
import Hex.Convert
import Html exposing (Html, button, div, text, wbr)
import Html.Attributes as HA exposing (height, src)
import Html.Events exposing (onClick)
import Http
import Integer
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE
import Natural exposing (Natural)


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
-- Voter Preparation


type alias VoterPreparationForm =
    { voterType : VoterType
    , voterCred : VoterCredForm
    , feeProviderType : FeeProviderType
    }


type VoterType
    = CcVoter
    | DrepVoter
    | SpoVoter


type VoterCredForm
    = StakeKeyVoter String
    | ScriptVoter { scriptHash : String, utxoRef : String }


type FeeProviderType
    = ConnectedWalletFeeProvider
    | ExternalFeeProvider { endpoint : String }


type alias VoterIdentified =
    { voterType : VoterType
    , voterCred : CredentialWitness
    , feeProvider : FeeProvider
    }



-- Picking Gov Action


type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    , metadata : RemoteData ProposalMetadata
    }


type alias ProposalMetadata =
    { title : String
    , abstract : String
    , rawJson : String
    }


type RemoteData a
    = Loading
    | Retrieved a
    | Error String



-- Filling Rationale


type alias RationaleForm =
    { authors : List AuthorForm
    , summary : MarkdownForm
    , rationaleStatement : MarkdownForm
    , precedentDiscussion : MarkdownForm
    , counterargumentDiscussion : MarkdownForm
    , conclusion : MarkdownForm
    , internalVote : InternalVote
    , references : ReferencesForm
    }


type alias AuthorForm =
    {}


type alias MarkdownForm =
    {}


type alias InternalVote =
    { constitutional : Int
    , unconstitutional : Int
    , abstain : Int
    , didNotVote : Int
    }


type alias ReferencesForm =
    {}


type alias Rationale =
    { authors : Dict String (Maybe AuthorWitness)
    , summary : String
    , rationaleStatement : String
    , precedentDiscussion : String
    , counterargumentDiscussion : String
    , conclusion : String
    , internalVote : InternalVote
    , references : List Reference
    }


type alias AuthorWitness =
    {}


type alias Reference =
    {}



-- Preparation Model


type Step prep done
    = NotDone prep
    | Done done


type alias PreparationModel =
    { proposals : List ActiveProposal
    , voterStep : Step VoterPreparationForm VoterIdentified
    , pickProposalStep : Step {} ActiveProposal
    , rationaleCreationStep : Step RationaleForm Rationale
    , rationaleSignatureStep : Step (Dict String (Maybe AuthorWitness)) (Dict String AuthorWitness)
    , permanentStorageStep : Step StoragePrep Storage
    , buildTxStep : Step {} Transaction
    }


type alias StoragePrep =
    {}


type alias Storage =
    {}



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
    , protocolParams : Maybe ProtocolParams
    , errors : List String
    }


type Page
    = LandingPage
    | PreparationPage PreparationModel
    | SigningPage SigningModel
    | SubmissionPage SubmissionModel


type alias ProtocolParams =
    { costModels : CostModels
    , drepDeposit : Natural
    }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , utxos : Utxo.RefDict Output
    , changeAddress : Address
    }


type alias FeeProvider =
    { address : Address
    , utxos : Utxo.RefDict Output
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = LandingPage
      , walletsDiscovered = []
      , protocolParams = Nothing
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
                (JD.succeed Loading)



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
    | GotProtocolParams (Result Http.Error ProtocolParams)
    | GotProposals (Result Http.Error (List ActiveProposal))
      -- New messages for voter identification
    | VoterTypeSelected VoterType
    | VoterCredentialUpdated VoterCredForm
    | FeeProviderSelected FeeProviderType
    | SubmitVoterIdentification
      -- Preparation stage navigation
    | StartPreparation


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

        ( WalletMsg value, _ ) ->
            case JD.decodeValue Cip30.responseDecoder value of
                -- We just discovered available wallets
                Ok (Cip30.AvailableWallets wallets) ->
                    ( { model | walletsDiscovered = wallets }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ( StartPreparation, { protocolParams } ) ->
            case protocolParams of
                Just _ ->
                    ( { model
                        | page =
                            PreparationPage
                                { proposals = []
                                , voterStep =
                                    NotDone
                                        { voterType = DrepVoter
                                        , voterCred = StakeKeyVoter ""
                                        , feeProviderType = ConnectedWalletFeeProvider
                                        }
                                , pickProposalStep = NotDone {}
                                , rationaleCreationStep = NotDone initRationaleForm
                                , rationaleSignatureStep = NotDone Dict.empty
                                , permanentStorageStep = NotDone {}
                                , buildTxStep = NotDone {}
                                }
                      }
                    , loadGovernanceProposals
                    )

                Nothing ->
                    ( { model | errors = "Protocol parameters not loaded" :: model.errors }
                    , Cmd.none
                    )

        ( VoterTypeSelected newType, { page } ) ->
            case page of
                PreparationPage prep ->
                    case prep.voterStep of
                        NotDone form ->
                            ( { model
                                | page =
                                    PreparationPage
                                        { prep
                                            | voterStep =
                                                NotDone { form | voterType = newType }
                                        }
                              }
                            , Cmd.none
                            )

                        Done _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( VoterCredentialUpdated newCred, { page } ) ->
            case page of
                PreparationPage prep ->
                    case prep.voterStep of
                        NotDone form ->
                            ( { model
                                | page =
                                    PreparationPage
                                        { prep
                                            | voterStep =
                                                NotDone { form | voterCred = newCred }
                                        }
                              }
                            , Cmd.none
                            )

                        Done _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( FeeProviderSelected newProvider, { page } ) ->
            case page of
                PreparationPage prep ->
                    case prep.voterStep of
                        NotDone form ->
                            ( { model
                                | page =
                                    PreparationPage
                                        { prep
                                            | voterStep =
                                                NotDone
                                                    { form | feeProviderType = newProvider }
                                        }
                              }
                            , Cmd.none
                            )

                        Done _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


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


initRationaleForm : RationaleForm
initRationaleForm =
    { authors = []
    , summary = {}
    , rationaleStatement = {}
    , precedentDiscussion = {}
    , counterargumentDiscussion = {}
    , conclusion = {}
    , internalVote =
        { constitutional = 0
        , unconstitutional = 0
        , abstain = 0
        , didNotVote = 0
        }
    , references = {}
    }


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
        [ viewHeader
        , viewContent model
        , viewErrors model.errors
        ]


viewHeader : Html Msg
viewHeader =
    div []
        [ Html.h1 [] [ text "Cardano Governance Voting" ] ]


viewContent : Model -> Html Msg
viewContent model =
    case model.page of
        LandingPage ->
            viewLandingPage model.walletsDiscovered

        PreparationPage prepModel ->
            viewPreparationPage prepModel

        SigningPage signingModel ->
            viewSigningPage signingModel

        SubmissionPage submissionModel ->
            viewSubmissionPage submissionModel


viewLandingPage : List WalletDescriptor -> Html Msg
viewLandingPage wallets =
    div []
        [ Html.h2 [] [ text "Welcome to the Voting App" ]
        , Html.p [] [ text "Please connect your wallet to begin." ]
        , viewAvailableWallets wallets
        , button
            [ onClick StartPreparation ]
            [ text "Start Vote Preparation" ]
        ]



-- Preparation page


viewPreparationPage : PreparationModel -> Html Msg
viewPreparationPage model =
    div []
        [ Html.h2 [] [ text "Vote Preparation" ]
        , viewVoterIdentificationStep model.voterStep
        , viewProposalSelectionStep model
        , viewRationaleStep model.rationaleCreationStep
        , viewPermanentStorageStep model.permanentStorageStep
        , viewBuildTxStep model.buildTxStep
        ]


viewVoterIdentificationStep : Step VoterPreparationForm VoterIdentified -> Html Msg
viewVoterIdentificationStep step =
    case step of
        NotDone form ->
            div []
                [ Html.h3 [] [ text "Voter Identification" ]
                , viewVoterTypeSelector form.voterType
                , viewVoterCredentialsForm form.voterCred
                , viewFeeProviderSelector form.feeProviderType
                ]

        Done voter ->
            div []
                [ Html.h3 [] [ text "Voter Identified" ]
                , viewIdentifiedVoter voter
                ]


viewVoterTypeSelector : VoterType -> Html Msg
viewVoterTypeSelector currentType =
    div []
        [ Html.h4 [] [ text "Select Voter Type" ]
        , div []
            [ viewVoterTypeOption CcVoter "Constitutional Committee" (currentType == CcVoter)
            , viewVoterTypeOption DrepVoter "DRep" (currentType == DrepVoter)
            , viewVoterTypeOption SpoVoter "SPO" (currentType == SpoVoter)
            ]
        ]


viewVoterTypeOption : VoterType -> String -> Bool -> Html Msg
viewVoterTypeOption voterType label isSelected =
    div []
        [ Html.input
            [ HA.type_ "radio"
            , HA.name "voter-type"
            , HA.checked isSelected
            , onClick (VoterTypeSelected voterType)
            ]
            []
        , Html.label [] [ text label ]
        ]


viewCredTypeOption : VoterCredForm -> String -> Bool -> Html Msg
viewCredTypeOption voterCredType label isSelected =
    div []
        [ Html.input
            [ HA.type_ "radio"
            , HA.name "cred-type"
            , HA.checked isSelected
            , onClick (VoterCredentialUpdated voterCredType)
            ]
            []
        , Html.label [] [ text label ]
        ]


viewVoterCredentialsForm : VoterCredForm -> Html Msg
viewVoterCredentialsForm credForm =
    let
        isStakeKeyVoter =
            case credForm of
                StakeKeyVoter _ ->
                    True

                _ ->
                    False
    in
    div []
        [ Html.h4 [] [ text "Voter Credentials" ]
        , div []
            [ viewCredTypeOption (StakeKeyVoter "") "Stake Key Voter" isStakeKeyVoter
            , viewCredTypeOption (ScriptVoter { scriptHash = "", utxoRef = "" }) "Script Voter" (not isStakeKeyVoter)
            ]
        , case credForm of
            StakeKeyVoter key ->
                div []
                    [ Html.label [] [ text "Stake Key Hash" ]
                    , Html.input
                        [ HA.type_ "text"
                        , HA.value key
                        , Html.Events.onInput (\s -> VoterCredentialUpdated (StakeKeyVoter s))
                        ]
                        []
                    ]

            ScriptVoter { scriptHash, utxoRef } ->
                div []
                    [ Html.label [] [ text "Script Hash" ]
                    , Html.input
                        [ HA.type_ "text"
                        , HA.value scriptHash
                        , Html.Events.onInput
                            (\s -> VoterCredentialUpdated (ScriptVoter { scriptHash = s, utxoRef = utxoRef }))
                        ]
                        []
                    , Html.label [] [ text "UTxO Reference" ]
                    , Html.input
                        [ HA.type_ "text"
                        , HA.value utxoRef
                        , Html.Events.onInput
                            (\s -> VoterCredentialUpdated (ScriptVoter { scriptHash = scriptHash, utxoRef = s }))
                        ]
                        []
                    ]
        ]


viewFeeProviderSelector : FeeProviderType -> Html Msg
viewFeeProviderSelector feeProviderType =
    div []
        [ Html.h4 [] [ text "Fee Provider" ]
        , div []
            [ viewFeeProviderOption
                ConnectedWalletFeeProvider
                "Use Connected Wallet"
                (feeProviderType == ConnectedWalletFeeProvider)
            , Html.hr [] []
            , case feeProviderType of
                ExternalFeeProvider { endpoint } ->
                    div []
                        [ Html.label [] [ text "External Provider Endpoint" ]
                        , Html.input
                            [ HA.type_ "text"
                            , HA.value endpoint
                            , Html.Events.onInput
                                (\s -> FeeProviderSelected (ExternalFeeProvider { endpoint = s }))
                            ]
                            []
                        ]

                _ ->
                    text ""
            ]
        ]


viewFeeProviderOption : FeeProviderType -> String -> Bool -> Html Msg
viewFeeProviderOption feeProviderType label isSelected =
    div []
        [ Html.input
            [ HA.type_ "radio"
            , HA.name "fee-provider"
            , HA.checked isSelected
            , onClick (FeeProviderSelected feeProviderType)
            ]
            []
        , Html.label [] [ text label ]
        ]


viewIdentifiedVoter : VoterIdentified -> Html Msg
viewIdentifiedVoter voter =
    text "TODO viewIdentifiedVoter"


viewProposalSelectionStep : PreparationModel -> Html Msg
viewProposalSelectionStep model =
    text "TODO viewProposalSelectionStep"


viewRationaleStep : Step RationaleForm Rationale -> Html Msg
viewRationaleStep step =
    text "TODO viewRationaleStep"


viewPermanentStorageStep : Step StoragePrep Storage -> Html Msg
viewPermanentStorageStep step =
    text "TODO viewPermanentStorageStep"


viewBuildTxStep : Step {} Transaction -> Html Msg
viewBuildTxStep step =
    text "TODO viewBuildTxStep"



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
