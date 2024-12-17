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
viewVoterTypeSelector voterType =
    Debug.todo "viewVoterTypeSelector"


viewVoterCredentialsForm : VoterCredForm -> Html Msg
viewVoterCredentialsForm voterCredForm =
    Debug.todo "viewVoterCredentialsForm"


viewFeeProviderSelector : FeeProviderType -> Html Msg
viewFeeProviderSelector feeProviderType =
    Debug.todo "viewFeeProviderSelector"


viewIdentifiedVoter : VoterIdentified -> Html Msg
viewIdentifiedVoter voter =
    Debug.todo "viewIdentifiedVoter"


viewProposalSelectionStep : PreparationModel -> Html Msg
viewProposalSelectionStep model =
    Debug.todo "viewProposalSelectionStep"


viewRationaleStep : Step RationaleForm Rationale -> Html Msg
viewRationaleStep step =
    Debug.todo "viewRationaleStep"


viewPermanentStorageStep : Step StoragePrep Storage -> Html Msg
viewPermanentStorageStep step =
    Debug.todo "viewPermanentStorageStep"


viewBuildTxStep : Step {} Transaction -> Html Msg
viewBuildTxStep step =
    Debug.todo "viewBuildTxStep"



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
