module Page.Preparation exposing (AuthorWitness, BuildTxPrep, FeeProvider, FeeProviderForm, FeeProviderTemp, InternalVote, JsonLdContexts, LoadedWallet, MarkdownForm, Model, Msg, MsgToParent(..), Rationale, RationaleForm, RationaleSignatureForm, Reference, ReferenceType(..), Step, StorageForm, TaskCompleted, UpdateContext, ViewContext, VoterPreparationForm, handleTaskCompleted, init, noInternalVote, pinRationaleFile, update, view)

import Api exposing (ActiveProposal, CcInfo, DrepInfo, IpfsAnswer(..), PoolInfo)
import Blake2b exposing (blake2b256)
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CredentialWitness(..), ScriptWitness(..), TxFinalized, VoterWitness(..), WitnessSource(..))
import Cardano.Address exposing (Address, Credential(..), CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov as Gov exposing (ActionId, Anchor, CostModels, Id(..), Vote)
import Cardano.Pool as Pool
import Cardano.Script as Script
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cbor.Encode
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Extra
import ConcurrentTask.Http
import Dict exposing (Dict)
import Dict.Any
import File exposing (File)
import File.Select
import Helper exposing (prettyAdaLovelace, prettyAddr)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onCheck, onClick)
import Html.Lazy
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Markdown.Parser as Md
import Markdown.Renderer as Md
import Natural
import Platform.Cmd as Cmd
import RemoteData exposing (RemoteData, WebData)
import ScriptInfo exposing (ScriptInfo)
import Set exposing (Set)
import Storage
import Task
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


type alias Model =
    { someRefUtxos : Utxo.RefDict Output
    , voterStep : Step VoterPreparationForm VoterWitness VoterWitness
    , pickProposalStep : Step {} {} ActiveProposal
    , rationaleCreationStep : Step RationaleForm {} Rationale
    , rationaleSignatureStep : Step RationaleSignatureForm {} RationaleSignature
    , permanentStorageStep : Step StorageForm {} Storage
    , feeProviderStep : Step FeeProviderForm FeeProviderTemp FeeProvider
    , buildTxStep : Step BuildTxPrep {} TxFinalized
    , signTxStep : Step { error : Maybe String } SigningTx SignedTx
    }


type Step prep validating done
    = Preparing prep
    | Validating prep validating
    | Done prep done


init : Model
init =
    { someRefUtxos = Utxo.emptyRefDict
    , voterStep = Preparing initVoterForm
    , pickProposalStep = Preparing {}
    , rationaleCreationStep = Preparing initRationaleForm
    , rationaleSignatureStep = Preparing initRationaleSignatureForm
    , permanentStorageStep = Preparing initStorageForm
    , feeProviderStep = Preparing (ConnectedWalletFeeProvider { error = Nothing })
    , buildTxStep = Preparing { error = Nothing }
    , signTxStep = Preparing { error = Nothing }
    }



-- Voter Step


type alias VoterPreparationForm =
    { govId : Maybe Gov.Id
    , scriptInfo : RemoteData ConcurrentTask.Http.Error ScriptInfo
    , drepInfo : WebData DrepInfo
    , ccInfo : WebData CcInfo
    , poolInfo : WebData PoolInfo
    , utxoRef : String
    , expectedSigners : Dict String { expected : Bool, key : Bytes CredentialHash }
    , error : Maybe String
    }


initVoterForm : VoterPreparationForm
initVoterForm =
    { govId = Nothing
    , scriptInfo = RemoteData.NotAsked
    , drepInfo = RemoteData.NotAsked
    , ccInfo = RemoteData.NotAsked
    , poolInfo = RemoteData.NotAsked
    , utxoRef = ""
    , expectedSigners = Dict.empty
    , error = Nothing
    }



-- Rationale Step


type alias RationaleForm =
    { summary : String
    , rationaleStatement : MarkdownForm
    , precedentDiscussion : MarkdownForm
    , counterArgumentDiscussion : MarkdownForm
    , conclusion : MarkdownForm
    , internalVote : InternalVote
    , references : List Reference
    , error : Maybe String
    }


type alias MarkdownForm =
    String


type alias InternalVote =
    { constitutional : Int
    , unconstitutional : Int
    , abstain : Int
    , didNotVote : Int
    }


noInternalVote : InternalVote
noInternalVote =
    { constitutional = 0
    , unconstitutional = 0
    , abstain = 0
    , didNotVote = 0
    }


initRationaleForm : RationaleForm
initRationaleForm =
    { summary = ""
    , rationaleStatement = ""
    , precedentDiscussion = ""
    , counterArgumentDiscussion = ""
    , conclusion = ""
    , internalVote = noInternalVote
    , references = []
    , error = Nothing
    }


type alias Rationale =
    { summary : String
    , rationaleStatement : String
    , precedentDiscussion : Maybe String
    , counterArgumentDiscussion : Maybe String
    , conclusion : Maybe String
    , internalVote : InternalVote
    , references : List Reference
    }


type alias Reference =
    { type_ : ReferenceType
    , label : String
    , uri : String
    }


type ReferenceType
    = OtherRefType
    | GovernanceMetadataRefType
    | RelevantArticlesRefType


allRefTypes : List ReferenceType
allRefTypes =
    [ RelevantArticlesRefType
    , GovernanceMetadataRefType
    , OtherRefType
    ]


refTypeToString : ReferenceType -> String
refTypeToString refType =
    case refType of
        RelevantArticlesRefType ->
            "relevant articles"

        GovernanceMetadataRefType ->
            "governance metadata"

        OtherRefType ->
            "other"


refTypeFromString : String -> ReferenceType
refTypeFromString str =
    List.filter (\refType -> refTypeToString refType == str) allRefTypes
        |> List.head
        |> Maybe.withDefault OtherRefType


initRefForm : Reference
initRefForm =
    { type_ = RelevantArticlesRefType
    , label = ""
    , uri = ""
    }



-- Rationale Signature Step


type alias RationaleSignatureForm =
    { authors : List AuthorWitness
    , rationale : Rationale
    , error : Maybe String
    }


initRationaleSignatureForm : RationaleSignatureForm
initRationaleSignatureForm =
    { authors = []
    , rationale = rationaleFromForm initRationaleForm
    , error = Nothing
    }


type alias RationaleSignature =
    { authors : List AuthorWitness
    , rationale : Rationale
    , signedJson : String
    }


type alias AuthorWitness =
    { name : String
    , witnessAlgorithm : String
    , publicKey : String
    , signature : Maybe String
    }


initAuthorForm : AuthorWitness
initAuthorForm =
    { name = "John Doe"
    , witnessAlgorithm = "ed25519"
    , publicKey = ""
    , signature = Nothing
    }



-- Storage Step


type alias StorageForm =
    { ipfsServer : String
    , headers : List ( String, String )
    , error : Maybe String
    }


initStorageForm : StorageForm
initStorageForm =
    { ipfsServer = "https://ipfs.blockfrost.io/api/v0/ipfs"
    , headers = [ ( "project_id", "" ) ]
    , error = Nothing
    }


type alias Storage =
    { config : StorageForm
    , jsonFile : IpfsFile
    }


type alias IpfsFile =
    { name : String
    , cid : String
    , size : String
    }



-- Fee Provider Step


type FeeProviderForm
    = ConnectedWalletFeeProvider { error : Maybe String }
    | ExternalFeeProvider { endpoint : String, error : Maybe String }


type alias FeeProviderTemp =
    { address : Maybe Address
    , utxos : Maybe (Utxo.RefDict Output)
    }


type alias FeeProvider =
    { address : Address
    , utxos : Utxo.RefDict Output
    }



-- Build Tx Step


type alias BuildTxPrep =
    { error : Maybe String
    }



-- Sign Tx Step


type alias SigningTx =
    { tx : Transaction
    , expectedSignatures : List (Bytes CredentialHash)
    , vkeyWitnesses : List VKeyWitness
    }


type alias SignedTx =
    { signedTx : Transaction
    , txId : Bytes TransactionId
    }



-- ###################################################################
-- UPDATE
-- ###################################################################


type MsgToParent
    = CacheScriptInfo ScriptInfo
    | CacheDrepInfo DrepInfo
    | CacheCcInfo CcInfo
    | CachePoolInfo PoolInfo
    | RunTask (ConcurrentTask String TaskCompleted)


type TaskCompleted
    = GotRefUtxoTxBytes OutputReference (Result ConcurrentTask.Http.Error (Bytes Transaction))
    | GotScriptInfoTask (Result ConcurrentTask.Http.Error ScriptInfo)


type Msg
    = NoMsg
      -- Voter Step
    | VoterGovIdChange String
    | GotDrepInfo (Result Http.Error DrepInfo)
    | GotCcInfo (Result Http.Error CcInfo)
    | GotPoolInfo (Result Http.Error PoolInfo)
    | UtxoRefChange String
    | ToggleExpectedSigner String Bool
    | ValidateVoterFormButtonClicked
    | ChangeVoterButtonClicked
      -- Pick Proposal Step
    | PickProposalButtonClicked String
    | ChangeProposalButtonClicked
      -- Rationale
    | RationaleSummaryChange String
    | RationaleStatementChange String
    | PrecedentDiscussionChange String
    | CounterArgumentChange String
    | ConclusionChange String
    | InternalConstitutionalVoteChange String
    | InternalUnconstitutionalVoteChange String
    | InternalAbstainVoteChange String
    | InternalDidNotVoteChange String
    | AddRefButtonClicked
    | DeleteRefButtonClicked Int
    | ReferenceLabelChange Int String
    | ReferenceUriChange Int String
    | ReferenceTypeChange Int String
    | ValidateRationaleButtonClicked
    | EditRationaleButtonClicked
      -- Rationale Signature
    | AddAuthorButtonClicked
    | DeleteAuthorButtonClicked Int
    | AuthorNameChange Int String
    | LoadJsonSignatureButtonClicked Int String
    | FileSelectedForJsonSignature Int String File
    | LoadedAuthorSignatureJsonRationale Int String String
    | SkipRationaleSignaturesButtonClicked
    | ValidateRationaleSignaturesButtonClicked
    | ChangeAuthorsButtonClicked
      -- Storage
    | IpfsServerChange String
    | AddHeaderButtonClicked
    | DeleteHeaderButtonClicked Int
    | StorageHeaderFieldChange Int String
    | StorageHeaderValueChange Int String
    | PinJsonIpfsButtonClicked
    | GotIpfsAnswer (Result String IpfsAnswer)
    | AddOtherStorageButtonCLicked
      -- Fee Provider Step
    | FeeProviderUpdated FeeProviderForm
    | ValidateFeeProviderFormButtonClicked
    | ReceivedFeeProviderUtxos FeeProvider
    | ChangeFeeProviderButtonClicked
      -- Build Tx Step
    | BuildTxButtonClicked Vote
    | ChangeVoteButtonClicked


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , db : JD.Value
    , proposals : WebData (Dict String ActiveProposal)
    , scriptsInfo : Dict String ScriptInfo
    , drepsInfo : Dict String DrepInfo
    , ccsInfo : Dict String CcInfo
    , poolsInfo : Dict String PoolInfo
    , loadedWallet : Maybe LoadedWallet
    , feeProviderAskUtxosCmd : Cmd msg
    , jsonLdContexts : JsonLdContexts
    , jsonRationaleToFile : { fileContent : String, fileName : String } -> Cmd msg
    , costModels : Maybe CostModels
    , walletSignTx : Transaction -> Cmd msg
    }


type alias JsonLdContexts =
    { ccCip136Context : JE.Value }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , changeAddress : Address
    , utxos : Utxo.RefDict Output
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg, Maybe MsgToParent )
update ctx msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none, Nothing )

        --
        -- Voter Step
        --
        ToggleExpectedSigner keyHex expected ->
            let
                updatedExpectedSigners expectedSigners =
                    Dict.update keyHex
                        (Maybe.map (\entry -> { entry | expected = expected }))
                        expectedSigners
            in
            ( updateVoterForm (\form -> { form | expectedSigners = updatedExpectedSigners form.expectedSigners }) model
            , Cmd.none
            , Nothing
            )

        ValidateVoterFormButtonClicked ->
            case model.voterStep of
                Preparing form ->
                    let
                        ( newVoterStep, cmds, toParent ) =
                            confirmVoter ctx form model.someRefUtxos
                    in
                    ( { model | voterStep = newVoterStep }
                    , Cmd.map ctx.wrapMsg cmds
                    , toParent
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        VoterGovIdChange govIdStr ->
            case checkGovId ctx govIdStr of
                Err error ->
                    ( updateVoterForm (\_ -> { initVoterForm | error = Just error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok { govId, scriptInfo, expectedSigners, drepInfo, ccInfo, poolInfo, cmd, msgToParent } ->
                    ( updateVoterForm
                        (\_ ->
                            { initVoterForm
                                | govId = Just govId
                                , scriptInfo = scriptInfo
                                , expectedSigners = expectedSigners
                                , drepInfo = drepInfo
                                , ccInfo = ccInfo
                                , poolInfo = poolInfo
                            }
                        )
                        model
                    , Cmd.map ctx.wrapMsg cmd
                    , msgToParent
                    )

        GotDrepInfo result ->
            case result of
                Err error ->
                    ( updateVoterForm (\form -> { form | drepInfo = RemoteData.Failure (Debug.log "ERROR loading DRep info" error) }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok drepInfo ->
                    ( updateVoterForm (\form -> { form | drepInfo = RemoteData.Success drepInfo }) model
                    , Cmd.none
                    , Just <| CacheDrepInfo drepInfo
                    )

        GotCcInfo result ->
            case result of
                Err error ->
                    ( updateVoterForm (\form -> { form | ccInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok ccInfo ->
                    ( updateVoterForm (\form -> { form | ccInfo = RemoteData.Success ccInfo }) model
                    , Cmd.none
                    , Just <| CacheCcInfo ccInfo
                    )

        GotPoolInfo result ->
            case result of
                Err error ->
                    ( updateVoterForm (\form -> { form | poolInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok poolInfo ->
                    ( updateVoterForm (\form -> { form | poolInfo = RemoteData.Success poolInfo }) model
                    , Cmd.none
                    , Just <| CachePoolInfo poolInfo
                    )

        UtxoRefChange utxoRef ->
            ( updateVoterForm (\form -> { form | utxoRef = utxoRef }) model
            , Cmd.none
            , Nothing
            )

        ChangeVoterButtonClicked ->
            case model.voterStep of
                Done prep _ ->
                    ( { model | voterStep = Preparing prep }
                      -- TODO: also reset all dependents steps
                      -- |> resetProposal
                      -- |> resetRationaleCreation
                      -- |> resetRationaleSignature
                      -- |> resetStorage
                      -- |> resetFeeProvider
                      -- |> resetTxBuilding
                      -- |> resetTxSigning
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        --
        -- Pick Proposal Step
        --
        PickProposalButtonClicked actionId ->
            case ( model.pickProposalStep, ctx.proposals ) of
                ( Preparing form, RemoteData.Success proposalsDict ) ->
                    case Dict.get actionId proposalsDict of
                        Just prop ->
                            ( { model | pickProposalStep = Done form prop }
                            , Cmd.none
                            , Nothing
                            )

                        Nothing ->
                            ( model, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        ChangeProposalButtonClicked ->
            ( { model | pickProposalStep = Preparing {} }
            , Cmd.none
            , Nothing
            )

        --
        -- Rationale Step
        --
        RationaleSummaryChange summary ->
            ( updateRationaleForm (\form -> { form | summary = summary }) model
            , Cmd.none
            , Nothing
            )

        RationaleStatementChange statement ->
            ( updateRationaleForm (\form -> { form | rationaleStatement = statement }) model
            , Cmd.none
            , Nothing
            )

        PrecedentDiscussionChange precedentDiscussion ->
            ( updateRationaleForm (\form -> { form | precedentDiscussion = precedentDiscussion }) model
            , Cmd.none
            , Nothing
            )

        CounterArgumentChange argument ->
            ( updateRationaleForm (\form -> { form | counterArgumentDiscussion = argument }) model
            , Cmd.none
            , Nothing
            )

        ConclusionChange conclusion ->
            ( updateRationaleForm (\form -> { form | conclusion = conclusion }) model
            , Cmd.none
            , Nothing
            )

        InternalConstitutionalVoteChange constitutionalStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | constitutional = n }) constitutionalStr model
            , Cmd.none
            , Nothing
            )

        InternalUnconstitutionalVoteChange unconstitutionalStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | unconstitutional = n }) unconstitutionalStr model
            , Cmd.none
            , Nothing
            )

        InternalAbstainVoteChange abstainStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | abstain = n }) abstainStr model
            , Cmd.none
            , Nothing
            )

        InternalDidNotVoteChange didNotVoteStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | didNotVote = n }) didNotVoteStr model
            , Cmd.none
            , Nothing
            )

        AddRefButtonClicked ->
            ( updateRationaleForm (\form -> { form | references = initRefForm :: form.references }) model
            , Cmd.none
            , Nothing
            )

        DeleteRefButtonClicked n ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.removeAt n form.references }) model
            , Cmd.none
            , Nothing
            )

        ReferenceLabelChange n label ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | label = label }) form.references }) model
            , Cmd.none
            , Nothing
            )

        ReferenceUriChange n uri ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | uri = uri }) form.references }) model
            , Cmd.none
            , Nothing
            )

        ReferenceTypeChange n refTypeStr ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | type_ = refTypeFromString refTypeStr }) form.references }) model
            , Cmd.none
            , Nothing
            )

        ValidateRationaleButtonClicked ->
            case validateRationaleForm model.rationaleCreationStep of
                Done prep newRationale ->
                    ( { model
                        | rationaleCreationStep = Done prep newRationale
                        , rationaleSignatureStep = Preparing <| resetRationaleSignatures newRationale model.rationaleSignatureStep
                      }
                    , Cmd.none
                    , Nothing
                    )

                prepOrValidating ->
                    ( { model | rationaleCreationStep = prepOrValidating }, Cmd.none, Nothing )

        EditRationaleButtonClicked ->
            ( { model | rationaleCreationStep = editRationale model.rationaleCreationStep }
            , Cmd.none
            , Nothing
            )

        --
        -- Rationale Signature Step
        --
        AddAuthorButtonClicked ->
            ( updateAuthorsForm (\authors -> initAuthorForm :: authors) model
            , Cmd.none
            , Nothing
            )

        DeleteAuthorButtonClicked n ->
            ( updateAuthorsForm (\authors -> List.Extra.removeAt n authors) model
            , Cmd.none
            , Nothing
            )

        AuthorNameChange n name ->
            ( updateAuthorsForm (\authors -> List.Extra.updateAt n (\author -> { author | name = name }) authors) model
            , Cmd.none
            , Nothing
            )

        LoadJsonSignatureButtonClicked n authorName ->
            ( model
            , Cmd.map ctx.wrapMsg <|
                File.Select.file [ "application/json" ] <|
                    FileSelectedForJsonSignature n authorName
            , Nothing
            )

        FileSelectedForJsonSignature n authorName file ->
            ( model
            , Task.attempt (handleJsonSignatureFileRead n authorName) (File.toString file)
                |> Cmd.map ctx.wrapMsg
            , Nothing
            )

        LoadedAuthorSignatureJsonRationale n authorName jsonStr ->
            ( case JD.decodeString (authorWitnessExtractDecoder authorName) jsonStr of
                Err decodingError ->
                    { model
                        | rationaleSignatureStep =
                            signatureDecodingError decodingError model.rationaleSignatureStep
                    }

                Ok authorWitness ->
                    updateAuthorsForm (\authors -> List.Extra.updateAt n (\_ -> authorWitness) authors) model
            , Cmd.none
            , Nothing
            )

        SkipRationaleSignaturesButtonClicked ->
            ( { model | rationaleSignatureStep = skipRationaleSignature ctx.jsonLdContexts model.rationaleSignatureStep }
            , Cmd.none
            , Nothing
            )

        ValidateRationaleSignaturesButtonClicked ->
            validateRationaleSignature ctx.jsonLdContexts model

        ChangeAuthorsButtonClicked ->
            ( { model | rationaleSignatureStep = Preparing <| rationaleSignatureToForm model.rationaleSignatureStep }
            , Cmd.none
            , Nothing
            )

        --
        -- Permanent Storage Step
        --
        IpfsServerChange ipfsServer ->
            ( updateStorageForm (\form -> { form | ipfsServer = ipfsServer }) model
            , Cmd.none
            , Nothing
            )

        AddHeaderButtonClicked ->
            ( updateStorageForm (\form -> { form | headers = ( "", "" ) :: form.headers }) model
            , Cmd.none
            , Nothing
            )

        DeleteHeaderButtonClicked n ->
            ( updateStorageForm (\form -> { form | headers = List.Extra.removeAt n form.headers }) model
            , Cmd.none
            , Nothing
            )

        StorageHeaderFieldChange n field ->
            ( updateStorageForm (\form -> { form | headers = List.Extra.updateAt n (\( _, v ) -> ( field, v )) form.headers }) model
            , Cmd.none
            , Nothing
            )

        StorageHeaderValueChange n value ->
            ( updateStorageForm (\form -> { form | headers = List.Extra.updateAt n (\( f, _ ) -> ( f, value )) form.headers }) model
            , Cmd.none
            , Nothing
            )

        PinJsonIpfsButtonClicked ->
            case model.permanentStorageStep of
                Preparing form ->
                    validateIpfsFormAndSendPinRequest ctx form model

                Validating _ _ ->
                    ( model, Cmd.none, Nothing )

                Done _ _ ->
                    ( model, Cmd.none, Nothing )

        GotIpfsAnswer (Err httpError) ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none, Nothing )

                Validating form _ ->
                    ( { model | permanentStorageStep = Preparing { form | error = Just <| Debug.toString httpError } }
                    , Cmd.none
                    , Nothing
                    )

                Done _ _ ->
                    ( model, Cmd.none, Nothing )

        GotIpfsAnswer (Ok ipfsAnswer) ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none, Nothing )

                Validating form _ ->
                    handleIpfsAnswer model form ipfsAnswer

                Done _ _ ->
                    ( model, Cmd.none, Nothing )

        AddOtherStorageButtonCLicked ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none, Nothing )

                Validating _ _ ->
                    ( model, Cmd.none, Nothing )

                Done prep _ ->
                    ( { model | permanentStorageStep = Preparing prep }
                    , Cmd.none
                    , Nothing
                    )

        --
        -- Fee Provider Step
        --
        FeeProviderUpdated feeProviderForm ->
            ( updateFeeProviderForm feeProviderForm model
            , Cmd.none
            , Nothing
            )

        ValidateFeeProviderFormButtonClicked ->
            case model.feeProviderStep of
                Preparing form ->
                    case validateFeeProviderForm ctx.loadedWallet form of
                        (Validating _ _) as validating ->
                            ( { model | feeProviderStep = validating }
                            , ctx.feeProviderAskUtxosCmd
                            , Nothing
                            )

                        validated ->
                            ( { model | feeProviderStep = validated }, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        ReceivedFeeProviderUtxos feeProvider ->
            case model.feeProviderStep of
                Validating form _ ->
                    ( { model | feeProviderStep = Done form feeProvider }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        ChangeFeeProviderButtonClicked ->
            case model.feeProviderStep of
                Done prep _ ->
                    ( { model | feeProviderStep = Preparing prep }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        --
        -- Build Tx Step
        --
        BuildTxButtonClicked vote ->
            case allPrepSteps ctx.costModels model of
                Err error ->
                    ( { model | buildTxStep = Preparing { error = Just error } }
                    , Cmd.none
                    , Nothing
                    )

                Ok { voter, actionId, rationaleAnchor, localStateUtxos, feeProviderAddress, costModels } ->
                    let
                        tryTx =
                            [ Cardano.Vote voter [ { actionId = actionId, vote = vote, rationale = Just rationaleAnchor } ]
                            ]
                                |> Cardano.finalizeAdvanced
                                    { govState = Cardano.emptyGovernanceState
                                    , localStateUtxos = localStateUtxos
                                    , coinSelectionAlgo = CoinSelection.largestFirst
                                    , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
                                    , costModels = costModels
                                    }
                                    (Cardano.AutoFee { paymentSource = feeProviderAddress })
                                    []
                    in
                    case tryTx of
                        Err error ->
                            ( { model | buildTxStep = Preparing { error = Just <| "Error while building the Tx: " ++ Debug.toString error } }
                            , Cmd.none
                            , Nothing
                            )

                        Ok tx ->
                            ( { model | buildTxStep = Done { error = Nothing } tx }
                            , Cmd.none
                            , Nothing
                            )

        ChangeVoteButtonClicked ->
            ( { model | buildTxStep = Preparing { error = Nothing } }
            , Cmd.none
            , Nothing
            )


handleTaskCompleted : TaskCompleted -> Model -> ( Model, Cmd Msg, Maybe MsgToParent )
handleTaskCompleted task model =
    case task of
        GotRefUtxoTxBytes outputRef bytesResult ->
            case ( model.voterStep, bytesResult ) of
                ( Validating form voterWitness, Ok txBytes ) ->
                    case Transaction.deserialize txBytes of
                        Nothing ->
                            let
                                errorMsg =
                                    "Failed to decode Tx "
                                        ++ Bytes.toHex outputRef.transactionId
                                        ++ ": "
                                        ++ Bytes.toHex txBytes
                            in
                            ( { model | voterStep = Preparing { form | error = Just errorMsg } }
                            , Cmd.none
                            , Nothing
                            )

                        Just tx ->
                            case List.Extra.getAt outputRef.outputIndex tx.body.outputs of
                                Nothing ->
                                    let
                                        errorMsg =
                                            "The Tx retrieved (id: "
                                                ++ Bytes.toHex outputRef.transactionId
                                                ++ ") doesn’t seem to have an output at position "
                                                ++ String.fromInt outputRef.outputIndex
                                    in
                                    ( { model | voterStep = Preparing { form | error = Just errorMsg } }
                                    , Cmd.none
                                    , Nothing
                                    )

                                Just output ->
                                    ( { model
                                        | voterStep = Done { form | error = Nothing } voterWitness
                                        , someRefUtxos = Dict.Any.insert outputRef output model.someRefUtxos
                                      }
                                    , Cmd.none
                                    , Nothing
                                    )

                ( Validating form _, Err httpError ) ->
                    ( { model | voterStep = Preparing { form | error = Just (Debug.toString httpError) } }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        GotScriptInfoTask scriptInfoResult ->
            case scriptInfoResult of
                Err error ->
                    ( updateVoterForm (\form -> { form | scriptInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok scriptInfo ->
                    ( updateVoterForm
                        (\form ->
                            { form
                                | scriptInfo = RemoteData.Success scriptInfo
                                , expectedSigners =
                                    case scriptInfo.script of
                                        Script.Native nativeScript ->
                                            Script.extractSigners nativeScript
                                                |> Dict.map (\_ key -> { expected = True, key = key })

                                        Script.Plutus _ ->
                                            Dict.empty
                            }
                        )
                        model
                    , Cmd.none
                    , Just <| CacheScriptInfo scriptInfo
                    )



-- Voter Step


updateVoterForm : (VoterPreparationForm -> VoterPreparationForm) -> Model -> Model
updateVoterForm f ({ voterStep } as model) =
    case voterStep of
        Preparing form ->
            { model | voterStep = Preparing (f form) }

        _ ->
            model


type alias GovIdCheck =
    { govId : Gov.Id
    , scriptInfo : RemoteData ConcurrentTask.Http.Error ScriptInfo
    , expectedSigners : Dict String { expected : Bool, key : Bytes CredentialHash }
    , poolInfo : WebData PoolInfo
    , drepInfo : WebData DrepInfo
    , ccInfo : WebData CcInfo
    , cmd : Cmd Msg
    , msgToParent : Maybe MsgToParent
    }


checkGovId : UpdateContext msg -> String -> Result String GovIdCheck
checkGovId ctx str =
    case Gov.idFromBech32 str of
        Nothing ->
            Err <| "This doesn’t look like a valid CIP 129 governance Id: " ++ str

        Just govId ->
            let
                defaultCheck =
                    { govId = govId
                    , scriptInfo = RemoteData.NotAsked
                    , expectedSigners = Dict.empty
                    , poolInfo = RemoteData.NotAsked
                    , drepInfo = RemoteData.NotAsked
                    , ccInfo = RemoteData.NotAsked
                    , cmd = Cmd.none
                    , msgToParent = Nothing
                    }
            in
            case govId of
                Gov.CcColdCredId _ ->
                    Err <| "You are supposed to vote with your hot CC key, not the cold key: " ++ str

                Gov.GovActionId _ ->
                    Err "Please use one of the drep/pool/cc_hot CIP 129 governance Ids. Proposal to vote on will be selected later."

                -- Using a public key for the gov Id is the simplest case
                Gov.CcHotCredId ((VKeyHash keyHash) as cred) ->
                    let
                        ( ccInfo, fetchCcInfo ) =
                            case Dict.get (Bytes.toHex keyHash) ctx.ccsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getCcInfo cred GotCcInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | ccInfo = ccInfo, cmd = fetchCcInfo }

                Gov.DrepId ((VKeyHash keyHash) as cred) ->
                    let
                        ( drepInfo, fetchDrepInfo ) =
                            case Dict.get (Bytes.toHex keyHash) ctx.drepsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getDrepInfo cred GotDrepInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | drepInfo = drepInfo, cmd = fetchDrepInfo }

                Gov.PoolId poolId ->
                    let
                        ( poolInfo, fetchPoolInfo ) =
                            case Dict.get (Bytes.toHex poolId) ctx.poolsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getPoolLiveStake poolId GotPoolInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | poolInfo = poolInfo, cmd = fetchPoolInfo }

                -- Using a script voter will require some extra information to be fetched
                Gov.CcHotCredId ((ScriptHash scriptHash) as cred) ->
                    let
                        ( scriptInfo, expectedSigners, fetchScriptInfoMsgToParent ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.scriptsInfo of
                                Nothing ->
                                    ( RemoteData.Loading
                                    , Dict.empty
                                    , Api.defaultApiProvider.getScriptInfo scriptHash
                                        |> Storage.cacheWrap
                                            { db = ctx.db, storeName = "scriptInfo" }
                                            ScriptInfo.storageDecoder
                                            ScriptInfo.storageEncode
                                            { key = Bytes.toHex scriptHash }
                                        |> ConcurrentTask.onJsException (\{ message } -> ConcurrentTask.fail <| ConcurrentTask.Http.BadUrl <| "Uncaught JS exception: " ++ message)
                                        |> ConcurrentTask.Extra.toResult
                                        |> ConcurrentTask.map GotScriptInfoTask
                                        |> RunTask
                                        |> Just
                                    )

                                Just info ->
                                    ( RemoteData.Success info
                                    , case info.script of
                                        Script.Native nativeScript ->
                                            Script.extractSigners nativeScript
                                                |> Dict.map (\_ key -> { expected = True, key = key })

                                        Script.Plutus _ ->
                                            Dict.empty
                                    , Nothing
                                    )

                        ( ccInfo, fetchCcInfo ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.ccsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getCcInfo cred GotCcInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok
                        { defaultCheck
                            | scriptInfo = scriptInfo
                            , expectedSigners = expectedSigners
                            , ccInfo = ccInfo
                            , cmd = fetchCcInfo
                            , msgToParent = fetchScriptInfoMsgToParent
                        }

                Gov.DrepId ((ScriptHash scriptHash) as cred) ->
                    let
                        ( scriptInfo, expectedSigners, fetchScriptInfoMsgToParent ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.scriptsInfo of
                                Nothing ->
                                    ( RemoteData.Loading
                                    , Dict.empty
                                    , Api.defaultApiProvider.getScriptInfo scriptHash
                                        |> Storage.cacheWrap
                                            { db = ctx.db, storeName = "scriptInfo" }
                                            ScriptInfo.storageDecoder
                                            ScriptInfo.storageEncode
                                            { key = Bytes.toHex scriptHash }
                                        |> ConcurrentTask.onJsException (\{ message } -> ConcurrentTask.fail <| ConcurrentTask.Http.BadUrl <| "Uncaught JS exception: " ++ message)
                                        |> ConcurrentTask.Extra.toResult
                                        |> ConcurrentTask.map GotScriptInfoTask
                                        |> RunTask
                                        |> Just
                                    )

                                Just info ->
                                    ( RemoteData.Success info
                                    , case info.script of
                                        Script.Native nativeScript ->
                                            Script.extractSigners nativeScript
                                                |> Dict.map (\_ key -> { expected = True, key = key })

                                        Script.Plutus _ ->
                                            Dict.empty
                                    , Nothing
                                    )

                        ( drepInfo, fetchDrepInfo ) =
                            case Dict.get (Bytes.toHex scriptHash) ctx.drepsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getDrepInfo cred GotDrepInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok
                        { defaultCheck
                            | scriptInfo = scriptInfo
                            , expectedSigners = expectedSigners
                            , drepInfo = drepInfo
                            , cmd = fetchDrepInfo
                            , msgToParent = fetchScriptInfoMsgToParent
                        }


confirmVoter : UpdateContext msg -> VoterPreparationForm -> Utxo.RefDict Output -> ( Step VoterPreparationForm VoterWitness VoterWitness, Cmd Msg, Maybe MsgToParent )
confirmVoter ctx form loadedRefUtxos =
    let
        justError errorMsg =
            ( Preparing { form | error = Just errorMsg }
            , Cmd.none
            , Nothing
            )
    in
    case form.govId of
        Nothing ->
            justError "You must provide a valid governance ID. It can be a bech32 pool ID, a CIP 129 DRep ID or CC hot ID."

        Just (CcColdCredId _) ->
            justError "CC cold keys are not used to vote, please provide your ID for the hot keys instead."

        Just (GovActionId _) ->
            justError "The proposal to vote on is selected later. For now please provide you voter ID. It can be a bech32 pool ID, a CIP 129 DRep ID or CC hot ID."

        Just (PoolId poolId) ->
            ( Done form <| WithPoolCred poolId
            , Cmd.none
            , Nothing
            )

        Just (DrepId (VKeyHash keyHash)) ->
            ( Done form <| WithDrepCred (WithKey keyHash)
            , Cmd.none
            , Nothing
            )

        Just (CcHotCredId (VKeyHash keyHash)) ->
            ( Done form <| WithCommitteeHotCred (WithKey keyHash)
            , Cmd.none
            , Nothing
            )

        Just (DrepId (ScriptHash _)) ->
            case form.scriptInfo of
                RemoteData.NotAsked ->
                    justError "Script info isn’t loading yet govId is a script. Please report the error."

                RemoteData.Loading ->
                    justError "Script info is still loading, please wait."

                RemoteData.Failure error ->
                    justError <| "There was an error loading the script info. Are you sure you registered? " ++ Debug.toString error

                RemoteData.Success scriptInfo ->
                    validateScriptVoter ctx form loadedRefUtxos WithDrepCred scriptInfo

        _ ->
            Debug.todo ""


validateScriptVoter : UpdateContext msg -> VoterPreparationForm -> Utxo.RefDict Output -> (CredentialWitness -> VoterWitness) -> ScriptInfo -> ( Step VoterPreparationForm VoterWitness VoterWitness, Cmd Msg, Maybe MsgToParent )
validateScriptVoter ctx form loadedRefUtxos toVoter scriptInfo =
    let
        justError errorMsg =
            ( Preparing { form | error = Just errorMsg }
            , Cmd.none
            , Nothing
            )
    in
    case ( form.utxoRef, utxoRefFromStr form.utxoRef ) of
        -- When not using a reference UTxO, just proceed with an inline witness
        ( "", _ ) ->
            case scriptInfo.script of
                Script.Native nativeScript ->
                    -- If the native script isn’t encoded correctly we can’t vote
                    if scriptInfo.nativeCborEncodingMatchesHash == Just True then
                        let
                            witness =
                                { script = WitnessValue nativeScript
                                , expectedSigners = keepOnlyExpectedSigners form.expectedSigners
                                }
                        in
                        ( Done { form | error = Nothing } <| toVoter <| WithScript scriptInfo.scriptHash <| NativeWitness witness
                        , Cmd.none
                        , Nothing
                        )

                    else
                        -- If the native script isn’t encoded correctly we can’t vote
                        justError "For technical reasons you need to provide a reference UTxO for the script."

                Script.Plutus { version, script } ->
                    let
                        witness =
                            { script = ( version, WitnessValue script )
                            , redeemerData = Debug.todo "Add forms for the redeemer Data"
                            , requiredSigners = Debug.todo "Add a required signers form for Plutus"
                            }
                    in
                    ( Done { form | error = Nothing } <| toVoter <| WithScript scriptInfo.scriptHash <| PlutusWitness witness
                    , Cmd.none
                    , Nothing
                    )

        ( _, Err error ) ->
            justError error

        ( _, Ok outputRef ) ->
            -- We are using an output ref for the script, that we may have already loaded or not yet
            case scriptInfo.script of
                Script.Native _ ->
                    let
                        witness =
                            { script = WitnessReference outputRef
                            , expectedSigners = keepOnlyExpectedSigners form.expectedSigners
                            }

                        voter =
                            toVoter <| WithScript scriptInfo.scriptHash <| NativeWitness witness
                    in
                    if Dict.Any.member outputRef loadedRefUtxos then
                        ( Done { form | error = Nothing } voter
                        , Cmd.none
                        , Nothing
                        )

                    else
                        ( Validating form voter
                        , Cmd.none
                        , Api.defaultApiProvider.retrieveTx outputRef.transactionId
                            |> Storage.cacheWrap
                                { db = ctx.db, storeName = "tx" }
                                Bytes.jsonDecoder
                                Bytes.jsonEncode
                                { key = Bytes.toHex outputRef.transactionId }
                            |> ConcurrentTask.onJsException (\{ message } -> ConcurrentTask.fail <| ConcurrentTask.Http.BadUrl <| "Uncaught JS exception: " ++ message)
                            |> ConcurrentTask.Extra.toResult
                            |> ConcurrentTask.map (GotRefUtxoTxBytes outputRef)
                            |> RunTask
                            |> Just
                        )

                Script.Plutus _ ->
                    Debug.todo "Handle Plutus script case"


keepOnlyExpectedSigners : Dict String { expected : Bool, key : Bytes CredentialHash } -> List (Bytes CredentialHash)
keepOnlyExpectedSigners signers =
    Dict.values signers
        |> List.filterMap
            (\{ expected, key } ->
                if expected then
                    Just key

                else
                    Nothing
            )


utxoRefFromStr : String -> Result String OutputReference
utxoRefFromStr str =
    case String.split "#" str of
        [ txIdHex, indexStr ] ->
            if String.length txIdHex == 64 then
                case ( Bytes.fromHex txIdHex, String.toInt indexStr ) of
                    ( Just txId, Just index ) ->
                        Ok { transactionId = txId, outputIndex = index }

                    ( Nothing, _ ) ->
                        Err <| "The Tx ID doesn’t look like valid hex: " ++ txIdHex

                    ( _, Nothing ) ->
                        Err <| "The output index doesn’t look like a valid integer: " ++ indexStr

            else
                Err "The Tx ID should be the hex of a 32 bytes identifier"

        _ ->
            Err "An output reference must have the shape: {txid}#0, for example: 10e7c91aca541c47c2a03debf6ebfc894ce553d0d0d3c01d053ebfca4e2893cb#0"



-- Rationale Step


updateRationaleForm : (RationaleForm -> RationaleForm) -> Model -> Model
updateRationaleForm f ({ rationaleCreationStep } as model) =
    case rationaleCreationStep of
        Preparing form ->
            { model | rationaleCreationStep = Preparing (f form) }

        _ ->
            model


updateRationaleInternalVoteForm : (Int -> InternalVote -> InternalVote) -> String -> Model -> Model
updateRationaleInternalVoteForm updateF numberStr model =
    let
        rationaleUpdate : RationaleForm -> RationaleForm
        rationaleUpdate form =
            String.toInt numberStr
                |> Maybe.map (\n -> { form | internalVote = updateF n form.internalVote })
                |> Maybe.withDefault form
    in
    updateRationaleForm rationaleUpdate model


validateRationaleForm : Step RationaleForm {} Rationale -> Step RationaleForm {} Rationale
validateRationaleForm step =
    case step of
        Preparing form ->
            let
                rationaleValidation =
                    validateRationaleSummary form.summary
                        |> Result.andThen (\_ -> validateRationaleStatement form.rationaleStatement)
                        |> Result.andThen (\_ -> validateRationaleDiscussion form.precedentDiscussion)
                        |> Result.andThen (\_ -> validateRationaleCounterArg form.counterArgumentDiscussion)
                        |> Result.andThen (\_ -> validateRationaleConclusion form.conclusion)
                        |> Result.andThen (\_ -> validateRationaleInternVote form.internalVote)
                        |> Result.andThen (\_ -> validateRationaleRefs form.references)
            in
            case rationaleValidation of
                Ok _ ->
                    Done form (rationaleFromForm form)

                Err err ->
                    Preparing { form | error = Just err }

        _ ->
            step


validateRationaleSummary : String -> Result String ()
validateRationaleSummary summary =
    -- Limited to 300 characters
    -- Should NOT support markdown (implied by not providing MD editor)
    let
        summaryLength =
            String.length summary
    in
    if summaryLength > 300 then
        Err ("Summary is limited to 300 characters, and is currently " ++ String.fromInt summaryLength)

    else if String.isEmpty (String.trim summary) then
        Err "The summary field is mandatory"

    else
        Ok ()


validateRationaleStatement : MarkdownForm -> Result String ()
validateRationaleStatement statement =
    case String.trim statement of
        "" ->
            Err "The rationale statement field is mandatory"

        str ->
            checkValidMarkdown str


checkValidMarkdown : String -> Result String ()
checkValidMarkdown str =
    case Md.parse str of
        Ok _ ->
            Ok ()

        Err deadEnds ->
            List.map Md.deadEndToString deadEnds
                |> String.join "\n"
                |> Err


validateRationaleDiscussion : MarkdownForm -> Result String ()
validateRationaleDiscussion discussion =
    -- Nothing to check really, except markdown syntax, implied by the editor
    checkValidMarkdown (String.trim discussion)


validateRationaleCounterArg : MarkdownForm -> Result String ()
validateRationaleCounterArg counterArg =
    -- Nothing to check really, except markdown syntax, implied by the editor
    checkValidMarkdown (String.trim counterArg)


validateRationaleConclusion : MarkdownForm -> Result String ()
validateRationaleConclusion _ =
    -- Nothing to check really
    Ok ()


validateRationaleInternVote : InternalVote -> Result String ()
validateRationaleInternVote internVote =
    -- Check that all numbers are positive
    if internVote.constitutional < 0 then
        Err "Constitutional internal vote must be >= 0"

    else if internVote.unconstitutional < 0 then
        Err "Unconstitutional internal vote must be >= 0"

    else if internVote.abstain < 0 then
        Err "Abstain internal vote must be >= 0"

    else if internVote.didNotVote < 0 then
        Err "DidNotVote internal vote must be >= 0"

    else
        Ok ()


validateRationaleRefs : List Reference -> Result String ()
validateRationaleRefs references =
    let
        -- Check that there are no duplicate label in the references
        validateNoDuplicate =
            case findDuplicate (List.map .label references) of
                Just dup ->
                    Err ("There is a duplicate label in the references: " ++ dup)

                Nothing ->
                    Ok ()

        -- TODO: Check that URIs seem valid
        checkUris _ =
            Ok ()
    in
    validateNoDuplicate
        |> Result.andThen
            (\_ ->
                List.map checkUris references
                    |> reduceResults
                    |> Result.map (always ())
            )


rationaleFromForm : RationaleForm -> Rationale
rationaleFromForm form =
    let
        cleanup str =
            case String.trim str of
                "" ->
                    Nothing

                trimmed ->
                    Just trimmed
    in
    { summary = String.trim form.summary
    , rationaleStatement = String.trim form.rationaleStatement
    , precedentDiscussion = cleanup form.precedentDiscussion
    , counterArgumentDiscussion = cleanup form.counterArgumentDiscussion
    , conclusion = cleanup form.conclusion
    , internalVote = form.internalVote
    , references = form.references
    }


editRationale : Step RationaleForm {} Rationale -> Step RationaleForm {} Rationale
editRationale step =
    case step of
        Preparing _ ->
            step

        Validating rationaleForm _ ->
            Preparing rationaleForm

        Done prep _ ->
            Preparing prep



-- Rationale Signature Step


resetRationaleSignatures : Rationale -> Step RationaleSignatureForm {} RationaleSignature -> RationaleSignatureForm
resetRationaleSignatures rationale step =
    let
        newRatSig authors =
            { authors = List.map (\a -> { a | signature = Nothing }) authors
            , rationale = rationale
            , error = Nothing
            }
    in
    case step of
        Preparing { authors } ->
            newRatSig authors

        Validating { authors } _ ->
            newRatSig authors

        Done _ { authors } ->
            newRatSig authors


encodeJsonLdRationale : Rationale -> JE.Value
encodeJsonLdRationale rationale =
    JE.object <|
        List.filterMap identity
            [ Just ( "summary", JE.string rationale.summary )
            , Just ( "rationaleStatement", JE.string rationale.rationaleStatement )
            , Maybe.map (\s -> ( "precedentDiscussion", JE.string s )) rationale.precedentDiscussion
            , Maybe.map (\s -> ( "counterargumentDiscussion", JE.string s )) rationale.counterArgumentDiscussion
            , Maybe.map (\s -> ( "conclusion", JE.string s )) rationale.conclusion
            , if rationale.internalVote == noInternalVote then
                Nothing

              else
                Just ( "internalVote", encodeInternalVote rationale.internalVote )
            , if List.isEmpty rationale.references then
                Nothing

              else
                Just ( "references", JE.list encodeReference rationale.references )
            ]


encodeInternalVote : InternalVote -> JE.Value
encodeInternalVote { constitutional, unconstitutional, abstain, didNotVote } =
    JE.object
        [ ( "constitutional", JE.int constitutional )
        , ( "unconstitutional", JE.int unconstitutional )
        , ( "abstain", JE.int abstain )
        , ( "didNotVote", JE.int didNotVote )
        ]


encodeReference : Reference -> JE.Value
encodeReference ref =
    JE.object
        [ ( "@type", encodeRefType ref.type_ )
        , ( "label", JE.string ref.label )
        , ( "uri", JE.string ref.uri )
        ]


encodeRefType : ReferenceType -> JE.Value
encodeRefType refType =
    case refType of
        OtherRefType ->
            JE.string "Other"

        GovernanceMetadataRefType ->
            JE.string "GovernanceMetadata"

        RelevantArticlesRefType ->
            JE.string "RelevantArticles"


updateAuthorsForm : (List AuthorWitness -> List AuthorWitness) -> Model -> Model
updateAuthorsForm f ({ rationaleSignatureStep } as model) =
    case rationaleSignatureStep of
        Preparing form ->
            { model
                | rationaleSignatureStep =
                    Preparing { form | authors = f form.authors, error = Nothing }
            }

        _ ->
            model


handleJsonSignatureFileRead : Int -> String -> Result x String -> Msg
handleJsonSignatureFileRead n authorName result =
    case result of
        Err _ ->
            NoMsg

        Ok json ->
            LoadedAuthorSignatureJsonRationale n authorName json


authorWitnessExtractDecoder : String -> JD.Decoder AuthorWitness
authorWitnessExtractDecoder authorName =
    JD.field "authors" (JD.list authorWitnessDecoder)
        |> JD.andThen
            (\authors ->
                case List.head <| List.filter (\a -> a.name == authorName) authors of
                    Just author ->
                        JD.succeed author

                    Nothing ->
                        JD.fail <| "No witness found for author: " ++ authorName
            )


authorWitnessDecoder : JD.Decoder AuthorWitness
authorWitnessDecoder =
    JD.map4
        (\name witnessAlgorithm publicKey signature ->
            { name = name
            , witnessAlgorithm = witnessAlgorithm
            , publicKey = publicKey
            , signature = signature
            }
        )
        (JD.field "name" JD.string)
        (JD.at [ "witness", "witnessAlgorithm" ] JD.string)
        (JD.at [ "witness", "publicKey" ] JD.string)
        (JD.map Just <| JD.at [ "witness", "signature" ] JD.string)


signatureDecodingError : JD.Error -> Step RationaleSignatureForm {} RationaleSignature -> Step RationaleSignatureForm {} RationaleSignature
signatureDecodingError decodingError rationaleSignatureStep =
    case rationaleSignatureStep of
        Preparing form ->
            Preparing { form | error = Just <| JD.errorToString decodingError }

        _ ->
            rationaleSignatureStep


skipRationaleSignature : JsonLdContexts -> Step RationaleSignatureForm {} RationaleSignature -> Step RationaleSignatureForm {} RationaleSignature
skipRationaleSignature jsonLdContexts step =
    case step of
        Preparing ({ authors } as form) ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    { form | authors = List.map (\a -> { a | signature = Nothing }) authors }
                        |> rationaleSignatureFromForm jsonLdContexts
                        |> Done form

        Validating ({ authors } as form) _ ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    { form | authors = List.map (\a -> { a | signature = Nothing }) authors }
                        |> rationaleSignatureFromForm jsonLdContexts
                        |> Done form

        Done _ _ ->
            step


validateRationaleSignature : JsonLdContexts -> Model -> ( Model, Cmd msg, Maybe MsgToParent )
validateRationaleSignature jsonLdContexts model =
    case model.rationaleSignatureStep of
        Preparing ({ authors } as form) ->
            case validateAuthorsForm authors of
                Err error ->
                    ( { model | rationaleSignatureStep = Preparing { form | error = Just error } }
                    , Cmd.none
                    , Nothing
                    )

                Ok _ ->
                    -- TODO: change to Validating instead, and emit a command to check signatures
                    ( { model | rationaleSignatureStep = Done form <| rationaleSignatureFromForm jsonLdContexts form }
                    , Cmd.none
                    , Nothing
                    )

        Validating _ _ ->
            ( model, Cmd.none, Nothing )

        Done _ _ ->
            ( model, Cmd.none, Nothing )


rationaleSignatureFromForm : JsonLdContexts -> RationaleSignatureForm -> RationaleSignature
rationaleSignatureFromForm jsonLdContexts form =
    { authors = form.authors
    , rationale = form.rationale
    , signedJson =
        createJsonRationale jsonLdContexts form.rationale form.authors
            |> JE.encode 0
    }


rationaleSignatureToForm : Step RationaleSignatureForm {} RationaleSignature -> RationaleSignatureForm
rationaleSignatureToForm step =
    case step of
        Preparing form ->
            form

        Validating form _ ->
            form

        Done form _ ->
            form


validateAuthorsForm : List AuthorWitness -> Result String ()
validateAuthorsForm authors =
    let
        -- Check that there are no duplicate names in the authors
        validateNoDuplicate =
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Err ("There is a duplicate name in the authors list: " ++ dup)

                Nothing ->
                    Ok ()

        -- Check that witnessAlgorithm are authorized by the CIP
        authorizedAlgorithms =
            Set.singleton "ed25519"

        checkWitnessAlgo algo =
            if Set.member algo authorizedAlgorithms then
                Ok ()

            else
                Err ("The witness algorithm (" ++ algo ++ ") is not in the authorized standard list: " ++ String.join ", " (Set.toList authorizedAlgorithms))
    in
    validateNoDuplicate
        |> Result.andThen
            (\_ ->
                List.map .witnessAlgorithm authors
                    |> List.map checkWitnessAlgo
                    |> reduceResults
                    |> Result.map (always ())
            )


findDuplicate : List comparable -> Maybe comparable
findDuplicate list =
    findDuplicateHelper list Set.empty


findDuplicateHelper : List comparable -> Set comparable -> Maybe comparable
findDuplicateHelper list seen =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if Set.member x seen then
                Just x

            else
                findDuplicateHelper xs (Set.insert x seen)


reduceResults : List (Result a b) -> Result a (List b)
reduceResults results =
    List.foldr
        (\result acc ->
            case ( result, acc ) of
                ( Err err, _ ) ->
                    Err err

                ( Ok value, Ok values ) ->
                    Ok (value :: values)

                ( _, Err err ) ->
                    Err err
        )
        (Ok [])
        results



-- Permanent Storage Step


updateStorageForm : (StorageForm -> StorageForm) -> Model -> Model
updateStorageForm formUpdate model =
    case model.permanentStorageStep of
        Preparing form ->
            { model | permanentStorageStep = Preparing <| formUpdate form }

        Validating _ _ ->
            model

        Done _ _ ->
            model


validateIpfsFormAndSendPinRequest : UpdateContext msg -> StorageForm -> Model -> ( Model, Cmd msg, Maybe MsgToParent )
validateIpfsFormAndSendPinRequest ctx form model =
    case ( model.rationaleSignatureStep, validateIpfsForm form ) of
        ( Done _ ratSig, Ok _ ) ->
            ( { model | permanentStorageStep = Validating form {} }
            , ctx.jsonRationaleToFile
                { fileContent = ratSig.signedJson
                , fileName = "rationale-signed.json"
                }
            , Nothing
            )

        ( Done _ _, Err error ) ->
            ( { model | permanentStorageStep = Preparing { form | error = Just error } }
            , Cmd.none
            , Nothing
            )

        _ ->
            ( { model | permanentStorageStep = Preparing { form | error = Just "Validate the rationale signature step first." } }
            , Cmd.none
            , Nothing
            )


validateIpfsForm : StorageForm -> Result String ()
validateIpfsForm form =
    let
        -- Check that the IPFS server url looks legit
        ipfsServerUrlSeemsLegit =
            case Url.fromString form.ipfsServer of
                Just _ ->
                    Ok ()

                Nothing ->
                    Err ("This url seems incorrect, it must look like this: https://subdomain.domain.org, instead I got this: " ++ form.ipfsServer)

        -- Check that headers look valid
        -- There are many rules, but will just check they aren’t empty
        nonEmptyHeadersResult headers =
            if List.any (\( f, _ ) -> String.isEmpty f) headers then
                Err "Empty header fields are forbidden."

            else
                Ok ()
    in
    ipfsServerUrlSeemsLegit
        |> Result.andThen (\_ -> nonEmptyHeadersResult form.headers)


pinRationaleFile : JD.Value -> Model -> ( Model, Cmd Msg )
pinRationaleFile fileAsValue model =
    case ( JD.decodeValue File.decoder fileAsValue, model.permanentStorageStep ) of
        ( Err error, Validating form _ ) ->
            ( { model | permanentStorageStep = Preparing { form | error = Just <| JD.errorToString error } }
            , Cmd.none
            )

        ( Ok file, Validating storageForm _ ) ->
            ( model
            , Api.defaultApiProvider.ipfsAdd
                { rpc = storageForm.ipfsServer
                , headers = storageForm.headers
                , file = file
                }
                GotIpfsAnswer
            )

        -- Ignore if we aren’t validating the permanent storage step
        _ ->
            ( model, Cmd.none )


handleIpfsAnswer : Model -> StorageForm -> IpfsAnswer -> ( Model, Cmd msg, Maybe MsgToParent )
handleIpfsAnswer model form ipfsAnswer =
    case ipfsAnswer of
        IpfsError error ->
            ( { model | permanentStorageStep = Preparing { form | error = Just error } }
            , Cmd.none
            , Nothing
            )

        IpfsAddSuccessful file ->
            ( { model | permanentStorageStep = Done form { config = form, jsonFile = file } }
            , Cmd.none
            , Nothing
            )



-- Fee Provider Step


updateFeeProviderForm : FeeProviderForm -> Model -> Model
updateFeeProviderForm form model =
    case model.feeProviderStep of
        Preparing _ ->
            { model | feeProviderStep = Preparing form }

        _ ->
            model


{-| Check if the external endpoint seems legit.
-}
validateFeeProviderForm : Maybe LoadedWallet -> FeeProviderForm -> Step FeeProviderForm FeeProviderTemp FeeProvider
validateFeeProviderForm maybeWallet feeProviderForm =
    case ( maybeWallet, feeProviderForm ) of
        ( Nothing, ConnectedWalletFeeProvider _ ) ->
            Preparing (ConnectedWalletFeeProvider { error = Just "No wallet connected, please connect a wallet first." })

        ( Just { changeAddress, utxos }, ConnectedWalletFeeProvider _ ) ->
            Done feeProviderForm { address = changeAddress, utxos = utxos }

        ( _, ExternalFeeProvider { endpoint } ) ->
            case Url.fromString endpoint of
                Just _ ->
                    Validating feeProviderForm { address = Nothing, utxos = Nothing }

                Nothing ->
                    Preparing (ExternalFeeProvider { endpoint = endpoint, error = Just <| "The endpoint does not look like a valid URL: " ++ endpoint })



-- Build Tx Step


type alias TxRequirements =
    { voter : VoterWitness
    , actionId : ActionId
    , rationaleAnchor : Anchor
    , localStateUtxos : Utxo.RefDict Output
    , feeProviderAddress : Address
    , costModels : CostModels
    }


allPrepSteps : Maybe CostModels -> Model -> Result String TxRequirements
allPrepSteps maybeCostModels m =
    case ( maybeCostModels, ( m.voterStep, m.pickProposalStep, m.rationaleSignatureStep ), ( m.permanentStorageStep, m.feeProviderStep ) ) of
        ( Just costModels, ( Done _ voter, Done _ p, Done _ r ), ( Done _ s, Done _ f ) ) ->
            Ok
                { voter = voter
                , actionId = p.id
                , rationaleAnchor =
                    { url = "ipfs://" ++ s.jsonFile.cid
                    , dataHash =
                        Bytes.fromText r.signedJson
                            |> Bytes.toU8
                            |> blake2b256 Nothing
                            |> Bytes.fromU8
                    }
                , localStateUtxos = Dict.Any.union m.someRefUtxos f.utxos
                , feeProviderAddress = f.address
                , costModels = costModels
                }

        ( Nothing, _, _ ) ->
            Err "Somehow cost models are missing, please report this issue"

        _ ->
            Err "Incomplete steps before Tx building"



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , walletChangeAddress : Maybe Address
    , proposals : WebData (Dict String ActiveProposal)
    , jsonLdContexts : JsonLdContexts
    , costModels : Maybe CostModels
    , signingLink : Transaction -> List (Bytes CredentialHash) -> List (Html msg) -> Html msg
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div []
        [ Html.h2 [] [ text "Vote Preparation" ]
        , viewVoterIdentificationStep ctx model.voterStep
        , Html.hr [] []
        , viewProposalSelectionStep ctx model
        , Html.hr [] []
        , viewRationaleStep ctx model.rationaleCreationStep
        , Html.hr [] []
        , viewRationaleSignatureStep ctx model.rationaleCreationStep model.rationaleSignatureStep
        , Html.hr [] []
        , viewPermanentStorageStep ctx model.rationaleSignatureStep model.permanentStorageStep
        , Html.hr [] []
        , viewFeeProviderStep ctx model.feeProviderStep
        , Html.hr [] []
        , viewBuildTxStep ctx model
        , Html.hr [] []
        , viewSignTxStep ctx model.buildTxStep
        ]



--
-- Voter Identification Step
--


viewVoterIdentificationStep : ViewContext msg -> Step VoterPreparationForm VoterWitness VoterWitness -> Html msg
viewVoterIdentificationStep ctx step =
    case step of
        Preparing form ->
            Html.map ctx.wrapMsg <|
                div []
                    [ Html.p [] [ textField "Voter governance ID (drep/pool/cc_hot)" (Maybe.withDefault "" <| Maybe.map Gov.idToBech32 form.govId) VoterGovIdChange ]
                    , Html.Lazy.lazy viewValidGovIdForm form
                    , Html.p [] [ button [ onClick <| ValidateVoterFormButtonClicked ] [ text "Confirm Voter" ] ]
                    , viewError form.error
                    ]

        Validating _ _ ->
            div []
                [ Html.p [] [ text "validating voter information ..." ]
                ]

        Done form voter ->
            Html.map ctx.wrapMsg <| viewIdentifiedVoter form voter


viewValidGovIdForm : VoterPreparationForm -> Html Msg
viewValidGovIdForm form =
    let
        viewVotingPower accessor webData =
            case webData of
                RemoteData.NotAsked ->
                    text "not querried"

                RemoteData.Loading ->
                    text "loading ..."

                RemoteData.Failure _ ->
                    text <| "? Most likely, this voter is inactive, or not registered yet, or was just registered this epoch."

                RemoteData.Success success ->
                    text <| Helper.prettyAdaLovelace <| Natural.fromSafeInt <| accessor success
    in
    case form.govId of
        Nothing ->
            text ""

        -- First the easy case: voting with a key
        Just (CcHotCredId (VKeyHash hash)) ->
            div []
                [ Html.p [] [ text <| "Voting as a CC member with a hot key of hash: " ++ Bytes.toHex hash ]
                , viewCcInfo form.ccInfo
                ]

        Just (DrepId (VKeyHash hash)) ->
            div []
                [ Html.p [] [ text <| "Voting as a DRep with a key of hash: " ++ Bytes.toHex hash ]
                , Html.p []
                    [ text "Voting power: "
                    , viewVotingPower .votingPower form.drepInfo
                    ]
                ]

        Just (PoolId hash) ->
            div []
                [ Html.p [] [ text <| "Voting as a SPO with pool ID (hex): " ++ Bytes.toHex hash ]
                , Html.p []
                    [ text "Live stake: "
                    , viewVotingPower .stake form.poolInfo
                    ]
                ]

        -- Then the hard case: voting with a script
        Just (CcHotCredId (ScriptHash hash)) ->
            div []
                [ Html.p [] [ text <| "Voting as a CC member with a script of hash: " ++ Bytes.toHex hash ]
                , viewCcInfo form.ccInfo
                , viewScriptForm form
                ]

        Just (DrepId (ScriptHash hash)) ->
            div []
                [ Html.p [] [ text <| "Voting as a DRep with a script of hash: " ++ Bytes.toHex hash ]
                , Html.p []
                    [ text "Voting power: "
                    , viewVotingPower .votingPower form.drepInfo
                    ]
                , viewScriptForm form
                ]

        Just govId ->
            Html.p [] [ text <| "Unexpected type of governance Id: " ++ Debug.toString govId ]


viewCcInfo : WebData CcInfo -> Html msg
viewCcInfo remoteCcInfo =
    case remoteCcInfo of
        RemoteData.Success { coldCred, hotCred, status, epochMandateEnd } ->
            div []
                [ Html.p [] [ text <| "Cold credential: " ++ Gov.idToBech32 (CcColdCredId coldCred) ]
                , Html.p [] [ text <| "Hot credential (used to vote): " ++ Gov.idToBech32 (CcHotCredId hotCred) ]
                , Html.p [] [ text <| "Member status: " ++ status ]
                , Html.p [] [ text <| "Mandate ending at epoch: " ++ String.fromInt epochMandateEnd ]
                ]

        RemoteData.NotAsked ->
            Html.p [] [ text "CC member info not querried" ]

        RemoteData.Loading ->
            Html.p [] [ text "CC member info loading ..." ]

        RemoteData.Failure error ->
            Html.p [] [ text <| "CC member info loading error: " ++ Debug.toString error ]


viewScriptForm : VoterPreparationForm -> Html Msg
viewScriptForm { scriptInfo, utxoRef, expectedSigners } =
    case scriptInfo of
        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            Html.p [] [ text "Loading info for script ..." ]

        RemoteData.Failure err ->
            Html.p [] [ text <| "Error while loading script info: " ++ Debug.toString err ]

        RemoteData.Success { scriptHash, script, nativeCborEncodingMatchesHash } ->
            let
                utxoRefForm =
                    Html.p [] [ textField "Reference UTxO" utxoRef UtxoRefChange ]

                refScriptFeeSavings =
                    Transaction.estimateRefScriptFeeSavings script

                refScriptSuggestion =
                    -- Suggest reference script for more than ₳0.005 savings
                    if refScriptFeeSavings >= 5000 then
                        div []
                            [ Html.p [] [ text <| "By using a reference input for your script, you could save this much in Tx fees: " ++ prettyAdaLovelace (Natural.fromSafeInt refScriptFeeSavings) ]
                            , utxoRefForm
                            ]
                        -- Print warning for more than ₳0.005 additional cost of using a reference script

                    else if refScriptFeeSavings <= -5000 then
                        Html.p [] [ text <| "Weirdly, using a reference input for your script would cost you more: " ++ prettyAdaLovelace (Natural.fromSafeInt -refScriptFeeSavings) ]
                        -- Just ignore if it doesn’t affect the fees in any significant way

                    else
                        text ""

                additionalSignerCost =
                    Natural.fromSafeInt <|
                        Transaction.defaultTxFeeParams.feePerByte
                            * additionalBytesPerSignature

                additionalBytesPerSignature =
                    Transaction.encodeVKeyWitness
                        { vkey = Bytes.dummy 32 "", signature = Bytes.dummy 64 "" }
                        |> Cbor.Encode.encode
                        |> Bytes.fromBytes
                        |> Bytes.width
            in
            case script of
                Script.Native _ ->
                    if nativeCborEncodingMatchesHash == Just True then
                        div []
                            [ Html.p [] [ text "Type of script: Native Script" ]
                            , refScriptSuggestion
                            , Html.p [] [ text <| "Expected signers: (each adds " ++ prettyAdaLovelace additionalSignerCost ++ " to the Tx fees)" ]
                            , div [] (List.map viewExpectedSignerCheckbox <| Dict.values expectedSigners)
                            ]

                    else
                        div []
                            [ Html.p [] [ text "Type of script: Native Script" ]
                            , Html.p [] [ text <| "IMPORTANT: for technical reasons, we need you to provide a reference UTxO containing your script of hash: " ++ Bytes.toHex scriptHash ]
                            , utxoRefForm
                            , Html.p [] [ text <| "Expected signers: (each adds " ++ prettyAdaLovelace additionalSignerCost ++ " to the Tx fees)" ]
                            , div [] (List.map viewExpectedSignerCheckbox <| Dict.values expectedSigners)
                            ]

                Script.Plutus plutusScript ->
                    div []
                        [ Html.p [] [ text <| "Plutus script version: " ++ Debug.toString plutusScript.version ]
                        , Html.p [] [ text <| "Script size: " ++ (String.fromInt <| Bytes.width plutusScript.script) ++ " Bytes" ]
                        , refScriptSuggestion
                        , Html.p [] [ text "WIP: we are waiting for someone needing this to implement Plutus voters" ]
                        ]


viewExpectedSignerCheckbox : { expected : Bool, key : Bytes CredentialHash } -> Html Msg
viewExpectedSignerCheckbox { expected, key } =
    let
        keyHex =
            Bytes.toHex key
    in
    Html.p []
        [ Html.input
            [ HA.type_ "checkbox"
            , HA.id keyHex
            , HA.name keyHex
            , HA.checked expected
            , onCheck (ToggleExpectedSigner keyHex)
            ]
            []
        , Html.label [ HA.for keyHex ] [ text <| " key hash: " ++ keyHex ]
        ]


textField : String -> String -> (String -> msg) -> Html msg
textField label value toMsg =
    Html.span []
        [ Html.label [] [ text <| label ++ " " ]
        , Html.input
            [ HA.type_ "text"
            , HA.value value
            , Html.Events.onInput toMsg
            ]
            []
        ]


viewIdentifiedVoter : VoterPreparationForm -> VoterWitness -> Html Msg
viewIdentifiedVoter form voter =
    let
        govIdStr =
            Maybe.map Gov.idToBech32 form.govId
                |> Maybe.withDefault ""

        ( voterTypeText, voterCred ) =
            case voter of
                WithCommitteeHotCred cred ->
                    ( "Constitutional Committee Voter: " ++ govIdStr, cred )

                WithDrepCred cred ->
                    let
                        votingPowerStr =
                            case form.drepInfo of
                                RemoteData.Success { votingPower } ->
                                    Helper.prettyAdaLovelace (Natural.fromSafeInt votingPower)

                                _ ->
                                    "?"
                    in
                    ( "DRep Voter (voting power: " ++ votingPowerStr ++ "): " ++ govIdStr, cred )

                WithPoolCred hash ->
                    let
                        votingPower =
                            case form.poolInfo of
                                RemoteData.Success { stake } ->
                                    Helper.prettyAdaLovelace (Natural.fromSafeInt stake)

                                _ ->
                                    "?"
                    in
                    ( "SPO Voter (voting power: " ++ votingPower ++ "): " ++ Pool.toBech32 hash
                    , WithKey hash
                    )
    in
    div []
        [ Html.p [] [ text voterTypeText ]
        , case voterCred of
            WithKey cred ->
                Html.p [] [ text <| "Using the key of hash: " ++ Bytes.toHex cred ]

            WithScript hash (NativeWitness { expectedSigners }) ->
                div []
                    [ Html.p []
                        [ text <| "Using a native script (hash: " ++ Bytes.toHex hash ++ ")"
                        , text " and expecting the following signers:"
                        ]
                    , Html.ul [] (List.map (\s -> Html.li [] [ text <| "key hash: " ++ Bytes.toHex s ]) expectedSigners)
                    ]

            WithScript _ (PlutusWitness _) ->
                Html.p [] [ text "TODO: display Plutus script witness" ]
        , Html.p [] [ button [ onClick <| ChangeVoterButtonClicked ] [ text "Change Voter" ] ]
        ]



--
-- Proposal Selection Step
--


viewProposalSelectionStep : ViewContext msg -> Model -> Html msg
viewProposalSelectionStep ctx model =
    case model.pickProposalStep of
        Preparing _ ->
            div []
                [ Html.h3 [] [ text "Pick a Proposal" ]
                , case ctx.proposals of
                    RemoteData.NotAsked ->
                        text "Proposals are not loading, please report this error."

                    RemoteData.Loading ->
                        text "Proposals loading ..."

                    RemoteData.Failure httpError ->
                        Html.pre []
                            [ text "Something went wrong while loading proposals."
                            , text <| Debug.toString httpError
                            ]

                    RemoteData.Success proposalsDict ->
                        -- Sorted by ActionId for now
                        Dict.values proposalsDict
                            |> List.map viewActiveProposal
                            |> div []
                            |> Html.map ctx.wrapMsg
                ]

        Validating _ _ ->
            div []
                [ Html.h3 [] [ text "Pick a Proposal" ]
                , Html.p [] [ text "Validating the picked proposal ..." ]
                ]

        Done _ { id, actionType, metadata, metadataUrl } ->
            let
                ( title, content ) =
                    case metadata of
                        RemoteData.NotAsked ->
                            ( "not loading", Nothing )

                        RemoteData.Loading ->
                            ( "loading ...", Nothing )

                        RemoteData.Failure error ->
                            ( "ERROR for " ++ metadataUrl ++ ": " ++ Debug.toString error
                            , Nothing
                            )

                        RemoteData.Success meta ->
                            ( meta.body.title |> Maybe.withDefault "unknown (unexpected metadata format)"
                            , Just <|
                                div []
                                    [ Html.p [] [ text "Abstract:" ]
                                    , Html.p [] [ text <| Maybe.withDefault "Unknown abstract (unexpected metadata format)" meta.body.abstract ]
                                    , Html.p [] [ text "Raw metadata:" ]
                                    , Html.pre [] [ text meta.raw ]
                                    ]
                            )
            in
            div []
                [ Html.h3 [] [ text "Pick a Proposal" ]
                , div []
                    [ Html.p [] [ text "Picked: ", cardanoScanActionLink id ]
                    , Html.p [] [ text <| "Type: " ++ actionType ]
                    , Html.p [] [ Html.strong [] [ text <| "Title: " ++ title ] ]
                    , content
                        |> Maybe.withDefault (text "")
                    ]
                , Html.p [] [ button [ onClick <| ctx.wrapMsg ChangeProposalButtonClicked ] [ text "Change Proposal" ] ]
                ]


viewActiveProposal : ActiveProposal -> Html Msg
viewActiveProposal { id, actionType, metadata, metadataUrl } =
    Html.p []
        [ button [ onClick (PickProposalButtonClicked <| Gov.actionIdToString id) ] [ text "Pick this proposal" ]
        , text " "
        , cardanoScanActionLink id
        , text <| ", type: " ++ actionType
        , text ", title: "
        , text <|
            case metadata of
                RemoteData.NotAsked ->
                    "not loading"

                RemoteData.Loading ->
                    "loading ..."

                RemoteData.Failure error ->
                    "ERROR for " ++ metadataUrl ++ ": " ++ Debug.toString error

                RemoteData.Success meta ->
                    meta.body.title
                        |> Maybe.withDefault "unknown (unexpected metadata format)"
        ]


cardanoScanActionLink : ActionId -> Html msg
cardanoScanActionLink id =
    Html.a
        [ HA.href <|
            "https://preview.cardanoscan.io/govAction/"
                ++ (id.transactionId |> Bytes.toHex)
                ++ (Bytes.toHex <| Bytes.fromBytes <| Cbor.Encode.encode (Cbor.Encode.int id.govActionIndex))
        , HA.target "_blank"
        , HA.rel "noopener noreferrer"
        ]
        [ text <| "Id: " ++ (strBothEnds 8 8 <| Bytes.toHex id.transactionId)
        , text <| "#" ++ String.fromInt id.govActionIndex
        ]


strBothEnds : Int -> Int -> String -> String
strBothEnds startLength endLength str =
    -- Helper function to only display the start and end of a long string
    let
        strLength =
            String.length str
    in
    if strLength <= startLength + endLength then
        str

    else
        String.slice 0 startLength str
            ++ "..."
            ++ String.slice (strLength - endLength) strLength str



--
-- Rationale Step
--


viewRationaleStep : ViewContext msg -> Step RationaleForm {} Rationale -> Html msg
viewRationaleStep ctx step =
    Html.map ctx.wrapMsg <|
        case step of
            Preparing form ->
                div []
                    [ Html.h3 [] [ text "Vote Rationale" ]
                    , viewSummaryForm form.summary
                    , viewStatementForm form.rationaleStatement
                    , viewPrecedentDiscussionForm form.precedentDiscussion
                    , viewCounterArgumentForm form.counterArgumentDiscussion
                    , viewConclusionForm form.conclusion
                    , viewInternalVoteForm form.internalVote
                    , viewReferencesForm form.references
                    , Html.p [] [ Html.button [ onClick ValidateRationaleButtonClicked ] [ text "Confirm rationale" ] ]
                    , viewError form.error
                    ]

            Validating _ _ ->
                div []
                    [ Html.h3 [] [ text "Vote Rationale" ]
                    , Html.p [] [ text "validating rationale data ..." ]
                    , Html.p [] [ button [ onClick EditRationaleButtonClicked ] [ text "Edit rationale" ] ]
                    ]

            Done _ rationale ->
                div []
                    [ Html.h3 [] [ text "Vote Rationale" ]
                    , viewSummary rationale.summary
                    , viewStatementMd rationale.rationaleStatement
                    , viewPrecedentDiscussionMd rationale.precedentDiscussion
                    , viewCounterArgumentMd rationale.counterArgumentDiscussion
                    , viewConclusion rationale.conclusion
                    , viewInternalVote rationale.internalVote
                    , viewReferences rationale.references
                    , Html.p [] [ button [ onClick EditRationaleButtonClicked ] [ text "Edit rationale" ] ]
                    ]


viewSummaryForm : MarkdownForm -> Html Msg
viewSummaryForm form =
    div []
        [ Html.h4 [] [ text "Summary" ]
        , Html.p [] [ text "Compulsory." ]
        , Html.p [] [ text "Clearly state your stance, summarize your rationale with your main argument." ]
        , Html.p [] [ text "Limited to 300 characters, does NOT support markdown." ]
        , div []
            [ Html.textarea
                [ HA.value form
                , Html.Events.onInput RationaleSummaryChange
                ]
                []
            ]
        ]


viewStatementForm : MarkdownForm -> Html Msg
viewStatementForm form =
    div []
        [ Html.h4 [] [ text "Rationale Statement" ]
        , Html.p [] [ text "Compulsory." ]
        , Html.p [] [ text "Fully describe your rationale, with your arguments in full details." ]
        , Html.p [] [ text "No size limit and markdown is supported (preview below)." ]
        , div []
            [ Html.textarea
                [ HA.value form
                , Html.Events.onInput RationaleStatementChange
                ]
                []
            ]
        , viewMdBelowForm form
        ]


viewMdBelowForm : String -> Html msg
viewMdBelowForm form =
    case String.trim form of
        "" ->
            text ""

        str ->
            div [ HA.style "background-color" "#eeeeee", HA.style "padding" "0.01rem" ]
                [ viewMd str ]


viewPrecedentDiscussionForm : MarkdownForm -> Html Msg
viewPrecedentDiscussionForm form =
    div []
        [ Html.h4 [] [ text "Precedent Discussion" ]
        , Html.p [] [ text "Optional." ]
        , Html.p [] [ text "Discuss what you feel is relevant precedent." ]
        , Html.p [] [ text "No size limit and markdown is supported (preview below)." ]
        , div []
            [ Html.textarea
                [ HA.value form
                , Html.Events.onInput PrecedentDiscussionChange
                ]
                []
            ]
        , viewMdBelowForm form
        ]


viewCounterArgumentForm : MarkdownForm -> Html Msg
viewCounterArgumentForm form =
    div []
        [ Html.h4 [] [ text "Counter Argument Discussion" ]
        , Html.p [] [ text "Optional." ]
        , Html.p [] [ text "Discuss significant counter arguments to your position." ]
        , Html.p [] [ text "No size limit and markdown is supported (preview below)." ]
        , div []
            [ Html.textarea
                [ HA.value form
                , Html.Events.onInput CounterArgumentChange
                ]
                []
            ]
        , viewMdBelowForm form
        ]


viewConclusionForm : MarkdownForm -> Html Msg
viewConclusionForm form =
    div []
        [ Html.h4 [] [ text "Conclusion" ]
        , Html.p [] [ text "Optional." ]
        , Html.p [] [ text "No size limit, does NOT support markdown." ]
        , div []
            [ Html.textarea
                [ HA.value form
                , Html.Events.onInput ConclusionChange
                ]
                []
            ]
        ]


viewInternalVoteForm : InternalVote -> Html Msg
viewInternalVoteForm { constitutional, unconstitutional, abstain, didNotVote } =
    div []
        [ Html.h4 [] [ text "Internal Vote" ]
        , Html.p [] [ text "If you vote as a group, you can report the group internal votes." ]
        , Helper.viewNumberInput "Constitutional: " constitutional InternalConstitutionalVoteChange
        , Helper.viewNumberInput "Unconstitutional: " unconstitutional InternalUnconstitutionalVoteChange
        , Helper.viewNumberInput "Abstain: " abstain InternalAbstainVoteChange
        , Helper.viewNumberInput "Did not vote: " didNotVote InternalDidNotVoteChange
        ]


viewReferencesForm : List Reference -> Html Msg
viewReferencesForm references =
    div []
        [ Html.h4 [] [ text "References" ]
        , Html.p [] [ button [ onClick AddRefButtonClicked ] [ text "Add a reference" ] ]
        , div [] (List.indexedMap viewOneRefForm references)
        ]


viewOneRefForm : Int -> Reference -> Html Msg
viewOneRefForm n reference =
    Html.p []
        [ button [ onClick (DeleteRefButtonClicked n) ] [ text "Delete" ]
        , Html.label [ HA.for "ref-type" ] [ text " reference type: " ]
        , Html.select
            [ HA.id "ref-type"
            , HA.value (refTypeToString reference.type_)
            , Html.Events.onInput (ReferenceTypeChange n)
            ]
            (List.map viewRefOption allRefTypes)
        , Html.text " label: "
        , Html.input
            [ HA.type_ "text"
            , HA.value reference.label
            , Html.Events.onInput (ReferenceLabelChange n)
            ]
            []
        , Html.text " uri: "
        , Html.input
            [ HA.type_ "text"
            , HA.value reference.uri
            , Html.Events.onInput (ReferenceUriChange n)
            ]
            []
        ]


viewRefOption : ReferenceType -> Html Msg
viewRefOption refType =
    Html.option
        [ HA.value <| refTypeToString refType
        ]
        [ text <| refTypeToString refType ]


viewSummary : String -> Html msg
viewSummary summary =
    div []
        [ Html.h4 [] [ text "Summary" ]
        , Html.p [] [ text summary ]
        ]


viewStatementMd : String -> Html msg
viewStatementMd statement =
    div []
        [ Html.h4 [] [ text "Rationale Statement" ]
        , viewMd statement
        ]


viewMd : String -> Html msg
viewMd str =
    case Md.parse str of
        Err deadEnds ->
            let
                deadEndsString =
                    List.map Md.deadEndToString deadEnds
                        |> String.join "\n"
            in
            Html.p []
                [ Html.pre [] [ text "Unexpected error while parsing markdown:" ]
                , Html.pre [] [ text deadEndsString ]
                ]

        Ok blocks ->
            case Md.render Md.defaultHtmlRenderer blocks of
                Err errors ->
                    Html.p []
                        [ Html.pre [] [ text "Unexpected error while rendering markdown:" ]
                        , Html.pre [] [ text errors ]
                        ]

                Ok rendered ->
                    Html.p [] rendered


viewPrecedentDiscussionMd : Maybe String -> Html msg
viewPrecedentDiscussionMd maybeDiscussion =
    case maybeDiscussion of
        Nothing ->
            text ""

        Just discussion ->
            div []
                [ Html.h4 [] [ text "Precedent Discussion" ]
                , viewMd discussion
                ]


viewCounterArgumentMd : Maybe String -> Html msg
viewCounterArgumentMd maybeArgument =
    case maybeArgument of
        Nothing ->
            text ""

        Just argument ->
            div []
                [ Html.h4 [] [ text "Counter Argument" ]
                , viewMd argument
                ]


viewConclusion : Maybe String -> Html msg
viewConclusion maybeConclusion =
    case maybeConclusion of
        Nothing ->
            text ""

        Just conclusion ->
            div []
                [ Html.h4 [] [ text "Conclusion" ]
                , Html.p [] [ text conclusion ]
                ]


viewInternalVote : InternalVote -> Html msg
viewInternalVote ({ constitutional, unconstitutional, abstain, didNotVote } as internalVote) =
    if internalVote == noInternalVote then
        text ""

    else
        div []
            [ Html.h4 [] [ text "Internal Vote" ]
            , Html.ul []
                [ Html.li [] [ text <| "Constitutional: " ++ String.fromInt constitutional ]
                , Html.li [] [ text <| "Unconstitutional: " ++ String.fromInt unconstitutional ]
                , Html.li [] [ text <| "Abstain: " ++ String.fromInt abstain ]
                , Html.li [] [ text <| "Did not vote: " ++ String.fromInt didNotVote ]
                ]
            ]


viewReferences : List Reference -> Html msg
viewReferences references =
    if List.isEmpty references then
        text ""

    else
        div []
            [ Html.h4 [] [ text "References" ]
            , Html.ul [] (List.map viewRef references)
            ]


viewRef : Reference -> Html msg
viewRef ref =
    Html.li []
        [ text <| "ref type: " ++ refTypeToString ref.type_
        , text <| " , label: " ++ ref.label
        , text <| " , URI: " ++ ref.uri
        ]



--
-- Rationale Signature Step
--


viewRationaleSignatureStep :
    ViewContext msg
    -> Step RationaleForm {} Rationale
    -> Step RationaleSignatureForm {} RationaleSignature
    -> Html msg
viewRationaleSignatureStep ctx rationaleCreationStep step =
    case ( rationaleCreationStep, step ) of
        ( Preparing _, _ ) ->
            div []
                [ Html.h3 [] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please validate the rationale creation step first." ]
                ]

        ( Validating _ _, _ ) ->
            div []
                [ Html.h3 [] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please validate the rationale creation step first." ]
                ]

        ( Done _ _, Preparing form ) ->
            div []
                [ Html.h3 [] [ text "Rationale Signature" ]
                , Html.map ctx.wrapMsg <| viewRationaleSignatureForm ctx.jsonLdContexts form
                , Html.p []
                    [ button [ onClick <| ctx.wrapMsg SkipRationaleSignaturesButtonClicked ] [ text "Skip rationale signing" ]
                    , text " or "
                    , button [ onClick <| ctx.wrapMsg ValidateRationaleSignaturesButtonClicked ] [ text "Validate rationale signing" ]
                    ]
                , viewError form.error
                ]

        ( Done _ _, Validating _ _ ) ->
            div []
                [ Html.h3 [] [ text "Rationale Signature" ]
                , Html.p [] [ text "Validating rationale author signatures ..." ]
                ]

        ( Done _ _, Done _ ratSig ) ->
            let
                downloadButton =
                    Html.a
                        [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode ratSig.signedJson
                        , HA.download "rationale-signed.json"
                        ]
                        [ button [] [ text "Download signed JSON rationale" ] ]
            in
            if List.isEmpty ratSig.authors then
                Html.map ctx.wrapMsg <|
                    div []
                        [ Html.h3 [] [ text "Rationale Signature" ]
                        , Html.p [] [ downloadButton ]
                        , Html.p [] [ text "No registered author." ]
                        , Html.p [] [ button [ onClick ChangeAuthorsButtonClicked ] [ text "Update authors" ] ]
                        ]

            else
                Html.map ctx.wrapMsg <|
                    div []
                        [ Html.h3 [] [ text "Rationale Signature" ]
                        , Html.p [] [ downloadButton ]
                        , Html.ul [] (List.map viewSigner ratSig.authors)
                        , Html.p [] [ button [ onClick ChangeAuthorsButtonClicked ] [ text "Update authors" ] ]
                        ]


viewRationaleSignatureForm : JsonLdContexts -> RationaleSignatureForm -> Html Msg
viewRationaleSignatureForm jsonLdContexts ({ authors } as form) =
    let
        jsonRationale =
            (rationaleSignatureFromForm jsonLdContexts { form | authors = [] }).signedJson
    in
    div []
        [ Html.p [] [ text "Here is the JSON-LD rationale file generated from your rationale inputs." ]
        , Html.p []
            [ Html.a
                [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode jsonRationale
                , HA.download "rationale.json"
                ]
                [ button [] [ text "Download JSON rationale" ] ]
            ]
        , Html.h4 [] [ text "Authors" ]
        , Html.p []
            [ text "Each author needs to sign the above metadata."
            , text " For now, the only supported method is to download this json file, and sign it with cardano-signer."
            , text " Later I plan to add the ability to sign directly with the web wallet (like Eternl)."
            , Html.pre []
                [ text "cardano-signer.js sign --cip100 \\\n"
                , text "   --data-file rationale.json \\\n"
                , text "   --secret-key dummy.skey \\\n"
                , text "   --author-name \"The great Name\" \\\n"
                , text "   --out-file rationale-signed.json"
                ]
            ]
        , Html.p []
            [ text "Add individual authors that contributed to this rationale."
            , text " Provide each author signature or skip all signatures."
            ]
        , Html.p [] [ button [ onClick AddAuthorButtonClicked ] [ text "Add an author" ] ]
        , div [] (List.indexedMap viewOneAuthorForm authors)
        ]


createJsonRationale : JsonLdContexts -> Rationale -> List AuthorWitness -> JE.Value
createJsonRationale jsonLdContexts rationale authors =
    JE.object <|
        List.filterMap identity
            [ Just ( "@context", jsonLdContexts.ccCip136Context )
            , Just ( "hashAlgorithm", JE.string "blake2b-256" )
            , Just ( "body", encodeJsonLdRationale rationale )
            , if List.isEmpty authors then
                Nothing

              else
                Just <| ( "authors", JE.list encodeAuthorWitness authors )
            ]


encodeAuthorWitness : AuthorWitness -> JE.Value
encodeAuthorWitness { name, witnessAlgorithm, publicKey, signature } =
    case signature of
        Nothing ->
            JE.object [ ( "name", JE.string name ) ]

        Just sig ->
            JE.object
                [ ( "name", JE.string name )
                , ( "witnessAlgorithm", JE.string witnessAlgorithm )
                , ( "publicKey", JE.string publicKey )
                , ( "signature", JE.string sig )
                ]


viewOneAuthorForm : Int -> AuthorWitness -> Html Msg
viewOneAuthorForm n author =
    Html.p []
        [ button [ onClick (DeleteAuthorButtonClicked n) ] [ text "Delete" ]
        , Html.text " name: "
        , Html.input
            [ HA.type_ "text"
            , HA.value author.name
            , Html.Events.onInput (AuthorNameChange n)
            ]
            []
        , case author.signature of
            Nothing ->
                Html.span []
                    [ Html.text " signature: "
                    , button [ onClick <| LoadJsonSignatureButtonClicked n author.name ] [ text "Load signed JSON file" ]
                    ]

            Just sig ->
                Html.span []
                    [ Html.text " witness algorithm: "
                    , Html.text author.witnessAlgorithm
                    , Html.text ", public key: "
                    , Html.text author.publicKey
                    , Html.text ", signature: "
                    , text <| sig ++ " "
                    , button [ onClick <| LoadJsonSignatureButtonClicked n author.name ] [ text "Change signature" ]
                    ]
        ]


viewSigner : AuthorWitness -> Html Msg
viewSigner { name, witnessAlgorithm, publicKey, signature } =
    Html.li [] <|
        (text <| "Name: " ++ name)
            :: (case signature of
                    Nothing ->
                        [ text ", no signature provided" ]

                    Just sig ->
                        [ text <| " , witness algorithm: " ++ witnessAlgorithm
                        , text <| " , public key: " ++ publicKey
                        , text <| " , signature: " ++ sig
                        ]
               )



--
-- Storage Step
--


viewPermanentStorageStep : ViewContext msg -> Step RationaleSignatureForm {} RationaleSignature -> Step StorageForm {} Storage -> Html msg
viewPermanentStorageStep ctx rationaleSigStep step =
    case ( rationaleSigStep, step ) of
        ( Done _ _, Preparing form ) ->
            Html.map ctx.wrapMsg <|
                div []
                    [ Html.h3 [] [ text "Permanent Storage" ]
                    , Html.p []
                        [ text "Only the hash of your rationale is stored on Cardano,"
                        , text " so it’s recommended to also store the actual JSON file containing the rationale in a permanent storage solution."
                        , text " Here we provide an easy way to store it on IPFS."
                        , text " You can specify your own IPFS RPC server, or use one of an API provider, such as Blockfrost for example."
                        , text " More info on "
                        , Html.a [ HA.href "https://blockfrost.dev/start-building/ipfs/", HA.target "_blank", HA.rel "noopener noreferrer" ]
                            [ text "Blockfrost docs." ]
                        ]
                    , Html.p []
                        [ Html.text "IPFS RPC server: "
                        , Html.input
                            [ HA.type_ "text"
                            , HA.placeholder "e.g. https://ipfs.blockfrost.io/api/v0/ipfs"
                            , HA.value form.ipfsServer
                            , Html.Events.onInput IpfsServerChange
                            ]
                            []
                        ]
                    , Html.p []
                        [ text "Request headers: "
                        , button [ onClick AddHeaderButtonClicked ] [ text "add" ]
                        ]
                    , Html.ul [] (List.indexedMap viewHeader form.headers)
                    , Html.p [] [ button [ onClick PinJsonIpfsButtonClicked ] [ text "Pin JSON rationale to IPFS" ] ]
                    , viewError form.error
                    ]

        ( Done _ _, Validating _ _ ) ->
            div []
                [ Html.h3 [] [ text "Permanent Storage" ]
                , Html.p [] [ text "Uploading rationale to IPFS server ..." ]
                ]

        ( Done _ r, Done _ storage ) ->
            let
                link =
                    "https://ipfs.io/ipfs/" ++ storage.jsonFile.cid

                dataHash =
                    Bytes.fromText r.signedJson
                        |> Bytes.toU8
                        |> blake2b256 Nothing
                        |> Bytes.fromU8
            in
            Html.map ctx.wrapMsg <|
                div []
                    [ Html.h3 [] [ text "Permanent Storage" ]
                    , Html.p []
                        [ text "File uploaded: "
                        , Html.a [ HA.href link, HA.download storage.jsonFile.name, HA.target "_blank" ] [ text link ]
                        ]
                    , Html.ul []
                        [ Html.li [] [ text <| "name: " ++ storage.jsonFile.name ]
                        , Html.li [] [ text <| "cid: " ++ storage.jsonFile.cid ]
                        , Html.li [] [ text <| "size: " ++ storage.jsonFile.size ++ " Bytes" ]
                        , Html.li [] [ text <| "file hash: " ++ Bytes.toHex dataHash ]
                        ]
                    , Html.p [] [ button [ onClick AddOtherStorageButtonCLicked ] [ text "Add another storage" ] ]
                    ]

        _ ->
            div []
                [ Html.h3 [] [ text "Permanent Storage" ]
                , Html.p [] [ text "Please complete the rationale signature step first." ]
                ]


viewHeader : Int -> ( String, String ) -> Html Msg
viewHeader n ( field, value ) =
    Html.li []
        [ button [ onClick (DeleteHeaderButtonClicked n) ] [ text "Delete" ]
        , text " "
        , Html.input
            [ HA.type_ "text"
            , HA.placeholder "e.g. project_id"
            , HA.value field
            , Html.Events.onInput (StorageHeaderFieldChange n)
            ]
            []
        , Html.text " : "
        , Html.input
            [ HA.type_ "text"
            , HA.placeholder "e.g. ipfsEnrkKWDwlA9hV4IajI4ILrFdsHJpIqNC"
            , HA.value value
            , Html.Events.onInput (StorageHeaderValueChange n)
            ]
            []
        ]



--
-- Fee Provider Step
--


viewFeeProviderStep : ViewContext msg -> Step FeeProviderForm FeeProviderTemp FeeProvider -> Html msg
viewFeeProviderStep ctx step =
    case step of
        Preparing form ->
            div []
                [ Html.h3 [] [ text "Fee Provider" ]
                , Html.map ctx.wrapMsg <| viewFeeProviderForm form
                , Html.p [] [ button [ onClick <| ctx.wrapMsg ValidateFeeProviderFormButtonClicked ] [ text "Confirm Fee Provider" ] ]
                , let
                    maybeError =
                        case form of
                            ConnectedWalletFeeProvider { error } ->
                                error

                            ExternalFeeProvider { error } ->
                                error
                  in
                  case maybeError of
                    Just error ->
                        Html.p [] [ Html.pre [] [ text error ] ]

                    Nothing ->
                        text ""
                ]

        Validating _ _ ->
            div []
                [ Html.h3 [] [ text "Fee Provider" ]
                , Html.p [] [ text "validating fee provider information ..." ]
                ]

        Done _ { address, utxos } ->
            div []
                [ Html.h3 [] [ text "Fee Provider" ]
                , Html.p [] [ text <| "Address: " ++ prettyAddr address ]
                , Html.p [] [ text <| "Available UTxO count: " ++ String.fromInt (Dict.Any.size utxos) ]
                , Html.p [] [ button [ onClick <| ctx.wrapMsg ChangeFeeProviderButtonClicked ] [ text "Change fee provider" ] ]
                ]


viewFeeProviderForm : FeeProviderForm -> Html Msg
viewFeeProviderForm feeProviderForm =
    let
        isUsingWalletForFees =
            case feeProviderForm of
                ConnectedWalletFeeProvider _ ->
                    True

                _ ->
                    False
    in
    div []
        [ viewFeeProviderOption
            (ConnectedWalletFeeProvider { error = Nothing })
            "Use connected wallet"
            isUsingWalletForFees
        , viewFeeProviderOption
            (ExternalFeeProvider { endpoint = "", error = Nothing })
            "(WIP) Use external fee provider"
            (not isUsingWalletForFees)
        , case feeProviderForm of
            ExternalFeeProvider { endpoint, error } ->
                div []
                    [ Html.label [] [ text "External Provider Endpoint" ]
                    , Html.input
                        [ HA.type_ "text"
                        , HA.value endpoint
                        , Html.Events.onInput
                            (\s -> FeeProviderUpdated (ExternalFeeProvider { endpoint = s, error = error }))
                        ]
                        []
                    ]

            _ ->
                text ""
        ]


viewFeeProviderOption : FeeProviderForm -> String -> Bool -> Html Msg
viewFeeProviderOption feeProviderForm label isSelected =
    div []
        [ Html.input
            [ HA.type_ "radio"
            , HA.name "fee-provider"
            , HA.checked isSelected
            , onClick (FeeProviderUpdated feeProviderForm)
            ]
            []
        , Html.label [] [ text label ]
        ]



--
-- Tx Building Step
--


viewBuildTxStep : ViewContext msg -> Model -> Html msg
viewBuildTxStep ctx model =
    case ( allPrepSteps ctx.costModels model, model.buildTxStep ) of
        ( Err _, _ ) ->
            div []
                [ Html.h3 [] [ text "Tx Building" ]
                , Html.p [] [ text "Complete all previous steps first." ]
                ]

        ( Ok _, Preparing { error } ) ->
            div []
                [ Html.h3 [] [ text "Tx Building" ]
                , Html.p []
                    [ button [ onClick <| ctx.wrapMsg <| BuildTxButtonClicked Gov.VoteYes ] [ text "Vote YES" ]
                    , text " "
                    , button [ onClick <| ctx.wrapMsg <| BuildTxButtonClicked Gov.VoteNo ] [ text "Vote NO" ]
                    , text " "
                    , button [ onClick <| ctx.wrapMsg <| BuildTxButtonClicked Gov.VoteAbstain ] [ text "Vote ABSTAIN" ]
                    ]
                , viewError error
                ]

        ( Ok _, Validating _ _ ) ->
            div []
                [ Html.h3 [] [ text "Tx Building" ]
                , Html.p [] [ text "validating information ..." ]
                ]

        ( Ok _, Done _ { tx } ) ->
            div []
                [ Html.h3 [] [ text "Tx Building" ]
                , Html.p [] [ text "The generated Tx (₳ displayed as lovelaces) :" ]
                , Html.p [] [ Html.pre [] [ text <| prettyTx tx ] ]
                , Html.p [] [ button [ onClick <| ctx.wrapMsg <| ChangeVoteButtonClicked ] [ text "Change vote" ] ]
                ]



--
-- Tx Signing Step
--


viewSignTxStep : ViewContext msg -> Step BuildTxPrep {} TxFinalized -> Html msg
viewSignTxStep ctx buildTxStep =
    case buildTxStep of
        Done _ { tx, expectedSignatures } ->
            div []
                [ Html.h3 [] [ text "Tx Signing" ]
                , Html.p [] [ text "Expecting signatures for the following public key hashes:" ]
                , Html.ul [] (List.map (\hash -> Html.li [] [ Html.pre [] [ text <| Bytes.toHex hash ] ]) expectedSignatures)
                , Html.p []
                    [ text "Finalize your voting transaction by signing and submitting it via the dedicated signing page: "
                    , ctx.signingLink tx expectedSignatures [ text "signing page" ]
                    ]
                ]

        _ ->
            div []
                [ Html.h3 [] [ text "Tx Signing" ]
                , Html.p [] [ text "Please complete the Tx building step first." ]
                ]



--
-- Helpers
--


viewError : Maybe String -> Html msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just err ->
            Html.p []
                [ text "Error:"
                , Html.pre [] [ text err ]
                ]
