module Page.Preparation exposing (AuthorWitness, BuildTxPrep, FeeProvider, FeeProviderForm, FeeProviderTemp, InternalVote, JsonLdContexts, LoadedWallet, MarkdownForm, Model, Msg, MsgToParent(..), Rationale, RationaleForm, RationaleSignatureForm, Reference, ReferenceType(..), Step, StorageForm, StorageMethod, TaskCompleted, UpdateContext, ViewContext, VoterPreparationForm, handleTaskCompleted, init, noInternalVote, pinRationaleFile, update, view)

{-| This module handles the complete vote preparation workflow, from identifying
the voter to signing the transaction, which is handled by another page.

The workflow is split into the following sequential steps:

1.  Voter identification - Who is voting (DRep/SPO/CC)
2.  Proposal selection - What proposal to vote on
3.  Rationale creation - The reasoning behind the vote
4.  Rationale signing - Optional signatures from multiple authors
5.  Permanent storage - Storing rationale on IPFS
6.  Fee handling - How transaction fees will be paid
7.  Transaction building - Creating the vote transaction
8.  Transaction signing - Redirect to the signing page

Each step follows a common pattern using the Step type:

  - Preparing: Initial form state
  - Validating: Checking inputs
  - Done: Step is complete

The steps are sequential but allow going back to modify previous steps.

-}

import Api exposing (ActiveProposal, CcInfo, DrepInfo, IpfsAnswer(..), PoolInfo)
import Blake2b exposing (blake2b256)
import Bytes as ElmBytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CredentialWitness(..), ScriptWitness(..), TxFinalized, VoterWitness(..), WitnessSource(..))
import Cardano.Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
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
import File.Download
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
import Markdown.Block
import Markdown.Parser as Md
import Markdown.Renderer as Md exposing (defaultHtmlRenderer)
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


{-| Main model containing the state for all preparation steps.
Each step uses the Step type to track its progress.
-}
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
    , visibleProposalCount : Int
    }


{-| Represents the three possible states of any workflow step:

1.  Preparing - User is filling out form data
2.  Validating - Form data is being validated/processed
3.  Done - Step is complete with validated data

This allows a consistent pattern across all the preparation steps.

-}
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
    , visibleProposalCount = 10
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
    , against : Int
    }


noInternalVote : InternalVote
noInternalVote =
    { constitutional = 0
    , unconstitutional = 0
    , abstain = 0
    , didNotVote = 0
    , against = 0
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
    , error : Maybe String
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


type StorageMethod
    = StandardIPFS
    | CustomIPFS


type alias StorageForm =
    { storageMethod : StorageMethod
    , ipfsServer : String
    , headers : List ( String, String )
    , error : Maybe String
    }


initStorageForm : StorageForm
initStorageForm =
    { storageMethod = StandardIPFS
    , ipfsServer = "https://ipfs.blockfrost.io/api/v0/ipfs"
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


{-| Messages that can be sent to the parent component to:

  - Cache loaded data for reuse
  - Execute concurrent tasks

-}
type MsgToParent
    = CacheScriptInfo ScriptInfo
    | CacheDrepInfo DrepInfo
    | CacheCcInfo CcInfo
    | CachePoolInfo PoolInfo
    | RunTask (ConcurrentTask String TaskCompleted)


{-| Results from asynchronous tasks:

  - Loading reference transaction bytes
  - Loading script info

-}
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
    | ShowMoreProposals Int
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
    | InternalAgainstVoteChange String
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
    | ConvertToPdfButtonClicked String
    | GotPdfFile (Result Http.Error ElmBytes.Bytes)
      -- Storage
    | StorageMethodSelected StorageMethod
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


{-| Configuration required by the update function.
Provides access to:

  - External data (proposals, script info, etc.)
  - Cardano network cost models (for script execution)
  - Wallet integration
  - In-browser storage connection
  - Governance metadata JSON LD format

-}
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
    , networkId : NetworkId
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
        ShowMoreProposals currentCount ->
            ( { model | visibleProposalCount = currentCount + 10 }
            , Cmd.none
            , Nothing
            )

        StorageMethodSelected method ->
            ( updateStorageForm (\form -> { form | storageMethod = method }) model
            , Cmd.none
            , Nothing
            )

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

        InternalAgainstVoteChange againstVoteStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | against = n }) againstVoteStr model
            , Cmd.none
            , Nothing
            )

        AddRefButtonClicked ->
            ( updateRationaleForm (\form -> { form | references = form.references ++ [ initRefForm ] }) model
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
                    let
                        -- Initialize with no rationale signature
                        form =
                            { authors = []
                            , rationale = newRationale
                            , error = Nothing
                            }

                        updatedModel =
                            { model
                                | rationaleCreationStep = Done prep newRationale
                                , rationaleSignatureStep =
                                    Done form (rationaleSignatureFromForm ctx.jsonLdContexts form)
                            }
                    in
                    ( updatedModel, Cmd.none, Nothing )

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

        ConvertToPdfButtonClicked rawFileContent ->
            ( model
            , Api.defaultApiProvider.convertToPdf rawFileContent GotPdfFile
                |> Cmd.map ctx.wrapMsg
            , Nothing
            )

        GotPdfFile result ->
            case model.rationaleSignatureStep of
                Done form rationaleSignature ->
                    case result of
                        Err httpError ->
                            ( { model
                                | rationaleSignatureStep = Done form { rationaleSignature | error = Just <| "An error happened when trying to convert the JSON LD to a PDF: " ++ Debug.toString httpError }
                              }
                            , Cmd.none
                            , Nothing
                            )

                        Ok elmBytes ->
                            ( model
                            , File.Download.bytes "rationale.pdf" "application/pdf" elmBytes
                            , Nothing
                            )

                _ ->
                    ( model, Cmd.none, Nothing )

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


{-| Validates if a governance ID is valid and loads associated information:

  - For scripts: loads script info and extracts expected signers
  - For DReps: loads voting power
  - For CCs: loads committee member info
  - For SPOs: loads stake pool info

Returns information needed for voting or an error message.

-}
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
                                    ( RemoteData.Loading, Api.defaultApiProvider.getCcInfo ctx.networkId cred GotCcInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | ccInfo = ccInfo, cmd = fetchCcInfo }

                Gov.DrepId ((VKeyHash keyHash) as cred) ->
                    let
                        ( drepInfo, fetchDrepInfo ) =
                            case Dict.get (Bytes.toHex keyHash) ctx.drepsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getDrepInfo ctx.networkId cred GotDrepInfo )

                                Just info ->
                                    ( RemoteData.Success info, Cmd.none )
                    in
                    Ok { defaultCheck | drepInfo = drepInfo, cmd = fetchDrepInfo }

                Gov.PoolId poolId ->
                    let
                        ( poolInfo, fetchPoolInfo ) =
                            case Dict.get (Bytes.toHex poolId) ctx.poolsInfo of
                                Nothing ->
                                    ( RemoteData.Loading, Api.defaultApiProvider.getPoolLiveStake ctx.networkId poolId GotPoolInfo )

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
                                    , Api.defaultApiProvider.getScriptInfo ctx.networkId scriptHash
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
                                    ( RemoteData.Loading, Api.defaultApiProvider.getCcInfo ctx.networkId cred GotCcInfo )

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
                                    , Api.defaultApiProvider.getScriptInfo ctx.networkId scriptHash
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
                                    ( RemoteData.Loading, Api.defaultApiProvider.getDrepInfo ctx.networkId cred GotDrepInfo )

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
                        , Api.defaultApiProvider.retrieveTx ctx.networkId outputRef.transactionId
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


validateMarkdownHeadings : String -> Result String ()
validateMarkdownHeadings markdown =
    case Md.parse markdown of
        Err _ ->
            Err "Invalid markdown syntax. Please check your formatting."

        Ok blocks ->
            if hasH1Heading blocks then
                Err "Please use heading level 2 (##) or higher. Level 1 headings (#) are reserved for the page title."

            else
                Ok ()


hasH1Heading : List Markdown.Block.Block -> Bool
hasH1Heading blocks =
    List.any isH1Block blocks


isH1Block : Markdown.Block.Block -> Bool
isH1Block block =
    case block of
        Markdown.Block.Heading level _ ->
            level == Markdown.Block.H1

        _ ->
            False


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
                |> Result.andThen (\_ -> validateMarkdownHeadings str)


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
    let
        trimmed =
            String.trim discussion
    in
    checkValidMarkdown trimmed
        |> Result.andThen (\_ -> validateMarkdownHeadings trimmed)


validateRationaleCounterArg : MarkdownForm -> Result String ()
validateRationaleCounterArg counterArg =
    let
        trimmed =
            String.trim counterArg
    in
    checkValidMarkdown trimmed
        |> Result.andThen (\_ -> validateMarkdownHeadings trimmed)


validateRationaleConclusion : MarkdownForm -> Result String ()
validateRationaleConclusion _ =
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

    else if internVote.against < 0 then
        Err "AgainstVote internal vote must be >= 0"

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


{-| Converts a RationaleForm into a final Rationale by:

  - Trimming whitespace
  - Converting empty strings to Nothing for optional fields
  - Preserving internal vote counts
  - Keeping all references

-}
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
encodeInternalVote { constitutional, unconstitutional, abstain, didNotVote, against } =
    JE.object
        [ ( "constitutional", JE.int constitutional )
        , ( "unconstitutional", JE.int unconstitutional )
        , ( "abstain", JE.int abstain )
        , ( "didNotVote", JE.int didNotVote )
        , ( "againstVote", JE.int against )
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
    , error = Nothing
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
    case form.storageMethod of
        StandardIPFS ->
            -- For standard IPFS, no validation needed
            Ok ()

        CustomIPFS ->
            let
                -- Check that the IPFS server url looks legit
                ipfsServerUrlSeemsLegit =
                    case Url.fromString form.ipfsServer of
                        Just _ ->
                            Ok ()

                        Nothing ->
                            Err ("This url seems incorrect, it must look like this: https://subdomain.domain.org, instead I got this: " ++ form.ipfsServer)

                -- Check that headers look valid
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
            , case storageForm.storageMethod of
                StandardIPFS ->
                    Api.defaultApiProvider.ipfsCfAdd
                        { file = file }
                        GotIpfsAnswer

                CustomIPFS ->
                    Api.defaultApiProvider.ipfsAdd
                        { rpc = storageForm.ipfsServer
                        , headers = storageForm.headers
                        , file = file
                        }
                        GotIpfsAnswer
            )

        -- Ignore if we aren't validating the permanent storage step
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


{-| Requirements needed to build a valid voting transaction:

  - Voter witness (key or script)
  - Proposal being voted on
  - Rationale anchor (metadata hash)
  - UTXOs for fee payment and scripts
  - Protocol parameters for cost calculation

-}
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


{-| Configuration needed by the view:

  - Message wrapper for parent component
  - Wallet connectivity status
  - Available proposals to vote on
  - JSON-LD metadata context for rationale
  - Protocol parameters
  - Link to transaction signing page

-}
type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , walletChangeAddress : Maybe Address
    , proposals : WebData (Dict String ActiveProposal)
    , jsonLdContexts : JsonLdContexts
    , costModels : Maybe CostModels
    , networkId : NetworkId
    , signingLink : Transaction -> List (Bytes CredentialHash) -> List (Html msg) -> Html msg
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div [ HA.style "max-width" "1440px", HA.style "margin" "0 auto" ]
        [ div
            [ HA.style "position" "relative"
            , HA.style "overflow" "hidden"
            , HA.style "padding-top" "6rem"
            , HA.style "padding-bottom" "6rem"
            , HA.style "margin-bottom" "2rem"
            ]
            [ div
                [ HA.style "position" "relative"
                , HA.style "z-index" "10"
                , HA.style "max-width" "840px"
                , HA.style "margin" "0 auto"
                , HA.style "padding" "0 1.5rem"
                ]
                [ Html.h1
                    [ HA.style "font-size" "3.5rem"
                    , HA.style "font-weight" "600"
                    , HA.style "line-height" "1.1"
                    , HA.style "margin-bottom" "1.5rem"
                    ]
                    [ text "Vote Preparation" ]
                , Html.p
                    [ HA.style "font-size" "1.25rem"
                    , HA.style "line-height" "1.6"
                    , HA.style "max-width" "640px"
                    , HA.style "margin-bottom" "2rem"
                    ]
                    [ text "This page helps you prepare and submit votes for governance proposals. You can identify yourself as a voter, select a proposal, create a rationale for your vote, and build the transaction."
                    ]
                ]
            , div
                [ HA.style "position" "absolute"
                , HA.style "z-index" "1"
                , HA.style "top" "-13rem"
                , HA.style "right" "0"
                , HA.style "left" "0"
                , HA.style "overflow" "hidden"
                , HA.style "transform" "translateZ(0)"
                , HA.style "filter" "blur(64px)"
                ]
                [ div
                    [ HA.style "position" "relative"
                    , HA.style "width" "100%"
                    , HA.style "padding-bottom" "58.7%"
                    , HA.style "background" "linear-gradient(90deg, #00E0FF, #0084FF)"
                    , HA.style "opacity" "0.8"
                    , HA.style "clip-path" "polygon(19% 5%, 36% 8%, 55% 15%, 76% 5%, 100% 16%, 100% 100%, 0 100%, 0 14%)"
                    ]
                    []
                ]
            ]
        , div
            [ HA.style "max-width" "840px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1.5rem"
            ]
            [ viewVoterIdentificationStep ctx model.voterStep
            , Html.hr [ HA.style "margin-top" "3rem", HA.style "border-color" "#C7C7C7" ] []
            , viewProposalSelectionStep ctx model
            , Html.hr [ HA.style "margin-top" "3rem", HA.style "border-color" "#C7C7C7" ] []
            , viewRationaleStep ctx model.rationaleCreationStep
            , Html.hr [ HA.style "margin-top" "1rem", HA.style "border-color" "#C7C7C7" ] []
            , viewRationaleSignatureStep ctx model.rationaleCreationStep model.rationaleSignatureStep
            , Html.hr [ HA.style "margin-top" "2rem", HA.style "border-color" "#C7C7C7" ] []
            , viewPermanentStorageStep ctx model.rationaleSignatureStep model.permanentStorageStep
            , Html.hr [ HA.style "margin-top" "2rem", HA.style "border-color" "#C7C7C7" ] []
            , viewFeeProviderStep ctx model.feeProviderStep
            , Html.hr [ HA.style "margin-top" "2rem", HA.style "border-color" "#C7C7C7" ] []
            , viewBuildTxStep ctx model
            , Html.hr [ HA.style "margin-top" "1rem", HA.style "border-color" "#C7C7C7" ] []
            , viewSignTxStep ctx model.buildTxStep
            ]
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
                    [ Html.h2 [ HA.class "text-3xl font-medium  mb-4" ] [ text "Vote Preparation" ]
                    , Html.p [] [ Helper.firstTextField "Voter governance ID (drep/pool/cc_hot)" (Maybe.withDefault "" <| Maybe.map Gov.idToBech32 form.govId) VoterGovIdChange ]
                    , Html.Lazy.lazy viewValidGovIdForm form
                    , Html.p [ HA.class "my-4" ] [ Helper.viewButton "Confirm Voter" ValidateVoterFormButtonClicked ]
                    , viewError form.error
                    ]

        Validating _ _ ->
            div [ HA.style "background-color" "#ffffff", HA.style "border-radius" "0.5rem", HA.style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)", HA.style "padding" "1.5rem" ]
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
                    Html.p [] [ Helper.textField "Reference UTxO" utxoRef UtxoRefChange ]

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
        [ Html.h2 [ HA.class "text-3xl font-medium  mb-4" ] [ text "Voter Information" ]
        , div [ HA.class " p-4 rounded-md border mb-4", HA.style "border-color" "#C6C6C6" ]
            [ Html.div [ HA.class "mb-2" ]
                [ Html.span [ HA.class "font-medium" ] [ text voterTypeText ]
                ]
            , Html.div []
                [ case voterCred of
                    WithKey cred ->
                        Html.div [ HA.class "flex flex-col space-y-1" ]
                            [ Html.div []
                                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Using key with hash:" ]
                                , Html.span [ HA.class "font-mono" ] [ text (Bytes.toHex cred) ]
                                ]
                            ]

                    WithScript hash (NativeWitness { expectedSigners }) ->
                        Html.div [ HA.class "flex flex-col space-y-2" ]
                            [ Html.div []
                                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Using native script with hash:" ]
                                , Html.span [ HA.class "font-mono" ] [ text (Bytes.toHex hash) ]
                                ]
                            , if List.isEmpty expectedSigners then
                                Html.div [ HA.class "mt-1" ] [ text "No expected signers." ]

                              else
                                Html.div []
                                    [ Html.p [ HA.class "font-medium mt-1 mb-1" ] [ text "Expected signers:" ]
                                    , Html.ul [ HA.class "list-disc pl-5 space-y-1" ]
                                        (List.map
                                            (\s ->
                                                Html.li [ HA.class "text-sm" ]
                                                    [ Html.span [ HA.class "font-mono" ] [ text (Bytes.toHex s) ] ]
                                            )
                                            expectedSigners
                                        )
                                    ]
                            ]

                    WithScript _ (PlutusWitness _) ->
                        Html.div []
                            [ Html.span [ HA.class "" ] [ text "Using Plutus script (details not available)" ] ]
                ]
            ]
        , Html.p [] [ Helper.viewButton "Change Voter" ChangeVoterButtonClicked ]
        ]



--
-- Proposal Selection Step
--


viewProposalSelectionStep : ViewContext msg -> Model -> Html msg
viewProposalSelectionStep ctx model =
    case model.pickProposalStep of
        Preparing _ ->
            let
                visibleCount =
                    model.visibleProposalCount
            in
            div [ HA.style "padding-top" "50px", HA.style "padding-bottom" "8px" ]
                [ Html.h2 [ HA.class "text-3xl font-medium mb-4" ] [ text "Pick a Proposal" ]
                , case ctx.proposals of
                    RemoteData.NotAsked ->
                        text "Proposals are not loading, please report this error."

                    RemoteData.Loading ->
                        div [ HA.style "display" "flex", HA.style "justify-content" "center", HA.style "padding" "2rem" ]
                            [ text "Loading proposals..." ]

                    RemoteData.Failure httpError ->
                        Html.pre []
                            [ text "Something went wrong while loading proposals."
                            , text <| Debug.toString httpError
                            ]

                    RemoteData.Success proposalsDict ->
                        if Dict.isEmpty proposalsDict then
                            div [ HA.style "text-align" "center", HA.style "padding" "2rem", HA.style "color" "#666" ]
                                [ text "No active proposals found." ]

                        else
                            let
                                allProposals =
                                    Dict.values proposalsDict

                                totalCount =
                                    Dict.size proposalsDict

                                visibleProposals =
                                    List.take visibleCount allProposals

                                hasMore =
                                    totalCount > visibleCount
                            in
                            div []
                                [ Html.p [ HA.style "margin-bottom" "1rem" ]
                                    [ text <| "Select a proposal to vote on (" ++ String.fromInt totalCount ++ " available):" ]
                                , div [ HA.style "display" "grid", HA.style "grid-template-columns" "repeat(auto-fill, minmax(250px, 1fr))", HA.style "gap" "1.5rem" ]
                                    (List.map (viewProposalCard ctx.wrapMsg ctx.networkId) visibleProposals)
                                , if hasMore then
                                    div
                                        [ HA.style "text-align" "center"
                                        , HA.style "margin-top" "2rem"
                                        ]
                                        [ button
                                            [ HA.style "background-color" "#f9fafb"
                                            , HA.style "color" "#272727"
                                            , HA.style "font-weight" "500"
                                            , HA.style "border" "1px solid #e2e8f0"
                                            , HA.style "border-radius" "0.5rem"
                                            , HA.style "padding" "0.75rem 1.5rem"
                                            , HA.style "cursor" "pointer"
                                            , HA.style "transition" "all 0.2s ease"
                                            , onClick (ctx.wrapMsg (ShowMoreProposals visibleCount))
                                            ]
                                            [ text <| "Show More (" ++ String.fromInt (min 10 (totalCount - visibleCount)) ++ " of " ++ String.fromInt (totalCount - visibleCount) ++ " remaining)" ]
                                        ]

                                  else
                                    text ""
                                ]
                ]

        Validating _ _ ->
            div []
                [ Html.h2 [ HA.class "text-3xl font-medium mb-4" ] [ text "Pick a Proposal" ]
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
                                    [ Html.strong [] [ text "Abstract: " ]
                                    , text <| Maybe.withDefault "Unknown abstract (unexpected metadata format)" meta.body.abstract
                                    ]
                            )
            in
            div [ HA.style "padding-top" "50px", HA.style "padding-bottom" "8px" ]
                [ Html.h2 [ HA.class "text-3xl font-medium mb-4" ] [ text "Pick a Proposal" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "mb-4" ]
                        [ Html.strong [ HA.class "font-medium" ] [ text "Selected proposal:" ]
                        ]
                    , div [ HA.class " p-4 rounded-md border mb-4", HA.style "border-color" "#C6C6C6" ]
                        [ div [ HA.class "mb-2" ]
                            [ Html.span [ HA.class "font-bold mr-2" ] [ text "Proposal ID:" ]
                            , cardanoScanActionLink ctx.networkId id
                            ]
                        , div [ HA.class "mb-2" ]
                            [ Html.span [ HA.class "font-bold mr-2" ] [ text "Type:" ]
                            , Html.span [] [ text actionType ]
                            ]
                        , div [ HA.class "mb-2" ]
                            [ Html.span [ HA.class "font-bold mr-2" ] [ text "Title:" ]
                            , Html.span [] [ text title ]
                            ]
                        , content
                            |> Maybe.withDefault (text "")
                        ]
                    ]
                , Html.p [ HA.style "margin-top" "4px" ] [ Helper.viewButton "Change Proposal" (ctx.wrapMsg ChangeProposalButtonClicked) ]
                ]


viewProposalCard : (Msg -> msg) -> NetworkId -> ActiveProposal -> Html msg
viewProposalCard wrapMsg networkId proposal =
    let
        idString =
            Gov.actionIdToString proposal.id

        title =
            case proposal.metadata of
                RemoteData.Success meta ->
                    meta.body.title |> Maybe.withDefault "Untitled Proposal"

                _ ->
                    "Loading..."

        abstract =
            case proposal.metadata of
                RemoteData.Success meta ->
                    meta.body.abstract |> Maybe.withDefault "No abstract available"

                RemoteData.Loading ->
                    "Loading proposal details..."

                RemoteData.Failure _ ->
                    "Error loading proposal details"

                RemoteData.NotAsked ->
                    "Proposal details not available"
    in
    div
        [ HA.style "border" "1px solid #E2E8F0"
        , HA.style "border-radius" "0.75rem"
        , HA.style "box-shadow" "0 2px 4px rgba(0,0,0,0.06)"
        , HA.style "background-color" "#FFFFFF"
        , HA.style "display" "flex"
        , HA.style "flex-direction" "column"
        , HA.style "height" "100%"
        , HA.style "transition" "all 0.3s ease"
        , HA.style "transform-origin" "center"
        , HA.style "position" "relative"
        , HA.style "overflow" "hidden"
        ]
        [ div
            [ HA.style "background-color" "#F7FAFC"
            , HA.style "padding" "1rem 1.25rem"
            , HA.style "border-bottom" "1px solid #EDF2F7"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1.125rem"
                , HA.style "white-space" "nowrap"
                , HA.style "overflow" "hidden"
                , HA.style "text-overflow" "ellipsis"
                , HA.style "color" "#1A202C"
                ]
                [ text title ]
            ]
        , div
            [ HA.style "padding" "1.25rem"
            , HA.style "flex-grow" "1"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            ]
            [ Html.p
                [ HA.style "font-size" "0.875rem"
                , HA.style "color" "#4A5568"
                , HA.style "line-height" "1.6"
                , HA.style "display" "-webkit-box"
                , HA.style "-webkit-line-clamp" "4"
                , HA.style "-webkit-box-orient" "vertical"
                , HA.style "overflow" "hidden"
                , HA.style "margin-bottom" "1.5rem"
                ]
                [ text abstract ]
            , div
                [ HA.style "font-size" "0.75rem"
                , HA.style "color" "#718096"
                , HA.style "margin-top" "auto"
                , HA.style "padding-top" "1rem"
                , HA.style "border-top" "1px solid #EDF2F7"
                ]
                [ Html.div
                    [ HA.style "display" "flex"
                    , HA.style "flex-wrap" "wrap"
                    , HA.style "justify-content" "space-between"
                    , HA.style "align-items" "center"
                    , HA.style "margin-bottom" "0.75rem"
                    ]
                    [ cardanoScanActionLink networkId proposal.id
                    , div
                        [ HA.style "font-size" "0.75rem"
                        , HA.style "font-weight" "500"
                        , HA.style "color" "#4A5568"
                        , HA.style "background-color" "#EDF2F7"
                        , HA.style "padding" "0.25rem 0.5rem"
                        , HA.style "border-radius" "9999px"
                        , HA.style "margin-left" "0.5rem"
                        ]
                        [ text proposal.actionType ]
                    ]
                ]
            , Html.button
                [ HA.style "width" "100%"
                , HA.style "background-color" "#272727"
                , HA.style "color" "white"
                , HA.style "font-weight" "500"
                , HA.style "font-size" "0.875rem"
                , HA.style "padding" "0.75rem 0"
                , HA.style "border" "none"
                , HA.style "border-radius" "0.5rem"
                , HA.style "cursor" "pointer"
                , HA.style "transition" "background-color 0.2s"
                , HA.style "margin-top" "1rem"
                , onClick (wrapMsg (PickProposalButtonClicked idString))
                ]
                [ text "Select Proposal" ]
            ]
        ]


cardanoScanActionLink : NetworkId -> ActionId -> Html msg
cardanoScanActionLink networkId id =
    let
        baseUrl =
            case networkId of
                Mainnet ->
                    "https://cardanoscan.io/govAction/"

                Testnet ->
                    "https://preview.cardanoscan.io/govAction/"
    in
    Html.a
        [ HA.href <|
            baseUrl
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
                div [ HA.style "padding-top" "50px", HA.style "padding-bottom" "8px" ]
                    [ Html.h2 [ HA.class "text-3xl font-medium my-4" ] [ text "Vote Rationale" ]
                    , Helper.formContainer [ viewSummaryForm form.summary ]
                    , Helper.formContainer [ viewStatementForm form.rationaleStatement ]
                    , Helper.formContainer [ viewPrecedentDiscussionForm form.precedentDiscussion ]
                    , Helper.formContainer [ viewCounterArgumentForm form.counterArgumentDiscussion ]
                    , Helper.formContainer [ viewConclusionForm form.conclusion ]
                    , Helper.formContainer [ viewInternalVoteForm form.internalVote ]
                    , viewReferencesForm form.references
                    , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Confirm rationale" ValidateRationaleButtonClicked ]
                    , viewError form.error
                    ]

            Validating _ _ ->
                div []
                    [ Html.h2 [ HA.class "text-3xl font-medium my-4" ] [ text "Vote Rationale" ]
                    , Helper.formContainer
                        [ Html.p [ HA.class "text-gray-600" ] [ text "Validating rationale data..." ]
                        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Edit rationale" EditRationaleButtonClicked ]
                        ]
                    ]

            Done _ rationale ->
                div []
                    [ Html.h2 [ HA.class "text-3xl font-medium my-4" ] [ text "Vote Rationale" ]
                    , div [ HA.class "space-y-6" ]
                        [ Helper.formContainer
                            [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                [ viewSummary rationale.summary ]
                            ]
                        , Helper.formContainer
                            [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                [ viewStatementMd rationale.rationaleStatement ]
                            ]
                        , if rationale.precedentDiscussion /= Nothing then
                            Helper.formContainer
                                [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                    [ viewPrecedentDiscussionMd rationale.precedentDiscussion ]
                                ]

                          else
                            text ""
                        , if rationale.counterArgumentDiscussion /= Nothing then
                            Helper.formContainer
                                [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                    [ viewCounterArgumentMd rationale.counterArgumentDiscussion ]
                                ]

                          else
                            text ""
                        , if rationale.conclusion /= Nothing then
                            Helper.formContainer
                                [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                    [ viewConclusion rationale.conclusion ]
                                ]

                          else
                            text ""
                        , if rationale.internalVote /= noInternalVote then
                            Helper.formContainer
                                [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                    [ viewInternalVote rationale.internalVote ]
                                ]

                          else
                            text ""
                        , if not (List.isEmpty rationale.references) then
                            Helper.formContainer
                                [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                                    [ viewReferences rationale.references ]
                                ]

                          else
                            text ""
                        ]
                    , Html.p [ HA.class "mt-6" ] [ Helper.viewButton "Edit rationale" EditRationaleButtonClicked ]
                    ]


viewSummaryForm : MarkdownForm -> Html Msg
viewSummaryForm form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Summary" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Compulsory" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Clearly state your stance, summarize your rationale with your main argument." ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Limited to 300 characters, does NOT support markdown." ]
            ]
        , Helper.viewTextarea form RationaleSummaryChange
        ]


viewStatementForm : MarkdownForm -> Html Msg
viewStatementForm form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Rationale Statement" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Compulsory" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Fully describe your rationale, with your arguments in full details." ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit. Use markdown with heading level 2 (##) or higher." ]
            ]
        , Helper.viewTextarea form RationaleStatementChange
        ]


viewPrecedentDiscussionForm : MarkdownForm -> Html Msg
viewPrecedentDiscussionForm form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Precedent Discussion" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Optional" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Discuss what you feel is relevant precedent." ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit and markdown is supported (preview below)." ]
            ]
        , Helper.viewTextarea form PrecedentDiscussionChange
        ]


viewCounterArgumentForm : MarkdownForm -> Html Msg
viewCounterArgumentForm form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Counter Argument Discussion" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Optional" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Discuss significant counter arguments to your position." ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit and markdown is supported (preview below)." ]
            ]
        , Helper.viewTextarea form CounterArgumentChange
        ]


viewConclusionForm : MarkdownForm -> Html Msg
viewConclusionForm form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Conclusion" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Optional" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit, does NOT support markdown." ]
            ]
        , Helper.viewTextarea form ConclusionChange
        ]


viewInternalVoteForm : InternalVote -> Html Msg
viewInternalVoteForm { constitutional, unconstitutional, abstain, didNotVote, against } =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Internal Vote" ]
        , Html.p [ HA.class "text-sm text-gray-600 mt-2 mb-4" ]
            [ text "If you vote as a group, you can report the group internal votes." ]
        , div [ HA.class "flex flex-wrap gap-4" ]
            [ div [ HA.class "mr-4" ] [ Helper.viewNumberInput "Constitutional" constitutional InternalConstitutionalVoteChange ]
            , div [ HA.class "mr-4" ] [ Helper.viewNumberInput "Unconstitutional" unconstitutional InternalUnconstitutionalVoteChange ]
            , div [ HA.class "mr-4" ] [ Helper.viewNumberInput "Abstain" abstain InternalAbstainVoteChange ]
            , div [ HA.class "mr-4" ] [ Helper.viewNumberInput "Did not vote" didNotVote InternalDidNotVoteChange ]
            , div [ HA.class "mr-4" ] [ Helper.viewNumberInput "Against voting" against InternalAgainstVoteChange ]
            ]
        ]


viewReferencesForm : List Reference -> Html Msg
viewReferencesForm references =
    div [ HA.style "margin-top" "50px" ]
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "References" ]
        , div [] (List.indexedMap viewOneRefForm references)
        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Add a reference" AddRefButtonClicked ]
        ]


viewOneRefForm : Int -> Reference -> Html Msg
viewOneRefForm n reference =
    Helper.formContainer
        [ div [ HA.class "flex flex-wrap gap-4" ]
            [ div [ HA.class "" ]
                [ Helper.labeledField "Type"
                    (Helper.viewSelect
                        [ HA.value (refTypeToString reference.type_)
                        , Html.Events.onInput (ReferenceTypeChange n)
                        , HA.class "w-full"
                        ]
                        (List.map viewRefOption allRefTypes)
                    )
                ]
            , div [ HA.class "" ]
                [ Helper.labeledField "Label"
                    (Helper.textFieldInline reference.label (ReferenceLabelChange n))
                ]
            , div [ HA.class "" ]
                [ Helper.labeledField "URI"
                    (Helper.textFieldInline reference.uri (ReferenceUriChange n))
                ]
            , div [ HA.class "" ]
                [ Helper.viewButton "Delete" (DeleteRefButtonClicked n) ]
            ]
        ]


viewRefOption : ReferenceType -> Html Msg
viewRefOption refType =
    Html.option
        [ HA.value <| refTypeToString refType ]
        [ text <| refTypeToString refType ]


viewSummary : String -> Html msg
viewSummary summary =
    div []
        [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Summary" ]
        , Html.p [ HA.class "text-gray-800" ] [ text summary ]
        ]


viewStatementMd : String -> Html msg
viewStatementMd statement =
    div []
        [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Rationale Statement" ]
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
            case Md.render markdownRenderer blocks of
                Err errors ->
                    Html.p []
                        [ Html.pre [] [ text "Unexpected error while rendering markdown:" ]
                        , Html.pre [] [ text errors ]
                        ]

                Ok rendered ->
                    Html.div
                        [ HA.class "markdown-content" ]
                        rendered


markdownRenderer : Md.Renderer (Html msg)
markdownRenderer =
    { defaultHtmlRenderer | heading = customHeadingRenderer }


customHeadingRenderer : { level : Markdown.Block.HeadingLevel, rawText : String, children : List (Html msg) } -> Html msg
customHeadingRenderer { level, children } =
    case level of
        Markdown.Block.H1 ->
            Html.h1
                [ HA.style "font-size" "2rem"
                , HA.style "font-weight" "700"
                , HA.style "margin-top" "1.5rem"
                , HA.style "margin-bottom" "1rem"
                ]
                children

        Markdown.Block.H2 ->
            Html.h2
                [ HA.style "font-size" "1.5rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "1.5rem"
                , HA.style "margin-bottom" "1rem"
                ]
                children

        Markdown.Block.H3 ->
            Html.h3
                [ HA.style "font-size" "1.25rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "1rem"
                , HA.style "margin-bottom" "0.75rem"
                ]
                children

        Markdown.Block.H4 ->
            Html.h4
                [ HA.style "font-size" "1.125rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "1rem"
                , HA.style "margin-bottom" "0.75rem"
                ]
                children

        Markdown.Block.H5 ->
            Html.h5
                [ HA.style "font-size" "1rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "0.75rem"
                , HA.style "margin-bottom" "0.5rem"
                ]
                children

        Markdown.Block.H6 ->
            Html.h6
                [ HA.style "font-size" "0.875rem"
                , HA.style "font-weight" "600"
                , HA.style "margin-top" "0.75rem"
                , HA.style "margin-bottom" "0.5rem"
                ]
                children


viewPrecedentDiscussionMd : Maybe String -> Html msg
viewPrecedentDiscussionMd maybeDiscussion =
    case maybeDiscussion of
        Nothing ->
            text ""

        Just discussion ->
            div []
                [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Precedent Discussion" ]
                , viewMd discussion
                ]


viewCounterArgumentMd : Maybe String -> Html msg
viewCounterArgumentMd maybeArgument =
    case maybeArgument of
        Nothing ->
            text ""

        Just argument ->
            div []
                [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Counter Argument" ]
                , viewMd argument
                ]


viewConclusion : Maybe String -> Html msg
viewConclusion maybeConclusion =
    case maybeConclusion of
        Nothing ->
            text ""

        Just conclusion ->
            div []
                [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Conclusion" ]
                , Html.p [ HA.class "text-gray-800 whitespace-pre-wrap" ] [ text conclusion ]
                ]


viewInternalVote : InternalVote -> Html msg
viewInternalVote ({ constitutional, unconstitutional, abstain, didNotVote, against } as internalVote) =
    if internalVote == noInternalVote then
        text ""

    else
        div []
            [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Internal Vote" ]
            , Html.ul [ HA.class "space-y-1" ]
                [ Html.li [] [ Html.strong [ HA.class "font-medium" ] [ text "Constitutional: " ], text (String.fromInt constitutional) ]
                , Html.li [] [ Html.strong [ HA.class "font-medium" ] [ text "Unconstitutional: " ], text (String.fromInt unconstitutional) ]
                , Html.li [] [ Html.strong [ HA.class "font-medium" ] [ text "Abstain: " ], text (String.fromInt abstain) ]
                , Html.li [] [ Html.strong [ HA.class "font-medium" ] [ text "Did not vote: " ], text (String.fromInt didNotVote) ]
                , Html.li [] [ Html.strong [ HA.class "font-medium" ] [ text "Against voting: " ], text (String.fromInt against) ]
                ]
            ]


viewReferences : List Reference -> Html msg
viewReferences references =
    if List.isEmpty references then
        text ""

    else
        div []
            [ Html.h4 [ HA.class "text-xl font-medium mb-2" ] [ text "References" ]
            , Html.ul [ HA.class "space-y-2" ] (List.map viewRef references)
            ]


viewRef : Reference -> Html msg
viewRef ref =
    Html.li [ HA.class "py-1" ]
        [ Html.div [ HA.class "flex flex-col md:flex-row" ]
            [ Html.span [ HA.class "mr-4" ] [ Html.strong [ HA.class "font-medium" ] [ text "Type: " ], text (refTypeToString ref.type_) ]
            , Html.span [ HA.class "mr-4" ] [ Html.strong [ HA.class "font-medium" ] [ text "Label: " ], text ref.label ]
            , Html.span [] [ Html.strong [ HA.class "font-medium" ] [ text "URI: " ], text ref.uri ]
            ]
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
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please validate the rationale creation step first." ]
                ]

        ( Validating _ _, _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please validate the rationale creation step first." ]
                ]

        ( Done _ _, Preparing form ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.map ctx.wrapMsg <| viewRationaleSignatureForm ctx.jsonLdContexts form
                , Html.p []
                    [ Helper.viewButton "Skip rationale signing" (ctx.wrapMsg SkipRationaleSignaturesButtonClicked)
                    , text " or "
                    , Helper.viewButton "Validate rationale signing" (ctx.wrapMsg ValidateRationaleSignaturesButtonClicked)
                    ]
                , viewError form.error
                ]

        ( Done _ _, Validating _ _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Validating rationale author signatures ..." ]
                ]

        ( Done _ _, Done _ ratSig ) ->
            let
                downloadButton =
                    Html.a
                        [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode ratSig.signedJson
                        , HA.download "rationale-signed.json"
                        ]
                        [ Helper.viewButton "Download signed JSON rationale" NoMsg ]
            in
            if List.isEmpty ratSig.authors then
                Html.map ctx.wrapMsg <|
                    div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                        [ Html.h4 [ HA.class "text-3xl font-medium mb-4" ] [ text "Rationale Signature" ]
                        , div [ HA.style "display" "flex", HA.style "align-items" "center" ]
                            [ div [ HA.style "margin-right" "12px" ]
                                [ downloadButton ]
                            , div [ HA.style "margin-right" "12px" ]
                                [ Helper.viewButton "Generate PDF" (ConvertToPdfButtonClicked ratSig.signedJson) ]
                            ]
                        , Html.p [ HA.class "mt-4" ] [ text "No registered author." ]
                        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Update authors" ChangeAuthorsButtonClicked ]
                        ]

            else
                Html.map ctx.wrapMsg <|
                    div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                        [ Html.h4 [ HA.class "text-3xl font-medium" ] [ text "Rationale Signature" ]
                        , div [ HA.style "display" "flex", HA.style "align-items" "center" ]
                            [ div [ HA.style "margin-right" "12px" ]
                                [ downloadButton ]
                            , div [ HA.style "margin-right" "12px" ]
                                [ Helper.viewButton "Generate PDF" (ConvertToPdfButtonClicked ratSig.signedJson) ]
                            ]
                        , Html.ul [] (List.map viewSigner ratSig.authors)
                        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Update authors" ChangeAuthorsButtonClicked ]
                        ]


viewRationaleSignatureForm : JsonLdContexts -> RationaleSignatureForm -> Html Msg
viewRationaleSignatureForm jsonLdContexts ({ authors } as form) =
    let
        jsonRationale =
            (rationaleSignatureFromForm jsonLdContexts { form | authors = [] }).signedJson
    in
    div []
        [ Helper.formContainer
            [ Html.p [ HA.class "mb-4" ] [ text "Here is the JSON-LD rationale file generated from your rationale inputs." ]
            , Html.p [ HA.class "mb-6" ]
                [ Html.a
                    [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode jsonRationale
                    , HA.download "rationale.json"
                    ]
                    [ Helper.viewButton "Download JSON rationale" NoMsg ]
                ]
            ]
        , Html.h5 [ HA.class "text-xl font-medium" ] [ text "Authors" ]
        , Helper.formContainer
            [ Html.p [ HA.class "mb-4" ]
                [ text "Each author needs to sign the above metadata. "
                , text "For now, the only supported method is to download this json file, and sign it with cardano-signer. "
                , text "Later I plan to add the ability to sign directly with the web wallet (like Eternl)."
                ]
            , Html.pre [ HA.class "p-4 rounded-md border overflow-auto text-sm mb-6", HA.style "border-color" "#C6C6C6" ]
                [ text "cardano-signer.js sign --cip100 \\\n"
                , text "   --data-file rationale.json \\\n"
                , text "   --secret-key dummy.skey \\\n"
                , text "   --author-name \"The great Name\" \\\n"
                , text "   --out-file rationale-signed.json"
                ]
            , Html.p [ HA.class "mb-4" ]
                [ text "Add individual authors that contributed to this rationale. "
                , text "Provide each author signature or skip all signatures."
                ]
            , Html.p [ HA.class "mb-4" ] [ Helper.viewButton "Add an author" AddAuthorButtonClicked ]
            ]
        , div [] (List.indexedMap viewOneAuthorForm authors)
        ]


{-| Creates a JSON-LD document for the rationale that follows CIP-0136.
Includes:

  - Core rationale content
  - Authors and their signatures if present
  - Standard context and hash algorithm

-}
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
    Helper.formContainer
        [ div [ HA.class "flex items-center" ]
            [ div [ HA.class "flex-1 flex flex-col md:flex-row gap-4" ]
                [ div [ HA.class "w-full md:w-1/3" ]
                    [ Helper.labeledField "Author name"
                        (Helper.textFieldInline author.name (AuthorNameChange n))
                    ]
                , div [ HA.class "w-full md:w-2/3" ]
                    [ case author.signature of
                        Nothing ->
                            div [ HA.class "flex items-center" ]
                                [ Helper.labeledField "Signature"
                                    (div [ HA.class "flex items-center" ]
                                        [ Helper.viewButton "Load signature" (LoadJsonSignatureButtonClicked n author.name)
                                        , div [ HA.class "ml-2" ]
                                            [ Helper.viewButton "Delete" (DeleteAuthorButtonClicked n) ]
                                        ]
                                    )
                                ]

                        Just sig ->
                            div []
                                [ Helper.labeledField "Witness algorithm" (text author.witnessAlgorithm)
                                , Helper.labeledField "Public key" (text (String.left 12 author.publicKey ++ "..."))
                                , Helper.labeledField "Signature"
                                    (div [ HA.class "flex items-center" ]
                                        [ text (String.left 12 sig ++ "...")
                                        , div [ HA.class "ml-2" ]
                                            [ Helper.viewButton "Change" (LoadJsonSignatureButtonClicked n author.name) ]
                                        , div [ HA.class "ml-2" ]
                                            [ Helper.viewButton "Delete" (DeleteAuthorButtonClicked n) ]
                                        ]
                                    )
                                ]
                    ]
                ]
            ]
        ]


viewSigner : AuthorWitness -> Html Msg
viewSigner { name, witnessAlgorithm, publicKey, signature } =
    Html.li []
        [ Html.div []
            [ Html.strong [ HA.class "font-medium" ] [ text "Name: " ]
            , text name
            ]
        , case signature of
            Nothing ->
                Html.div [ HA.class "" ] [ text "No signature provided" ]

            Just sig ->
                div [ HA.class "text-sm" ]
                    [ Html.div []
                        [ Html.strong [ HA.class "font-medium" ] [ text "Witness algorithm: " ]
                        , text witnessAlgorithm
                        ]
                    , Html.div [ HA.class "font-mono" ]
                        [ Html.strong [ HA.class "font-medium" ] [ text "Public key: " ]
                        , text (String.left 10 publicKey ++ "..." ++ String.right 6 publicKey)
                        ]
                    , Html.div [ HA.class "font-mono" ]
                        [ Html.strong [ HA.class "font-medium" ] [ text "Signature: " ]
                        , text (String.left 10 sig ++ "..." ++ String.right 6 sig)
                        ]
                    ]
        ]



--
-- Storage Step
--


viewPermanentStorageStep : ViewContext msg -> Step RationaleSignatureForm {} RationaleSignature -> Step StorageForm {} Storage -> Html msg
viewPermanentStorageStep ctx rationaleSigStep step =
    case ( rationaleSigStep, step ) of
        ( Done _ _, Preparing form ) ->
            Html.map ctx.wrapMsg <|
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Permanent Storage" ]
                    , Html.p [ HA.class "mb-4" ]
                        [ text "Only the hash of your rationale is stored on Cardano,"
                        , text " so it's recommended to also store the actual JSON file containing the rationale in a permanent storage solution."
                        , text " Here we provide an easy way to store it on IPFS."
                        ]
                    , Helper.formContainer
                        [ Html.h4 [ HA.class "text-xl mt-4 mb-2" ] [ text "IPFS Method" ]
                        , div [ HA.class "flex items-center mb-2" ]
                            [ Html.input
                                [ HA.type_ "radio"
                                , HA.name "ipfs-method"
                                , HA.checked (form.storageMethod == StandardIPFS)
                                , onClick (StorageMethodSelected StandardIPFS)
                                , HA.class "mr-2"
                                ]
                                []
                            , Html.label [ HA.class "text-base" ] [ text "Pre-configured IPFS (Cardano Foundation)" ]
                            ]
                        , div [ HA.class "flex items-center mb-4" ]
                            [ Html.input
                                [ HA.type_ "radio"
                                , HA.name "ipfs-method"
                                , HA.checked (form.storageMethod == CustomIPFS)
                                , onClick (StorageMethodSelected CustomIPFS)
                                , HA.class "mr-2"
                                ]
                                []
                            , Html.label [ HA.class "text-base" ] [ text "Custom IPFS Provider" ]
                            ]
                        , if form.storageMethod == CustomIPFS then
                            div []
                                [ Helper.labeledField "IPFS RPC server:"
                                    (Helper.textFieldInline form.ipfsServer IpfsServerChange)
                                , div [ HA.class "mt-4" ]
                                    [ Helper.viewButton "Add header" AddHeaderButtonClicked ]
                                , Html.ul [ HA.class "my-4" ] (List.indexedMap viewHeader form.headers)
                                , Html.p [ HA.class "text-sm text-gray-600 mt-2" ]
                                    [ text "For example, use "
                                    , Html.a
                                        [ HA.href "https://blockfrost.dev/start-building/ipfs/"
                                        , HA.target "_blank"
                                        , HA.rel "noopener noreferrer"
                                        , HA.style "color" "#2563eb"
                                        , HA.style "text-decoration" "underline"
                                        ]
                                        [ text "Blockfrost" ]
                                    , text " or other IPFS providers."
                                    ]
                                ]

                          else
                            text ""
                        ]
                    , Html.p [] [ Helper.viewButton "Add to IPFS" PinJsonIpfsButtonClicked ]
                    , viewError form.error
                    ]

        ( Done _ _, Validating _ _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Permanent Storage" ]
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
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Permanent Storage" ]
                    , Helper.formContainer
                        [ Html.p [ HA.class "mb-4" ]
                            [ Html.strong [ HA.class "font-medium" ] [ text "File uploaded successfully:" ]
                            ]
                        , div [ HA.class " p-4 rounded-md border mb-4", HA.style "border-color" "#C6C6C6" ]
                            [ div [ HA.class "mb-2" ]
                                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Link:" ]
                                , Html.a
                                    [ HA.href link
                                    , HA.download storage.jsonFile.name
                                    , HA.target "_blank"
                                    , HA.class "text-blue-600 hover:text-blue-800 underline font-mono"
                                    ]
                                    [ text link ]
                                ]
                            , Html.ul [ HA.class "space-y-2 text-sm" ]
                                [ Html.li [ HA.class "flex" ]
                                    [ Html.span [ HA.class "font-bold w-24" ] [ text "Name: " ]
                                    , Html.span [ HA.class "font-mono" ] [ text storage.jsonFile.name ]
                                    ]
                                , Html.li [ HA.class "flex" ]
                                    [ Html.span [ HA.class "font-bold w-24" ] [ text "CID: " ]
                                    , Html.span [ HA.class "font-mono" ] [ text storage.jsonFile.cid ]
                                    ]
                                , Html.li [ HA.class "flex" ]
                                    [ Html.span [ HA.class "font-bold w-24" ] [ text "Size: " ]
                                    , Html.span [ HA.class "font-mono" ] [ text storage.jsonFile.size, text " Bytes" ]
                                    ]
                                , Html.li [ HA.class "flex" ]
                                    [ Html.span [ HA.class "font-bold w-24" ] [ text "File Hash: " ]
                                    , Html.span [ HA.class "font-mono break-all" ] [ text (Bytes.toHex dataHash) ]
                                    ]
                                ]
                            ]
                        , Html.p [] [ Helper.viewButton "Add another storage" AddOtherStorageButtonCLicked ]
                        ]
                    ]

        _ ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Permanent Storage" ]
                , Html.p [] [ text "Please complete the rationale signature step first." ]
                ]


viewHeader : Int -> ( String, String ) -> Html Msg
viewHeader n ( field, value ) =
    Helper.formContainer
        [ div [ HA.class "flex items-center" ]
            [ div [ HA.class "flex-1 flex gap-12" ]
                [ Helper.labeledField "Project ID"
                    (Helper.textFieldInline field (StorageHeaderFieldChange n))
                , Helper.labeledField "IPFS"
                    (Helper.textFieldInline value (StorageHeaderValueChange n))
                , Helper.viewButton "Delete" (DeleteHeaderButtonClicked n)
                ]
            ]
        ]



--
-- Fee Provider Step
--


viewFeeProviderStep : ViewContext msg -> Step FeeProviderForm FeeProviderTemp FeeProvider -> Html msg
viewFeeProviderStep ctx step =
    case step of
        Preparing form ->
            div [ HA.class "container mx-auto mb-4" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Fee Provider" ]
                , Html.map ctx.wrapMsg <| viewFeeProviderForm form
                , Html.p [] [ Helper.viewButton "Confirm Fee Provider" (ctx.wrapMsg ValidateFeeProviderFormButtonClicked) ]
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
            div [ HA.class "container mx-auto mb-4" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Fee Provider" ]
                , Html.p [] [ text "validating fee provider information ..." ]
                ]

        Done _ { address, utxos } ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Fee Provider" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "mb-4" ]
                        [ Html.strong [ HA.class "font-medium" ] [ text "Fee provider configured successfully:" ]
                        ]
                    , div [ HA.class "p-4 rounded-md border mb-4", HA.style "border-color" "#C6C6C6" ]
                        [ Html.div [ HA.class "mb-2" ]
                            [ Html.span [ HA.class "font-bold mr-2" ] [ text "Address:" ]
                            , Html.span [ HA.class "font-mono break-all" ] [ text <| prettyAddr address ]
                            ]
                        , Html.div []
                            [ Html.span [ HA.class "font-bold mr-2" ] [ text "Available UTxO count:" ]
                            , Html.span [ HA.class "font-mono" ] [ text <| String.fromInt (Dict.Any.size utxos) ]
                            ]
                        ]
                    , Html.p [] [ Helper.viewButton "Change fee provider" (ctx.wrapMsg ChangeFeeProviderButtonClicked) ]
                    ]
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
        [ Html.h4 [ HA.class "text-xl mt-4 mb-2" ] [ text "Payment Method" ]
        , Helper.formContainer
            [ div [ HA.class "flex items-center mb-2" ]
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name "fee-provider"
                    , HA.checked isUsingWalletForFees
                    , onClick (FeeProviderUpdated (ConnectedWalletFeeProvider { error = Nothing }))
                    , HA.class "mr-2"
                    ]
                    []
                , Html.label [ HA.class "text-base" ] [ text "Use connected wallet" ]
                ]
            , div [ HA.class "flex items-center mb-4" ]
                [ Html.input
                    [ HA.type_ "radio"
                    , HA.name "fee-provider"
                    , HA.checked (not isUsingWalletForFees)
                    , onClick (FeeProviderUpdated (ExternalFeeProvider { endpoint = "", error = Nothing }))
                    , HA.class "mr-2"
                    ]
                    []
                , Html.label [ HA.class "text-base" ] [ text "(WIP) Use external fee provider" ]
                ]
            , case feeProviderForm of
                ExternalFeeProvider { endpoint, error } ->
                    Helper.labeledField "External Provider Endpoint"
                        (Helper.textFieldInline endpoint (\s -> FeeProviderUpdated (ExternalFeeProvider { endpoint = s, error = error })))

                _ ->
                    text ""
            ]
        ]



--
-- Tx Building Step
--


viewBuildTxStep : ViewContext msg -> Model -> Html msg
viewBuildTxStep ctx model =
    case ( allPrepSteps ctx.costModels model, model.buildTxStep ) of
        ( Err error, _ ) ->
            let
                missingStepsDisplay =
                    case model.voterStep of
                        Preparing _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Voter identification not completed" ]

                        Validating _ _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Voter validation in progress" ]

                        Done _ _ ->
                            text ""

                proposalStepDisplay =
                    case model.pickProposalStep of
                        Preparing _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "No proposal selected" ]

                        Validating _ _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Proposal validation in progress" ]

                        Done _ _ ->
                            text ""

                rationaleStepDisplay =
                    case model.rationaleCreationStep of
                        Preparing _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Rationale not completed" ]

                        Validating _ _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Rationale validation in progress" ]

                        Done _ _ ->
                            text ""

                storageStepDisplay =
                    case model.permanentStorageStep of
                        Preparing _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Rationale not stored permanently" ]

                        Validating _ _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Storage validation in progress" ]

                        Done _ _ ->
                            text ""

                feeProviderStepDisplay =
                    case model.feeProviderStep of
                        Preparing _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Fee provider not set" ]

                        Validating _ _ ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Fee provider validation in progress" ]

                        Done _ _ ->
                            text ""

                costModelsDisplay =
                    case ctx.costModels of
                        Nothing ->
                            Html.div [ HA.class " mb-2" ] [ Html.strong [] [ text "⚠️ Missing: " ], text "Protocol parameters not loaded" ]

                        Just _ ->
                            text ""
            in
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Tx Building" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "text-gray-600 mb-4" ] [ text "Please complete the following steps before building the transaction:" ]
                    , div []
                        [ missingStepsDisplay
                        , proposalStepDisplay
                        , rationaleStepDisplay
                        , storageStepDisplay
                        , feeProviderStepDisplay
                        , costModelsDisplay
                        , if String.isEmpty error then
                            text ""

                          else
                            Html.div [ HA.class "text-red-600 mt-4 pt-4" ]
                                [ Html.strong [] [ text "Error: " ]
                                , text error
                                ]
                        ]
                    ]
                ]

        ( Ok _, Preparing { error } ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Tx Building" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "mb-4" ] [ text "Choose your vote:" ]
                    , div [ HA.style "display" "flex", HA.style "align-items" "center" ]
                        [ div [ HA.style "margin-right" "12px" ]
                            [ Helper.viewButton "Vote YES" (ctx.wrapMsg (BuildTxButtonClicked Gov.VoteYes)) ]
                        , div [ HA.style "margin-right" "12px" ]
                            [ Helper.viewButton "Vote NO" (ctx.wrapMsg (BuildTxButtonClicked Gov.VoteNo)) ]
                        , div []
                            [ Helper.viewButton "Vote ABSTAIN" (ctx.wrapMsg (BuildTxButtonClicked Gov.VoteAbstain)) ]
                        ]
                    ]
                , viewError error
                ]

        ( Ok _, Validating _ _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Tx Building" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "text-gray-600" ] [ text "Validating transaction information..." ] ]
                ]

        ( Ok _, Done _ { tx } ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Tx Building" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "mb-2" ] [ text "Transaction generated successfully (₳ displayed as lovelaces):" ]
                    , div [ HA.class "relative" ]
                        [ Html.pre
                            [ HA.style "padding" "1rem"
                            , HA.style "border-radius" "0.375rem"
                            , HA.style "border" "1px solid #C6C6C6"
                            , HA.style "overflow-x" "auto"
                            , HA.style "overflow-y" "auto"
                            , HA.style "margin-top" "0.5rem"
                            , HA.style "font-size" "0.875rem"
                            , HA.style "white-space" "pre-wrap"
                            , HA.style "word-break" "break-all"
                            , HA.style "word-wrap" "break-word"
                            , HA.style "max-height" "300px"
                            , HA.style "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace"
                            ]
                            [ text <| prettyTx tx ]
                        ]
                    , div [ HA.class "mt-4" ]
                        [ Helper.viewButton "Change vote" (ctx.wrapMsg ChangeVoteButtonClicked) ]
                    ]
                ]



--
-- Tx Signing Step
--


viewSignTxStep : ViewContext msg -> Step BuildTxPrep {} TxFinalized -> Html msg
viewSignTxStep ctx buildTxStep =
    case buildTxStep of
        Done _ { tx, expectedSignatures } ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Tx Signing" ]
                , Helper.formContainer
                    [ Html.h5 [ HA.class "text-xl font-medium mb-4" ] [ text "Finalize Your Vote" ]
                    , Html.p [ HA.class "mb-4" ] [ text "Expecting signatures for the following public key hashes:" ]
                    , div [ HA.class "p-4 rounded-md border mb-4", HA.style "border-color" "#C6C6C6" ]
                        [ Html.ul [ HA.class "font-mono text-sm space-y-2" ]
                            (List.map
                                (\hash ->
                                    Html.li [ HA.class "border-b pb-2 last:border-b-0 last:pb-0", HA.style "border-color" "#C6C6C6" ]
                                        [ text <| Bytes.toHex hash ]
                                )
                                expectedSignatures
                            )
                        ]
                    , Html.p [ HA.class "text-gray-800 mb-4" ]
                        [ text "Click the button below to proceed to the signing page where you can finalize and submit your voting transaction." ]
                    , ctx.signingLink tx
                        expectedSignatures
                        [ button
                            [ HA.style "display" "inline-flex"
                            , HA.style "align-items" "center"
                            , HA.style "justify-content" "center"
                            , HA.style "white-space" "nowrap"
                            , HA.style "border-radius" "9999px"
                            , HA.style "font-size" "0.875rem"
                            , HA.style "font-weight" "500"
                            , HA.style "transition" "all 0.2s"
                            , HA.style "outline" "none"
                            , HA.style "ring-offset" "background"
                            , HA.style "focus-visible:ring" "2px"
                            , HA.style "focus-visible:ring-color" "ring"
                            , HA.style "focus-visible:ring-offset" "2px"
                            , HA.style "background-color" "#272727"
                            , HA.style "color" "#f7fafc"
                            , HA.style "hover:bg-color" "#f9fafb"
                            , HA.style "hover:text-color" "#1a202c"
                            , HA.style "height" "4rem"
                            , HA.style "padding-left" "1.5rem"
                            , HA.style "padding-right" "1.5rem"
                            , HA.style "padding-top" "1.25rem"
                            , HA.style "padding-bottom" "1.25rem"
                            , HA.style "margin-top" "0.5rem"
                            , HA.style "margin-bottom" "0.5em"
                            ]
                            [ text "Go to Signing Page" ]
                        ]
                    ]
                ]

        _ ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Tx Signing" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "text-gray-600" ]
                        [ text "Please complete the Tx building step first." ]
                    ]
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
            Html.div [ HA.class "mt-4 p-4 bg-red-50 border rounded-md", HA.style "border-color" "#C6C6C6" ]
                [ Html.p [ HA.class "text-red-600 font-medium mb-2" ] [ text "Error:" ]
                , Html.pre [ HA.class "text-sm whitespace-pre-wrap" ] [ text err ]
                ]
