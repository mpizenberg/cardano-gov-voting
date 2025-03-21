module Page.Preparation exposing (InternalVote, JsonLdContexts, LoadedWallet, Model, Msg, MsgToParent(..), Rationale, Reference, ReferenceType(..), TaskCompleted, UpdateContext, ViewContext, handleTaskCompleted, init, noInternalVote, pinPdfFile, pinRationaleFile, update, view)

{-| This module handles the complete vote preparation workflow, from identifying
the voter to signing the transaction, which is handled by another page.

The workflow is split into the following sequential steps:

1.  Voter identification - Who is voting (DRep/SPO/CC)
2.  Proposal selection - What proposal to vote on
3.  IPFS storage configuration - How to store the rationale on IPFS
4.  Rationale creation - The reasoning behind the vote
5.  Rationale signing - Optional signatures from multiple authors
6.  Rationale storage - Storing rationale on IPFS
7.  Fee handling - How transaction fees will be paid
8.  Transaction building - Creating the vote transaction
9.  Transaction signing - Redirect to the signing page

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
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
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
import Helper exposing (prettyAdaLovelace)
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
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (RemoteData, WebData)
import ScriptInfo exposing (ScriptInfo)
import Set exposing (Set)
import Storage
import Task
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


type Model
    = Model InnerModel


{-| Main model containing the state for all preparation steps.
Each step uses the Step type to track its progress.
-}
type alias InnerModel =
    { someRefUtxos : Utxo.RefDict Output
    , voterStep : Step VoterPreparationForm VoterWitness VoterWitness
    , pickProposalStep : Step {} {} ActiveProposal
    , storageConfigStep : Step StorageForm {} StorageConfig
    , rationaleCreationStep : Step RationaleForm Rationale Rationale
    , rationaleSignatureStep : Step RationaleSignatureForm {} RationaleSignature
    , permanentStorageStep : Step { error : Maybe String } {} Storage
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


init : { label : String, description : String } -> Model
init ipfsPreconfig =
    Model
        { someRefUtxos = Utxo.emptyRefDict
        , voterStep = Preparing initVoterForm
        , pickProposalStep = Preparing {}
        , storageConfigStep = Preparing (initStorageForm ipfsPreconfig)
        , rationaleCreationStep = Preparing initRationaleForm
        , rationaleSignatureStep = Preparing initRationaleSignatureForm
        , permanentStorageStep = Preparing { error = Nothing }
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
    , pdfAutogen : Bool
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
    , pdfAutogen = True
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
    = PreconfigIPFS { label : String, description : String }
    | NmkrIPFS
    | BlockfrostIPFS
    | CustomIPFS


type alias StorageForm =
    { storageMethod : StorageMethod
    , nmkrUserId : String
    , nmkrApiToken : String
    , blockfrostProjectId : String
    , ipfsServer : String
    , headers : List ( String, String )
    , error : Maybe String
    }


initStorageForm : { label : String, description : String } -> StorageForm
initStorageForm ipfsPreconfig =
    { storageMethod = PreconfigIPFS ipfsPreconfig
    , nmkrUserId = ""
    , nmkrApiToken = ""
    , blockfrostProjectId = ""
    , ipfsServer = "https://ipfs.blockfrost.io/api/v0/ipfs"
    , headers = [ ( "project_id", "" ) ]
    , error = Nothing
    }


type StorageConfig
    = UsePreconfigIpfs { label : String, description : String }
    | UseNmkrIpfs { label : String, description : String, userId : String, apiToken : String }
    | UseCustomIpfs { label : String, description : String, ipfsServer : String, headers : List ( String, String ) }


type alias Storage =
    { jsonFile : IpfsFile
    }


type alias IpfsFile =
    { name : String
    , cid : String
    , size : String
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
      -- Storage Config Step
    | StorageMethodSelected StorageMethod
    | BlockfrostProjectIdChange String
    | NmkrUserIdChange String
    | NmkrApiTokenChange String
    | IpfsServerChange String
    | AddHeaderButtonClicked
    | DeleteHeaderButtonClicked Int
    | StorageHeaderFieldChange Int String
    | StorageHeaderValueChange Int String
    | ValidateStorageConfigButtonClicked
      -- Rationale
    | RationaleSummaryChange String
    | TogglePdfAutogen Bool
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
    | GotUnsignedPdfFile (Result Http.Error ElmBytes.Bytes)
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
    | GotSignedPdfFile (Result Http.Error ElmBytes.Bytes)
      -- Rationale Storage
    | PinJsonIpfsButtonClicked
    | GotIpfsAnswer (Result String IpfsAnswer)
    | AddOtherStorageButtonCLicked
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
    , jsonLdContexts : JsonLdContexts
    , jsonRationaleToFile : { fileContent : String, fileName : String } -> Cmd msg
    , pdfBytesToFile : { fileContentHex : String, fileName : String } -> Cmd msg
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
update ctx msg (Model model) =
    let
        ( updatedModel, cmd, toParent ) =
            innerUpdate ctx msg model
    in
    ( Model updatedModel, cmd, toParent )


innerUpdate : UpdateContext msg -> Msg -> InnerModel -> ( InnerModel, Cmd msg, Maybe MsgToParent )
innerUpdate ctx msg model =
    case msg of
        ShowMoreProposals currentCount ->
            ( { model | visibleProposalCount = currentCount + 10 }
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
                    ( updateVoterForm (\form -> { form | drepInfo = RemoteData.Failure error }) model
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
        -- Storage Configuration Step
        --
        StorageMethodSelected method ->
            ( updateStorageConfigForm (\form -> { form | storageMethod = method }) model
            , Cmd.none
            , Nothing
            )

        BlockfrostProjectIdChange projectId ->
            ( updateStorageConfigForm (\form -> { form | blockfrostProjectId = projectId }) model
            , Cmd.none
            , Nothing
            )

        NmkrUserIdChange userId ->
            ( updateStorageConfigForm (\form -> { form | nmkrUserId = userId }) model
            , Cmd.none
            , Nothing
            )

        NmkrApiTokenChange token ->
            ( updateStorageConfigForm (\form -> { form | nmkrApiToken = token }) model
            , Cmd.none
            , Nothing
            )

        IpfsServerChange ipfsServer ->
            ( updateStorageConfigForm (\form -> { form | ipfsServer = ipfsServer }) model
            , Cmd.none
            , Nothing
            )

        AddHeaderButtonClicked ->
            ( updateStorageConfigForm (\form -> { form | headers = form.headers ++ [ ( "", "" ) ] }) model
            , Cmd.none
            , Nothing
            )

        DeleteHeaderButtonClicked n ->
            ( updateStorageConfigForm (\form -> { form | headers = List.Extra.removeAt n form.headers }) model
            , Cmd.none
            , Nothing
            )

        StorageHeaderFieldChange n field ->
            ( updateStorageConfigForm (\form -> { form | headers = List.Extra.updateAt n (\( _, v ) -> ( field, v )) form.headers }) model
            , Cmd.none
            , Nothing
            )

        StorageHeaderValueChange n value ->
            ( updateStorageConfigForm (\form -> { form | headers = List.Extra.updateAt n (\( f, _ ) -> ( f, value )) form.headers }) model
            , Cmd.none
            , Nothing
            )

        ValidateStorageConfigButtonClicked ->
            case model.storageConfigStep of
                Preparing form ->
                    case validateIpfsForm form of
                        Ok storageConfig ->
                            -- TODO: test some endpoint to check custom config validity,
                            -- and return a "Validating" step instead of a "Done" step.
                            ( { model | storageConfigStep = Done { form | error = Nothing } storageConfig }
                            , Cmd.none
                            , Nothing
                            )

                        Err error ->
                            ( { model | storageConfigStep = Preparing { form | error = Just error } }
                            , Cmd.none
                            , Nothing
                            )

                -- When the user clicks on "Change storage configuration"
                -- TODO: Should be another msg
                Done prep _ ->
                    ( { model | storageConfigStep = Preparing prep }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( model, Cmd.none, Nothing )

        --
        -- Rationale Step
        --
        RationaleSummaryChange summary ->
            ( updateRationaleForm (\form -> { form | summary = summary }) model
            , Cmd.none
            , Nothing
            )

        TogglePdfAutogen pdfAutogen ->
            ( updateRationaleForm (\form -> { form | pdfAutogen = pdfAutogen }) model
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
            case ( validateRationaleForm model.rationaleCreationStep, model.pickProposalStep ) of
                -- If validation fails, it will return back to the form editing step with an error message
                ( Preparing formWithError, _ ) ->
                    ( { model | rationaleCreationStep = Preparing formWithError }, Cmd.none, Nothing )

                -- If validation partially succeeds but needs more processing,
                -- it will return in the Preparing step and we now need to store the PDF on IPFS,
                -- and then edit the rationale to include the PDF link
                ( Validating form rationale, Done _ { id } ) ->
                    let
                        tempUnsignedRationaleForm =
                            rationaleSignatureFromForm ctx.jsonLdContexts id { authors = [], error = Nothing, rationale = rationale }

                        rawFileContent =
                            tempUnsignedRationaleForm.signedJson
                    in
                    ( { model | rationaleCreationStep = Validating form rationale }
                      -- Save rationale PDF to IPFS and update rationale with link
                    , Api.defaultApiProvider.convertToPdf rawFileContent GotUnsignedPdfFile
                        |> Cmd.map ctx.wrapMsg
                    , Nothing
                    )

                -- If validation fully succeeds, it will proceed to the Done step
                ( Done prep newRationale, Done _ { id } ) ->
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
                                    Done form (rationaleSignatureFromForm ctx.jsonLdContexts id form)
                            }
                    in
                    ( updatedModel, Cmd.none, Nothing )

                _ ->
                    ( model, Cmd.none, Nothing )

        -- Handling PDF auto-gen for the rationale
        GotUnsignedPdfFile result ->
            case ( model.rationaleCreationStep, result ) of
                ( Validating _ _, Ok pdfBytes ) ->
                    ( model
                    , ctx.pdfBytesToFile
                        { fileContentHex = Bytes.fromBytes pdfBytes |> Bytes.toHex
                        , fileName = "unsigned-rationale.pdf"
                        }
                    , Nothing
                    )

                ( Validating form _, Err error ) ->
                    ( { model | rationaleCreationStep = Preparing { form | error = Just <| "An error occurred while converting the rationale to PDF: " ++ Debug.toString error } }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    -- Ignore if we are not validating the rationale
                    ( model, Cmd.none, Nothing )

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
            ( { model | rationaleSignatureStep = skipRationaleSignature ctx.jsonLdContexts model.pickProposalStep model.rationaleSignatureStep }
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
            , Api.defaultApiProvider.convertToPdf rawFileContent GotSignedPdfFile
                |> Cmd.map ctx.wrapMsg
            , Nothing
            )

        GotSignedPdfFile result ->
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
        -- Rationale Storage Step
        --
        PinJsonIpfsButtonClicked ->
            case ( model.storageConfigStep, model.permanentStorageStep ) of
                ( Done _ _, Preparing _ ) ->
                    sendPinRequest ctx model

                _ ->
                    ( model, Cmd.none, Nothing )

        GotIpfsAnswer (Err httpError) ->
            case ( model.rationaleCreationStep, model.permanentStorageStep ) of
                -- If we are validating the rationale form, it means the IPFS answer
                -- is most likely the PDF we got back for PDF auto-gen.
                ( Validating form _, _ ) ->
                    ( { model | rationaleCreationStep = Preparing { form | error = Just <| Debug.toString httpError } }
                    , Cmd.none
                    , Nothing
                    )

                -- Otherwise, if we are validating the permanent storage step, it means the IPFS answer
                -- is most likely for the signed JSON rationale.
                ( _, Validating form _ ) ->
                    ( { model | permanentStorageStep = Preparing { form | error = Just <| Debug.toString httpError } }
                    , Cmd.none
                    , Nothing
                    )

                -- Any other case is not supposed to happen.
                _ ->
                    ( model, Cmd.none, Nothing )

        GotIpfsAnswer (Ok ipfsAnswer) ->
            case ( model.rationaleCreationStep, model.permanentStorageStep ) of
                -- If we are validating the rationale form, it means the IPFS answer
                -- is most likely the PDF we got back for PDF auto-gen.
                ( Validating form rationale, _ ) ->
                    handlePdfIpfsAnswer ctx model form rationale ipfsAnswer

                -- Otherwise, if we are validating the permanent storage step, it means the IPFS answer
                -- is most likely for the signed JSON rationale.
                ( _, Validating _ _ ) ->
                    handleRationaleIpfsAnswer model ipfsAnswer

                _ ->
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
        -- Build Tx Step
        --
        BuildTxButtonClicked vote ->
            case allPrepSteps ctx model of
                Err error ->
                    ( { model | buildTxStep = Preparing { error = Just error } }
                    , Cmd.none
                    , Nothing
                    )

                Ok { voter, actionId, rationaleAnchor, localStateUtxos, walletAddress, costModels } ->
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
                                    (Cardano.AutoFee { paymentSource = walletAddress })
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
handleTaskCompleted task (Model model) =
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
                            ( Model { model | voterStep = Preparing { form | error = Just errorMsg } }
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
                                    ( Model { model | voterStep = Preparing { form | error = Just errorMsg } }
                                    , Cmd.none
                                    , Nothing
                                    )

                                Just output ->
                                    ( Model
                                        { model
                                            | voterStep = Done { form | error = Nothing } voterWitness
                                            , someRefUtxos = Dict.Any.insert outputRef output model.someRefUtxos
                                        }
                                    , Cmd.none
                                    , Nothing
                                    )

                ( Validating form _, Err httpError ) ->
                    ( Model { model | voterStep = Preparing { form | error = Just (Debug.toString httpError) } }
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    ( Model model, Cmd.none, Nothing )

        GotScriptInfoTask scriptInfoResult ->
            case scriptInfoResult of
                Err error ->
                    ( Model <| updateVoterForm (\form -> { form | scriptInfo = RemoteData.Failure error }) model
                    , Cmd.none
                    , Nothing
                    )

                Ok scriptInfo ->
                    ( Model <|
                        updateVoterForm
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


updateVoterForm : (VoterPreparationForm -> VoterPreparationForm) -> InnerModel -> InnerModel
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

                Script.Plutus plutusScript ->
                    let
                        witness =
                            { script = ( Script.plutusVersion plutusScript, WitnessValue <| Script.cborWrappedBytes plutusScript )
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



-- Storage Configuration Step


updateStorageConfigForm : (StorageForm -> StorageForm) -> InnerModel -> InnerModel
updateStorageConfigForm formUpdate model =
    case model.storageConfigStep of
        Preparing form ->
            { model | storageConfigStep = Preparing <| formUpdate form }

        Validating _ _ ->
            model

        Done _ _ ->
            model


validateIpfsForm : StorageForm -> Result String StorageConfig
validateIpfsForm form =
    case form.storageMethod of
        -- For standard IPFS, no validation needed
        PreconfigIPFS { label, description } ->
            Ok (UsePreconfigIpfs { label = label, description = description })

        BlockfrostIPFS ->
            case String.trim form.blockfrostProjectId of
                "" ->
                    Err "Missing blockfrost project id"

                projectId ->
                    Ok <|
                        UseCustomIpfs
                            { label = "Blockfrost IPFS"
                            , description = "Using Blockfrost IPFS server to store your files."
                            , ipfsServer = "https://ipfs.blockfrost.io/api/v0/ipfs"
                            , headers = [ ( "project_id", projectId ) ]
                            }

        NmkrIPFS ->
            case String.trim form.nmkrUserId of
                "" ->
                    Err "Missing nmkr user id"

                userId ->
                    Ok <|
                        UseNmkrIpfs
                            { label = "NMKR IPFS"
                            , description = "Using NMKR IPFS server to store your files."
                            , userId = userId
                            , apiToken = form.nmkrApiToken
                            }

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
                |> Result.map
                    (\_ ->
                        UseCustomIpfs
                            { label = "Custom IPFS Server"
                            , description = "Using a custom IPFS server configuration to store your files."
                            , ipfsServer = form.ipfsServer
                            , headers = form.headers
                            }
                    )



-- Rationale Step


updateRationaleForm : (RationaleForm -> RationaleForm) -> InnerModel -> InnerModel
updateRationaleForm f ({ rationaleCreationStep } as model) =
    case rationaleCreationStep of
        Preparing form ->
            { model | rationaleCreationStep = Preparing (f form) }

        _ ->
            model


updateRationaleInternalVoteForm : (Int -> InternalVote -> InternalVote) -> String -> InnerModel -> InnerModel
updateRationaleInternalVoteForm updateF numberStr model =
    let
        rationaleUpdate : RationaleForm -> RationaleForm
        rationaleUpdate form =
            String.toInt numberStr
                |> Maybe.map (\n -> { form | internalVote = updateF n form.internalVote })
                |> Maybe.withDefault form
    in
    updateRationaleForm rationaleUpdate model


validateMarkdownHeadings : List Markdown.Block.Block -> Result String ()
validateMarkdownHeadings blocks =
    if List.any isH1Block blocks then
        Err "Please use heading level 2 (##) or higher. Level 1 headings (#) are reserved for the level 1 sections, such as summary, rationale statement, discussion, etc."

    else
        Ok ()


isH1Block : Markdown.Block.Block -> Bool
isH1Block block =
    case block of
        Markdown.Block.Heading Markdown.Block.H1 _ ->
            True

        _ ->
            False


validateRationaleForm : Step RationaleForm Rationale Rationale -> Step RationaleForm Rationale Rationale
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
            case ( rationaleValidation, form.pdfAutogen ) of
                -- Without PDF autogeneration, validation is considered complete
                ( Ok _, False ) ->
                    let
                        formWithoutError =
                            { form | error = Nothing }
                    in
                    Done formWithoutError (rationaleFromForm formWithoutError)

                -- With PDF autogeneration, we will need to store on IPFS the PDF,
                -- and then auto-edit the rationale statement and references sections.
                ( Ok _, True ) ->
                    let
                        formWithoutError =
                            { form | error = Nothing }
                    in
                    Validating formWithoutError (rationaleFromForm formWithoutError)

                ( Err err, _ ) ->
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
                |> Result.andThen validateMarkdownHeadings


checkValidMarkdown : String -> Result String (List Markdown.Block.Block)
checkValidMarkdown str =
    let
        errorToString deadEnds =
            List.map Md.deadEndToString deadEnds
                |> String.join "\n\n"
    in
    Md.parse str
        |> Result.mapError errorToString


validateRationaleDiscussion : MarkdownForm -> Result String ()
validateRationaleDiscussion discussion =
    checkValidMarkdown (String.trim discussion)
        |> Result.andThen validateMarkdownHeadings


validateRationaleCounterArg : MarkdownForm -> Result String ()
validateRationaleCounterArg counterArg =
    checkValidMarkdown (String.trim counterArg)
        |> Result.andThen validateMarkdownHeadings


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


pinPdfFile : JD.Value -> Model -> ( Model, Cmd Msg )
pinPdfFile fileAsValue (Model model) =
    case ( JD.decodeValue File.decoder fileAsValue, model.storageConfigStep, model.rationaleCreationStep ) of
        ( Err error, _, Validating form _ ) ->
            ( Model { model | rationaleCreationStep = Preparing { form | error = Just <| JD.errorToString error } }
            , Cmd.none
            )

        ( Ok file, Done _ storageConfig, Validating _ _ ) ->
            ( Model model
            , case storageConfig of
                UsePreconfigIpfs _ ->
                    Api.defaultApiProvider.ipfsAddFile
                        { file = file }
                        GotIpfsAnswer

                UseNmkrIpfs { userId, apiToken } ->
                    Api.defaultApiProvider.ipfsAddFileNmkr
                        { userId = userId
                        , apiToken = apiToken
                        , file = file
                        }
                        GotIpfsAnswer

                UseCustomIpfs { ipfsServer, headers } ->
                    Api.defaultApiProvider.ipfsAddFileCustom
                        { rpc = ipfsServer
                        , headers = headers
                        , file = file
                        }
                        GotIpfsAnswer
            )

        -- Ignore if we aren't validating the rationale storage step
        _ ->
            ( Model model, Cmd.none )


editRationale : Step RationaleForm Rationale Rationale -> Step RationaleForm Rationale Rationale
editRationale step =
    case step of
        Preparing _ ->
            step

        Validating rationaleForm _ ->
            Preparing rationaleForm

        Done prep _ ->
            Preparing prep



-- Rationale Signature Step


encodeJsonLdRationale : Gov.ActionId -> Rationale -> JE.Value
encodeJsonLdRationale actionId rationale =
    JE.object <|
        List.filterMap identity
            [ Just ( "govActionId", JE.string <| Gov.idToBech32 <| GovActionId actionId )
            , Just ( "summary", JE.string rationale.summary )
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


updateAuthorsForm : (List AuthorWitness -> List AuthorWitness) -> InnerModel -> InnerModel
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


skipRationaleSignature : JsonLdContexts -> Step {} {} ActiveProposal -> Step RationaleSignatureForm {} RationaleSignature -> Step RationaleSignatureForm {} RationaleSignature
skipRationaleSignature jsonLdContexts pickProposalStep step =
    case ( pickProposalStep, step ) of
        ( Done _ { id }, Preparing ({ authors } as form) ) ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    { form | authors = List.map (\a -> { a | signature = Nothing }) authors }
                        |> rationaleSignatureFromForm jsonLdContexts id
                        |> Done form

        ( Done _ { id }, Validating ({ authors } as form) _ ) ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    { form | authors = List.map (\a -> { a | signature = Nothing }) authors }
                        |> rationaleSignatureFromForm jsonLdContexts id
                        |> Done form

        _ ->
            step


validateRationaleSignature : JsonLdContexts -> InnerModel -> ( InnerModel, Cmd msg, Maybe MsgToParent )
validateRationaleSignature jsonLdContexts model =
    case ( model.rationaleSignatureStep, model.pickProposalStep ) of
        ( Preparing ({ authors } as form), Done _ activeProposal ) ->
            case validateAuthorsForm authors of
                Err error ->
                    ( { model | rationaleSignatureStep = Preparing { form | error = Just error } }
                    , Cmd.none
                    , Nothing
                    )

                Ok _ ->
                    -- TODO: change to Validating instead, and emit a command to check signatures
                    ( { model | rationaleSignatureStep = Done form <| rationaleSignatureFromForm jsonLdContexts activeProposal.id form }
                    , Cmd.none
                    , Nothing
                    )

        _ ->
            ( model, Cmd.none, Nothing )


rationaleSignatureFromForm : JsonLdContexts -> Gov.ActionId -> RationaleSignatureForm -> RationaleSignature
rationaleSignatureFromForm jsonLdContexts actionId form =
    { authors = form.authors
    , rationale = form.rationale
    , signedJson =
        createJsonRationale jsonLdContexts actionId form.rationale form.authors
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



-- Rationale Storage Step


sendPinRequest : UpdateContext msg -> InnerModel -> ( InnerModel, Cmd msg, Maybe MsgToParent )
sendPinRequest ctx model =
    case model.rationaleSignatureStep of
        Done _ ratSig ->
            ( { model | permanentStorageStep = Validating { error = Nothing } {} }
            , ctx.jsonRationaleToFile
                { fileContent = ratSig.signedJson
                , fileName = "rationale-signed.json"
                }
            , Nothing
            )

        _ ->
            ( { model | permanentStorageStep = Preparing { error = Just "Validate the rationale signature step first." } }
            , Cmd.none
            , Nothing
            )


pinRationaleFile : JD.Value -> Model -> ( Model, Cmd Msg )
pinRationaleFile fileAsValue (Model model) =
    case ( JD.decodeValue File.decoder fileAsValue, model.storageConfigStep, model.permanentStorageStep ) of
        ( Err error, _, Validating _ _ ) ->
            ( Model { model | permanentStorageStep = Preparing { error = Just <| JD.errorToString error } }
            , Cmd.none
            )

        ( Ok file, Done _ storageConfig, Validating _ _ ) ->
            ( Model model
            , case storageConfig of
                UsePreconfigIpfs _ ->
                    Api.defaultApiProvider.ipfsAddFile
                        { file = file }
                        GotIpfsAnswer

                UseNmkrIpfs { userId, apiToken } ->
                    Api.defaultApiProvider.ipfsAddFileNmkr
                        { userId = userId
                        , apiToken = apiToken
                        , file = file
                        }
                        GotIpfsAnswer

                UseCustomIpfs { ipfsServer, headers } ->
                    Api.defaultApiProvider.ipfsAddFileCustom
                        { rpc = ipfsServer
                        , headers = headers
                        , file = file
                        }
                        GotIpfsAnswer
            )

        -- Ignore if we aren't validating the rationale storage step
        _ ->
            ( Model model, Cmd.none )


handlePdfIpfsAnswer : UpdateContext msg -> InnerModel -> RationaleForm -> Rationale -> IpfsAnswer -> ( InnerModel, Cmd msg, Maybe MsgToParent )
handlePdfIpfsAnswer ctx model form rationale ipfsAnswer =
    case ( model.pickProposalStep, ipfsAnswer ) of
        ( _, IpfsError error ) ->
            ( { model | rationaleCreationStep = Preparing { form | error = Just error } }
            , Cmd.none
            , Nothing
            )

        -- Edit the rationale to prepend a link to the PDF at the beginning of the rationale statement,
        -- and in the list of references.
        ( Done _ { id }, IpfsAddSuccessful file ) ->
            let
                pdfLink =
                    "https://ipfs.io/ipfs/" ++ file.cid

                updatedRationaleStatement =
                    "A [PDF version][pdf-link] of this rationale is also made available."
                        ++ "\n\n"
                        ++ ("[pdf-link]: " ++ pdfLink)
                        ++ "\n\n"
                        ++ rationale.rationaleStatement

                updatedReferences =
                    rationale.references
                        ++ [ { type_ = OtherRefType
                             , label = "Rationale PDF"
                             , uri = "ipfs://" ++ file.cid
                             }
                           ]

                updatedRationale =
                    { rationale
                        | rationaleStatement = updatedRationaleStatement
                        , references = updatedReferences
                    }

                -- Initialize rationale signature with no author
                rationaleSignatureForm =
                    { authors = []
                    , rationale = updatedRationale
                    , error = Nothing
                    }
            in
            ( { model
                | rationaleCreationStep = Done { form | error = Nothing } updatedRationale
                , rationaleSignatureStep =
                    Done rationaleSignatureForm (rationaleSignatureFromForm ctx.jsonLdContexts id rationaleSignatureForm)
              }
            , Cmd.none
            , Nothing
            )

        -- Do nothing if no proposal was picked yet
        ( _, IpfsAddSuccessful _ ) ->
            ( model, Cmd.none, Nothing )


handleRationaleIpfsAnswer : InnerModel -> IpfsAnswer -> ( InnerModel, Cmd msg, Maybe MsgToParent )
handleRationaleIpfsAnswer model ipfsAnswer =
    case ipfsAnswer of
        IpfsError error ->
            ( { model | permanentStorageStep = Preparing { error = Just error } }
            , Cmd.none
            , Nothing
            )

        IpfsAddSuccessful file ->
            ( { model | permanentStorageStep = Done { error = Nothing } { jsonFile = file } }
            , Cmd.none
            , Nothing
            )



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
    , walletAddress : Address
    , costModels : CostModels
    }


allPrepSteps : { a | loadedWallet : Maybe LoadedWallet, costModels : Maybe CostModels } -> InnerModel -> Result String TxRequirements
allPrepSteps { loadedWallet, costModels } m =
    case ( costModels, ( m.voterStep, m.pickProposalStep, m.rationaleSignatureStep ), ( m.permanentStorageStep, loadedWallet ) ) of
        ( Just theCostModels, ( Done _ voter, Done _ p, Done _ r ), ( Done _ s, Just { utxos, changeAddress } ) ) ->
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
                , localStateUtxos = Dict.Any.union m.someRefUtxos utxos
                , walletAddress = changeAddress
                , costModels = theCostModels
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
  - The current epoch
  - Available proposals to vote on
  - JSON-LD metadata context for rationale
  - Protocol parameters
  - The network ID
  - Link to transaction signing page
  - IPFS pre-configuration description

-}
type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , loadedWallet : Maybe LoadedWallet
    , epoch : Maybe Int
    , proposals : WebData (Dict String ActiveProposal)
    , jsonLdContexts : JsonLdContexts
    , costModels : Maybe CostModels
    , networkId : NetworkId
    , signingLink : Transaction -> List { keyName : String, keyHash : Bytes CredentialHash } -> List (Html msg) -> Html msg
    , ipfsPreconfig : { label : String, description : String }
    }


view : ViewContext msg -> Model -> Html msg
view ctx (Model model) =
    div [ HA.style "max-width" "1440px", HA.style "margin" "0 auto" ]
        [ viewPageHeader
        , div
            [ HA.style "max-width" "840px"
            , HA.style "margin" "0 auto"
            , HA.style "padding" "0 1.5rem"
            ]
            [ viewVoterIdentificationStep ctx model.voterStep
            , viewDivider
            , viewProposalSelectionStep ctx model
            , viewDivider
            , viewStorageConfigStep ctx model.storageConfigStep
            , viewDivider
            , viewRationaleStep ctx model.pickProposalStep model.storageConfigStep model.rationaleCreationStep
            , viewDivider
            , viewRationaleSignatureStep ctx model.pickProposalStep model.rationaleCreationStep model.rationaleSignatureStep
            , viewDivider
            , viewPermanentStorageStep ctx model.rationaleSignatureStep model.storageConfigStep model.permanentStorageStep
            , viewDivider
            , viewBuildTxStep ctx model
            , viewDivider
            , viewSignTxStep ctx model.voterStep model.buildTxStep
            ]
        ]


viewPageHeader : Html msg
viewPageHeader =
    div
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
        , viewHeaderBackground
        ]


viewHeaderBackground : Html msg
viewHeaderBackground =
    div
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


viewDivider : Html msg
viewDivider =
    Html.hr [ HA.style "margin-top" "3rem", HA.style "border-color" "#C7C7C7" ] []



--
-- Voter Identification Step
--


sectionTitle : String -> Html msg
sectionTitle title =
    Html.h2 [ HA.class "text-3xl font-medium mt-4 mb-4" ] [ text title ]


viewVoterIdentificationStep : ViewContext msg -> Step VoterPreparationForm VoterWitness VoterWitness -> Html msg
viewVoterIdentificationStep ctx step =
    case step of
        Preparing form ->
            Html.map ctx.wrapMsg <|
                div []
                    [ sectionTitle "Voter governance ID (drep/pool/cc_hot)"
                    , Html.p [] [ Helper.firstTextField "Enter drep/pool/cc_hot" (Maybe.withDefault "" <| Maybe.map Gov.idToBech32 form.govId) VoterGovIdChange ]
                    , Html.Lazy.lazy viewValidGovIdForm form
                    , Html.p [ HA.class "my-4" ] [ Helper.viewButton "Confirm Voter" ValidateVoterFormButtonClicked ]
                    , viewError form.error
                    ]

        Validating _ _ ->
            div []
                [ sectionTitle "Voter governance ID (drep/pool/cc_hot)"
                , Helper.boxContainer [ Html.p [] [ text "validating voter information ..." ] ]
                ]

        Done form voter ->
            Html.map ctx.wrapMsg <| viewIdentifiedVoter form voter


viewValidGovIdForm : VoterPreparationForm -> Html Msg
viewValidGovIdForm form =
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


viewVotingPower : (a -> Int) -> WebData a -> Html Msg
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
                    refScriptSuggestionView refScriptFeeSavings utxoRefForm
            in
            case script of
                Script.Native _ ->
                    if nativeCborEncodingMatchesHash == Just True then
                        div []
                            [ Html.p [] [ text "Type of script: Native Script" ]
                            , refScriptSuggestion
                            , viewScriptSignersSection expectedSigners
                            ]

                    else
                        div []
                            [ Html.p [] [ text "Type of script: Native Script" ]
                            , Html.p [] [ text <| "IMPORTANT: for technical reasons, we need you to provide a reference UTxO containing your script of hash: " ++ Bytes.toHex scriptHash ]
                            , utxoRefForm
                            , viewScriptSignersSection expectedSigners
                            ]

                Script.Plutus plutusScript ->
                    div []
                        [ Html.p [] [ text <| "Plutus script version: " ++ Debug.toString (Script.plutusVersion plutusScript) ]
                        , Html.p [] [ text <| "Script size: " ++ (String.fromInt <| Bytes.width <| Script.cborWrappedBytes plutusScript) ++ " Bytes" ]
                        , refScriptSuggestion
                        , Html.p [] [ text "WIP: we are waiting for someone needing this to implement Plutus voters" ]
                        ]


refScriptSuggestionView : Int -> Html Msg -> Html Msg
refScriptSuggestionView refScriptFeeSavings utxoRefForm =
    if refScriptFeeSavings >= 5000 then
        -- Suggest reference script for more than ₳0.005 savings
        div []
            [ Html.p [] [ text <| "By using a reference input for your script, you could save this much in Tx fees: " ++ prettyAdaLovelace (Natural.fromSafeInt refScriptFeeSavings) ]
            , utxoRefForm
            ]

    else if refScriptFeeSavings <= -5000 then
        -- Print warning for more than ₳0.005 additional cost of using a reference script
        Html.p [] [ text <| "Weirdly, using a reference input for your script would cost you more: " ++ prettyAdaLovelace (Natural.fromSafeInt -refScriptFeeSavings) ]

    else
        -- Just ignore if it doesn’t affect the fees in any significant way
        text ""


viewScriptSignersSection : Dict String { expected : Bool, key : Bytes CredentialHash } -> Html Msg
viewScriptSignersSection expectedSigners =
    let
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
    div []
        [ Html.p [] [ text <| "Expected signers: (each adds " ++ prettyAdaLovelace additionalSignerCost ++ " to the Tx fees)" ]
        , div [] (List.map viewExpectedSignerCheckbox <| Dict.values expectedSigners)
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
            getVoterDisplayInfo voter form govIdStr
    in
    div []
        [ sectionTitle "Voter Information"
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


getVoterDisplayInfo : VoterWitness -> VoterPreparationForm -> String -> ( String, CredentialWitness )
getVoterDisplayInfo voter form govIdStr =
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



--
-- Proposal Selection Step
--


viewProposalSelectionStep : ViewContext msg -> InnerModel -> Html msg
viewProposalSelectionStep ctx model =
    case model.pickProposalStep of
        Preparing _ ->
            viewProposalSelectionForm ctx model

        Validating _ _ ->
            div []
                [ sectionTitle "Pick a Proposal"
                , Html.p [] [ text "Validating the picked proposal ..." ]
                ]

        Done _ proposal ->
            viewSelectedProposal ctx proposal


viewProposalSelectionForm : ViewContext msg -> InnerModel -> Html msg
viewProposalSelectionForm ctx model =
    div [ HA.style "padding-top" "50px", HA.style "padding-bottom" "8px" ]
        [ viewProposalHeader ctx.networkId
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
                viewProposalList ctx proposalsDict model.visibleProposalCount
        ]


viewProposalHeader : NetworkId -> Html msg
viewProposalHeader networkId =
    div [ HA.class "flex items-center mb-4" ]
        [ Html.h2 [ HA.class "text-3xl font-medium" ] [ text "Pick a Proposal" ]
        , viewNetworkBadge networkId
        ]


viewNetworkBadge : NetworkId -> Html msg
viewNetworkBadge networkId =
    let
        ( bgColor, textColor, networkName ) =
            case networkId of
                Mainnet ->
                    ( "rgba(16, 185, 129, 0.1)", "#065f46", "Mainnet" )

                Testnet ->
                    ( "rgba(124, 58, 237, 0.1)", "#5b21b6", "Testnet" )
    in
    Html.span
        [ HA.style "background-color" bgColor
        , HA.style "color" textColor
        , HA.style "font-size" "0.7rem"
        , HA.style "font-weight" "600"
        , HA.style "padding" "0.15rem 0.5rem"
        , HA.style "border-radius" "9999px"
        , HA.style "white-space" "nowrap"
        , HA.style "margin-left" "8px"
        ]
        [ text networkName ]


viewProposalList : ViewContext msg -> Dict String ActiveProposal -> Int -> Html msg
viewProposalList ctx proposalsDict visibleCount =
    if Dict.isEmpty proposalsDict then
        div [ HA.style "text-align" "center", HA.style "padding" "2rem", HA.style "color" "#666" ]
            [ text "No active proposals found." ]

    else
        let
            epochVisibility =
                Maybe.withDefault 0 ctx.epoch

            allProposals =
                Dict.values proposalsDict
                    |> List.filter (\p -> p.epoch_validity.end >= epochVisibility)

            totalProposalCount =
                List.length allProposals

            visibleProposals =
                List.sortBy (\proposal -> proposal.epoch_validity.end) allProposals
                    |> List.take visibleCount

            hasMore =
                totalProposalCount > visibleCount
        in
        div []
            [ Html.p [ HA.style "margin-bottom" "1rem" ]
                [ text <| "Select a proposal to vote on (" ++ String.fromInt totalProposalCount ++ " available):" ]
            , div [ HA.style "display" "grid", HA.style "grid-template-columns" "repeat(auto-fill, minmax(250px, 1fr))", HA.style "gap" "1.5rem" ]
                (List.map (viewProposalCard ctx.wrapMsg ctx.networkId) visibleProposals)
            , viewShowMoreButton ctx.wrapMsg hasMore visibleCount totalProposalCount
            ]


viewShowMoreButton : (Msg -> msg) -> Bool -> Int -> Int -> Html msg
viewShowMoreButton wrapMsg hasMore visibleCount totalCount =
    if hasMore then
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
                , HA.style "font-size" "0.875rem"
                , onClick (wrapMsg (ShowMoreProposals visibleCount))
                ]
                [ text <| "Show More (" ++ String.fromInt (min 10 (totalCount - visibleCount)) ++ " of " ++ String.fromInt (totalCount - visibleCount) ++ " remaining)" ]
            ]

    else
        text ""


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
            , HA.style "display" "flex"
            , HA.style "justify-content" "space-between"
            , HA.style "align-items" "center"
            ]
            [ Html.h3
                [ HA.style "font-weight" "600"
                , HA.style "font-size" "1rem"
                , HA.style "color" "#1A202C"
                , HA.style "line-height" "1.4"
                , HA.style "word-wrap" "break-word"
                , HA.style "flex" "1"
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
                    , HA.style "margin-bottom" "0.3rem"
                    ]
                    [ Html.div [ HA.style "display" "flex", HA.style "align-items" "center" ]
                        [ Html.span [ HA.style "font-weight" "500", HA.style "color" "#4A5568", HA.style "margin-right" "0.5rem" ] [ text "ID:" ]
                        , Html.a
                            [ HA.href <| cardanoScanActionUrl networkId proposal.id
                            , HA.target "_blank"
                            , HA.rel "noopener noreferrer"
                            , HA.style "display" "inline-flex"
                            , HA.style "align-items" "center"
                            , HA.style "color" "#3182CE"
                            , HA.style "text-decoration" "underline"
                            , HA.style "cursor" "pointer"
                            , HA.title "View on Cardanoscan (opens in new tab)"
                            ]
                            [ text <| strBothEnds 5 5 <| Bytes.toHex proposal.id.transactionId
                            , text <| "#" ++ String.fromInt proposal.id.govActionIndex
                            , Html.span
                                [ HA.style "margin-left" "0.25rem"
                                , HA.style "font-size" "0.8rem"
                                ]
                                [ text "↗" ]
                            ]
                        ]
                    , div
                        [ HA.style "font-size" "0.75rem"
                        , HA.style "font-weight" "500"
                        , HA.style "color" "#4A5568"
                        , HA.style "background-color" "#EDF2F7"
                        , HA.style "padding" "0.25rem 0.5rem"
                        , HA.style "border-radius" "9999px"
                        , HA.style "margin-left" "0.5rem"
                        ]
                        [ text proposal.actionType
                        , Helper.viewActionTypeIcon proposal.actionType
                        ]
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



-- Helper function to generate CardanoScan URL


cardanoScanActionUrl : NetworkId -> ActionId -> String
cardanoScanActionUrl networkId id =
    let
        baseUrl =
            case networkId of
                Mainnet ->
                    "https://cardanoscan.io/govAction/"

                Testnet ->
                    "https://preview.cardanoscan.io/govAction/"
    in
    baseUrl
        ++ (id.transactionId |> Bytes.toHex)
        ++ (Bytes.toHex <| Bytes.fromBytes <| Cbor.Encode.encode (Cbor.Encode.int id.govActionIndex))


cardanoScanActionLink : NetworkId -> ActionId -> Html msg
cardanoScanActionLink networkId id =
    let
        url =
            cardanoScanActionUrl networkId id
    in
    Html.a
        [ HA.href url
        , HA.target "_blank"
        , HA.rel "noopener noreferrer"
        , HA.class "text-blue-600 hover:text-blue-800 underline font-mono"
        ]
        [ text <| strBothEnds 8 8 <| Bytes.toHex id.transactionId
        , text <| "#" ++ String.fromInt id.govActionIndex
        , Html.span
            [ HA.style "margin-left" "0.25rem"
            , HA.style "font-size" "0.8rem"
            ]
            [ text "↗" ]
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


viewSelectedProposal : ViewContext msg -> ActiveProposal -> Html msg
viewSelectedProposal ctx { id, actionType, metadata, metadataUrl } =
    let
        ( title, content ) =
            getProposalContent metadata metadataUrl
    in
    div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
        [ div [ HA.class "flex items-center mb-4" ]
            [ sectionTitle "Pick a Proposal"
            , viewNetworkBadge ctx.networkId
            ]
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


getProposalContent : RemoteData String ProposalMetadata -> String -> ( String, Maybe (Html msg) )
getProposalContent metadata metadataUrl =
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



--
-- Storage Configuration Step
--


viewStorageConfigStep : ViewContext msg -> Step StorageForm {} StorageConfig -> Html msg
viewStorageConfigStep ctx step =
    case step of
        Preparing form ->
            Html.map ctx.wrapMsg <|
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ sectionTitle "Storage Configuration"
                    , Html.p [ HA.class "mb-4" ]
                        [ text "Only the hash of your rationale is stored on Cardano,"
                        , text " so it's recommended to also store the actual JSON file containing the rationale in a permanent storage solution."
                        , text " Here we provide an easy way to store it on IPFS."
                        ]
                    , Helper.formContainer
                        [ Html.h4 [ HA.class "text-xl mt-4 mb-2" ] [ text "IPFS Method" ]
                        , Helper.radioInput
                            { group = "ipfs-method"
                            , label = ctx.ipfsPreconfig.label
                            , checked =
                                case form.storageMethod of
                                    PreconfigIPFS _ ->
                                        True

                                    _ ->
                                        False
                            , onClick = StorageMethodSelected <| PreconfigIPFS ctx.ipfsPreconfig
                            }
                        , Helper.radioInput
                            { group = "ipfs-method"
                            , label = "Blockfrost IPFS Provider"
                            , checked = form.storageMethod == BlockfrostIPFS
                            , onClick = StorageMethodSelected BlockfrostIPFS
                            }
                        , Helper.radioInput
                            { group = "ipfs-method"
                            , label = "NMKR IPFS Provider"
                            , checked = form.storageMethod == NmkrIPFS
                            , onClick = StorageMethodSelected NmkrIPFS
                            }
                        , Helper.radioInput
                            { group = "ipfs-method"
                            , label = "Custom IPFS Provider"
                            , checked = form.storageMethod == CustomIPFS
                            , onClick = StorageMethodSelected CustomIPFS
                            }
                        , case form.storageMethod of
                            BlockfrostIPFS ->
                                div []
                                    [ Helper.labeledField "Blockfrost project ID:"
                                        (Helper.textFieldInline form.blockfrostProjectId BlockfrostProjectIdChange)
                                    ]

                            NmkrIPFS ->
                                div [ HA.class "flex-1 flex gap-12" ]
                                    [ Helper.labeledField "NMKR user ID:"
                                        (Helper.textFieldInline form.nmkrUserId NmkrUserIdChange)
                                    , Helper.labeledField "NMKR API token:"
                                        (Helper.textFieldInline form.nmkrApiToken NmkrApiTokenChange)
                                    ]

                            CustomIPFS ->
                                div []
                                    [ Helper.labeledField "IPFS RPC server:"
                                        (Helper.textFieldInline form.ipfsServer IpfsServerChange)
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
                                        , div [ HA.class "mt-4" ]
                                            [ Helper.viewButton "Add HTTP header" AddHeaderButtonClicked ]
                                        ]
                                    ]

                            PreconfigIPFS _ ->
                                text ""
                        ]
                    , Html.p [] [ Helper.viewButton "Validate storage config" ValidateStorageConfigButtonClicked ]
                    , viewError form.error
                    ]

        Validating _ _ ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Storage Configuration" ]
                , Html.p [] [ text "Validating storage configuration ..." ]
                ]

        Done _ storageConfig ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Storage Configuration" ]
                , Helper.formContainer
                    [ Html.p [ HA.class "mb-4" ]
                        [ Html.strong [ HA.class "font-medium" ] [ text "Selected storage method:" ] ]
                    , div [ HA.class "p-4 rounded-md border mb-4", HA.style "border-color" "#C6C6C6" ]
                        [ case storageConfig of
                            UsePreconfigIpfs { label, description } ->
                                div [ HA.class "flex flex-col space-y-2" ]
                                    [ div [ HA.class "flex items-center" ]
                                        [ Html.span [ HA.class "font-medium" ] [ text label ]
                                        ]
                                    , Html.p [ HA.class "text-gray-600 text-sm ml-8" ]
                                        [ text description ]
                                    ]

                            UseNmkrIpfs { label, description } ->
                                div [ HA.class "flex flex-col space-y-2" ]
                                    [ div [ HA.class "flex items-center" ]
                                        [ Html.span [ HA.class "font-medium" ] [ text label ]
                                        ]
                                    , Html.p [ HA.class "text-gray-600 text-sm ml-8" ]
                                        [ text description ]
                                    ]

                            UseCustomIpfs { label, description, ipfsServer } ->
                                div [ HA.class "flex flex-col space-y-2" ]
                                    [ div [ HA.class "flex items-center" ]
                                        [ Html.span [ HA.class "font-medium" ] [ text label ]
                                        ]
                                    , Html.p [ HA.class "text-gray-600 text-sm ml-8" ]
                                        [ text description ]
                                    , Html.p [ HA.class "ml-8" ]
                                        [ Html.span [ HA.class "font-bold mr-2" ] [ text "Server:" ]
                                        , Html.a
                                            [ HA.href ipfsServer
                                            , HA.target "_blank"
                                            , HA.rel "noopener noreferrer"
                                            , HA.class "font-mono break-all text-blue-600 hover:text-blue-800 underline"
                                            ]
                                            [ text ipfsServer ]
                                        ]
                                    ]
                        ]
                    ]
                , Html.p [] [ Helper.viewButton "Change storage configuration" (ctx.wrapMsg ValidateStorageConfigButtonClicked) ]
                ]



--
-- Rationale Step
--


viewRationaleStep :
    ViewContext msg
    -> Step {} {} ActiveProposal
    -> Step StorageForm {} StorageConfig
    -> Step RationaleForm Rationale Rationale
    -> Html msg
viewRationaleStep ctx pickProposalStep storageConfigStep step =
    Html.map ctx.wrapMsg <|
        case ( pickProposalStep, storageConfigStep, step ) of
            ( Done _ _, Done _ _, Preparing form ) ->
                viewRationaleForm form

            ( Done _ _, Done _ _, Validating _ _ ) ->
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ sectionTitle "Vote Rationale"
                    , Helper.formContainer
                        [ Html.p [ HA.class "text-gray-600" ] [ text "Auto-generation of PDF in progress ..." ]
                        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Edit rationale" EditRationaleButtonClicked ]
                        ]
                    ]

            ( Done _ _, Done _ _, Done _ rationale ) ->
                viewCompletedRationale rationale

            ( _, Done _ _, _ ) ->
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ sectionTitle "Vote Rationale"
                    , Html.p [] [ text "Please pick a proposal first." ]
                    ]

            ( Done _ _, _, _ ) ->
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ sectionTitle "Vote Rationale"
                    , Html.p [] [ text "Please validate the IPFS config step first." ]
                    ]

            _ ->
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ sectionTitle "Vote Rationale"
                    , Html.p [] [ text "Please pick a proposal and  validate the IPFS config step first." ]
                    ]


viewRationaleForm : RationaleForm -> Html Msg
viewRationaleForm form =
    div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
        [ sectionTitle "Vote Rationale"
        , Helper.formContainer [ viewSummaryForm form.summary ]
        , Helper.formContainer [ viewStatementForm form.pdfAutogen form.rationaleStatement ]
        , Helper.formContainer [ viewPrecedentDiscussionForm form.precedentDiscussion ]
        , Helper.formContainer [ viewCounterArgumentForm form.counterArgumentDiscussion ]
        , Helper.formContainer [ viewConclusionForm form.conclusion ]
        , Helper.formContainer [ viewInternalVoteForm form.internalVote ]
        , viewReferencesForm form.references
        , Html.p [ HA.class "mt-4" ] [ Helper.viewButton "Confirm rationale" ValidateRationaleButtonClicked ]
        , viewError form.error
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


viewStatementForm : Bool -> MarkdownForm -> Html Msg
viewStatementForm hasAutoGen form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Rationale Statement" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Compulsory" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Fully describe your rationale, with your arguments in full details." ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit. Use markdown with heading level 2 (##) or higher." ]
            ]
        , viewPdfAutogenCheckbox hasAutoGen
        , Helper.viewTextarea form RationaleStatementChange
        ]


viewPdfAutogenCheckbox : Bool -> Html Msg
viewPdfAutogenCheckbox hasAutoGen =
    div
        [ HA.class "flex items-center mb-3" ]
        [ div
            [ HA.class "relative flex items-center" ]
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.id "autogen-checkbox"
                , HA.name "autogen-checkbox"
                , HA.checked hasAutoGen
                , onCheck TogglePdfAutogen
                , HA.class "h-4 w-4 cursor-pointer border-gray-300 rounded"
                , HA.style "accent-color" "#272727"
                ]
                []
            , Html.label
                [ HA.for "autogen-checkbox"
                , HA.class "ml-2 text-sm font-medium text-gray-700 cursor-pointer flex items-center"
                ]
                [ Html.span [ HA.class "mr-1" ] [ text "Auto-generate PDF and add it to the rationale" ]
                ]
            ]
        ]


viewPrecedentDiscussionForm : MarkdownForm -> Html Msg
viewPrecedentDiscussionForm form =
    div []
        [ Html.h4 [ HA.class "text-xl font-medium" ] [ text "Precedent Discussion" ]
        , div [ HA.class "mt-2 mb-4" ]
            [ Html.p [ HA.class "text-sm text-gray-600" ] [ text "Optional" ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "Discuss what you feel is relevant precedent." ]
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit and markdown is supported." ]
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
            , Html.p [ HA.class "text-sm text-gray-600" ] [ text "No size limit and markdown is supported." ]
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


viewCompletedRationale : Rationale -> Html Msg
viewCompletedRationale rationale =
    div []
        [ sectionTitle "Vote Rationale"
        , div [ HA.class "space-y-6" ]
            [ maybeViewSection viewSummary (Just rationale.summary)
            , maybeViewSection (viewMd "Rationale Statement") (Just rationale.rationaleStatement)
            , maybeViewSection (viewMd "Precedent Discussion") rationale.precedentDiscussion
            , maybeViewSection (viewMd "Counter Argument") rationale.counterArgumentDiscussion
            , maybeViewSection viewConclusion rationale.conclusion
            , let
                internalVote =
                    if rationale.internalVote == noInternalVote then
                        Nothing

                    else
                        Just rationale.internalVote
              in
              maybeViewSection viewInternalVote internalVote
            , let
                references =
                    if List.isEmpty rationale.references then
                        Nothing

                    else
                        Just rationale.references
              in
              maybeViewSection viewReferences references
            ]
        , Html.p [ HA.class "mt-6" ] [ Helper.viewButton "Edit rationale" EditRationaleButtonClicked ]
        ]



-- Helper to conditionally render a section if content exists


maybeViewSection : (a -> Html msg) -> Maybe a -> Html msg
maybeViewSection viewFn maybeValue =
    case maybeValue of
        Nothing ->
            text ""

        Just value ->
            Helper.formContainer
                [ div [ HA.class "bg-gray-50 p-4 rounded-md border", HA.style "border-color" "#e5e7eb" ]
                    [ viewFn value ]
                ]


viewSummary : String -> Html msg
viewSummary summary =
    div []
        [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "Summary" ]
        , Html.p [ HA.class "text-gray-800" ] [ text summary ]
        ]


viewMd : String -> String -> Html msg
viewMd section str =
    div []
        [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text section ]
        , case Md.parse str of
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
        ]


markdownRenderer : Md.Renderer (Html msg)
markdownRenderer =
    { defaultHtmlRenderer
        | heading = customHeadingRenderer
        , link =
            \link content ->
                Html.a
                    [ HA.href link.destination
                    , HA.target "_blank"
                    , HA.rel "noopener noreferrer"
                    , HA.class "text-blue-600 hover:text-blue-800 underline"
                    ]
                    content
    }


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


viewConclusion : String -> Html msg
viewConclusion conclusion =
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
            [ Html.h4 [ HA.class "text-xl font-bold mb-2" ] [ text "References" ]
            , Html.ul [ HA.class "space-y-2" ] (List.map viewRef references)
            ]


viewRef : Reference -> Html msg
viewRef ref =
    let
        uriDisplay =
            if String.startsWith "ipfs://" ref.uri then
                let
                    cid =
                        String.dropLeft 7 ref.uri

                    gatewayUrl =
                        "https://ipfs.io/ipfs/" ++ cid
                in
                Html.a
                    [ HA.href gatewayUrl
                    , HA.target "_blank"
                    , HA.rel "noopener noreferrer"
                    , HA.class "text-blue-600 hover:text-blue-800 underline"
                    ]
                    [ text ref.uri ]

            else
                text ref.uri
    in
    Html.li [ HA.class "py-2 border-b last:border-b-0", HA.style "border-color" "#e5e7eb" ]
        [ Html.div [ HA.class "flex flex-col space-y-1" ]
            [ Html.div []
                [ Html.strong [ HA.class "font-medium" ] [ text "Type: " ]
                , text (refTypeToString ref.type_)
                ]
            , Html.div []
                [ Html.strong [ HA.class "font-medium" ] [ text "Label: " ]
                , text ref.label
                ]
            , Html.div [ HA.class "flex" ]
                [ Html.strong [ HA.class "font-medium" ] [ text "URI: " ]
                , Html.div [ HA.class "break-all ml-2" ] [ uriDisplay ]
                ]
            ]
        ]



--
-- Rationale Signature Step
--


viewRationaleSignatureStep :
    ViewContext msg
    -> Step {} {} ActiveProposal
    -> Step RationaleForm Rationale Rationale
    -> Step RationaleSignatureForm {} RationaleSignature
    -> Html msg
viewRationaleSignatureStep ctx pickProposalStep rationaleCreationStep step =
    case ( pickProposalStep, rationaleCreationStep, step ) of
        ( _, Preparing _, _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please validate the rationale creation step first." ]
                ]

        ( _, Validating _ _, _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please validate the rationale creation step first." ]
                ]

        ( Done _ { id }, Done _ _, Preparing form ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.map ctx.wrapMsg <| viewRationaleSignatureForm ctx.jsonLdContexts id form
                , Html.p []
                    [ Helper.viewButton "Skip rationale signing" (ctx.wrapMsg SkipRationaleSignaturesButtonClicked)
                    , text " or "
                    , Helper.viewButton "Validate rationale signing" (ctx.wrapMsg ValidateRationaleSignaturesButtonClicked)
                    ]
                , viewError form.error
                ]

        ( _, Done _ _, Preparing _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Please pick a proposal first." ]
                ]

        ( _, Done _ _, Validating _ _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Signature" ]
                , Html.p [] [ text "Validating rationale author signatures ..." ]
                ]

        ( _, Done _ _, Done _ ratSig ) ->
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


viewRationaleSignatureForm : JsonLdContexts -> Gov.ActionId -> RationaleSignatureForm -> Html Msg
viewRationaleSignatureForm jsonLdContexts actionId ({ authors } as form) =
    let
        jsonRationale =
            (rationaleSignatureFromForm jsonLdContexts actionId { form | authors = [] }).signedJson
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
createJsonRationale : JsonLdContexts -> Gov.ActionId -> Rationale -> List AuthorWitness -> JE.Value
createJsonRationale jsonLdContexts actionId rationale authors =
    JE.object <|
        List.filterMap identity
            [ Just ( "@context", jsonLdContexts.ccCip136Context )
            , Just ( "hashAlgorithm", JE.string "blake2b-256" )
            , Just ( "body", encodeJsonLdRationale actionId rationale )
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


viewPermanentStorageStep :
    ViewContext msg
    -> Step RationaleSignatureForm {} RationaleSignature
    -> Step StorageForm {} StorageConfig
    -> Step { error : Maybe String } {} Storage
    -> Html msg
viewPermanentStorageStep ctx rationaleSigStep storageConfigStep step =
    case ( rationaleSigStep, storageConfigStep, step ) of
        ( Done _ _, Done _ _, Preparing form ) ->
            Html.map ctx.wrapMsg <|
                div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                    [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Storage" ]
                    , Html.p [] [ Helper.viewButton "Add to IPFS" PinJsonIpfsButtonClicked ]
                    , viewError form.error
                    ]

        ( Done _ _, Done _ _, Validating _ _ ) ->
            div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Storage" ]
                , Html.p [] [ text "Uploading rationale to IPFS server ..." ]
                ]

        ( Done _ r, Done _ _, Done _ storage ) ->
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
                div [ HA.style "padding-top" "8px" ]
                    [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Storage" ]
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
                [ Html.h4 [ HA.class "text-3xl font-medium my-4" ] [ text "Rationale Storage" ]
                , Html.p [] [ text "Please complete the storage config and rationale signature steps first." ]
                ]


viewHeader : Int -> ( String, String ) -> Html Msg
viewHeader n ( field, value ) =
    Helper.formContainer
        [ div [ HA.class "flex items-center" ]
            [ div [ HA.class "flex-1 flex gap-12" ]
                [ Helper.labeledField "Request Header Title"
                    (Helper.textFieldInline field (StorageHeaderFieldChange n))
                , Helper.labeledField "Request Header Value"
                    (Helper.textFieldInline value (StorageHeaderValueChange n))
                , Helper.viewButton "Delete" (DeleteHeaderButtonClicked n)
                ]
            ]
        ]



--
-- Tx Building Step
--


viewBuildTxStep : ViewContext msg -> InnerModel -> Html msg
viewBuildTxStep ctx model =
    div [ HA.style "padding-top" "8px", HA.style "padding-bottom" "8px" ]
        [ sectionTitle "Tx Building"
        , case ( allPrepSteps ctx model, model.buildTxStep ) of
            ( Err _, _ ) ->
                viewMissingStepsMessage ctx model

            ( Ok _, Preparing { error } ) ->
                viewVoteOptionsForm ctx error

            ( Ok _, Validating _ _ ) ->
                Helper.formContainer
                    [ Html.p [ HA.class "text-gray-600" ] [ text "Validating transaction information..." ] ]

            ( Ok _, Done _ { tx } ) ->
                viewBuiltTransaction ctx tx
        ]


viewMissingStepsMessage : ViewContext msg -> InnerModel -> Html msg
viewMissingStepsMessage ctx model =
    Helper.formContainer
        [ Html.p [ HA.class "text-gray-600 mb-4" ] [ text "Please complete the following steps before building the transaction:" ]
        , div []
            [ viewMissingStep "Voter identification" (isStepIncomplete model.voterStep)
            , viewMissingStep "Proposal selection" (isStepIncomplete model.pickProposalStep)
            , viewMissingStep "Rationale creation" (isStepIncomplete model.rationaleCreationStep)
            , viewMissingStep "Rationale storage" (isStepIncomplete model.permanentStorageStep)
            , viewMissingStep "Connect wallet" (ctx.loadedWallet == Nothing)
            , viewMissingStep "Protocol parameters" (ctx.costModels == Nothing)
            ]
        ]


viewMissingStep : String -> Bool -> Html msg
viewMissingStep stepName isMissing =
    if isMissing then
        Html.div [ HA.class " mb-2" ]
            [ Html.strong [] [ text "⚠️ Missing: " ]
            , text stepName
            ]

    else
        text ""


isStepIncomplete : Step a b c -> Bool
isStepIncomplete step =
    case step of
        Done _ _ ->
            False

        _ ->
            True


viewVoteOptionsForm : ViewContext msg -> Maybe String -> Html msg
viewVoteOptionsForm ctx error =
    Helper.formContainer
        [ Html.p [ HA.class "mb-4" ] [ text "Choose your vote:" ]
        , div [ HA.style "display" "flex", HA.style "align-items" "center" ]
            [ div [ HA.style "margin-right" "12px" ]
                [ Helper.viewButton "Vote YES" (ctx.wrapMsg (BuildTxButtonClicked Gov.VoteYes)) ]
            , div [ HA.style "margin-right" "12px" ]
                [ Helper.viewButton "Vote NO" (ctx.wrapMsg (BuildTxButtonClicked Gov.VoteNo)) ]
            , div []
                [ Helper.viewButton "Vote ABSTAIN" (ctx.wrapMsg (BuildTxButtonClicked Gov.VoteAbstain)) ]
            ]
        , viewError error
        ]


viewBuiltTransaction : ViewContext msg -> Transaction -> Html msg
viewBuiltTransaction ctx tx =
    Helper.formContainer
        [ Html.p [ HA.class "mb-2" ]
            [ text "Transaction generated successfully"
            , Html.span [ HA.style "color" "red" ] [ text " (₳ displayed as lovelaces)" ]
            , Html.span [] [ text ":" ]
            ]
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



--
-- Tx Signing Step
--


viewSignTxStep : ViewContext msg -> Step a b VoterWitness -> Step BuildTxPrep {} TxFinalized -> Html msg
viewSignTxStep ctx voterStep buildTxStep =
    case ( buildTxStep, voterStep ) of
        ( Done _ { tx, expectedSignatures }, Done _ voterWitness ) ->
            let
                -- Extract the main voter credential information
                voterId =
                    case voterWitness of
                        Cardano.WithCommitteeHotCred (Cardano.WithKey hotkey) ->
                            [ ( Bytes.toHex hotkey, "Committee hot key" ) ]

                        Cardano.WithCommitteeHotCred (Cardano.WithScript scriptHash witness) ->
                            ( Bytes.toHex scriptHash, "Committee governance script" )
                                :: extractMultisigKeys witness

                        Cardano.WithDrepCred (Cardano.WithKey credKey) ->
                            [ ( Bytes.toHex credKey, "DRep key" ) ]

                        Cardano.WithDrepCred (Cardano.WithScript scriptHash witness) ->
                            ( Bytes.toHex scriptHash, "DRep governance script" )
                                :: extractMultisigKeys witness

                        Cardano.WithPoolCred poolKey ->
                            [ ( Bytes.toHex poolKey, "SPO key" ) ]

                extractMultisigKeys : ScriptWitness -> List ( String, String )
                extractMultisigKeys witness =
                    case witness of
                        NativeWitness { expectedSigners } ->
                            List.map (\signer -> ( Bytes.toHex signer, "Multisig signer" )) expectedSigners

                        -- "let’s leave it as-is because we don’t handle them anyway for now"
                        PlutusWitness _ ->
                            []

                walletSpendingCredential =
                    case ctx.loadedWallet of
                        Just { changeAddress } ->
                            Address.extractPubKeyHash changeAddress
                                |> Maybe.map (\hash -> [ ( Bytes.toHex hash, "Wallet spending key" ) ])
                                |> Maybe.withDefault []

                        Nothing ->
                            []

                walletStakeCredential =
                    case ctx.loadedWallet of
                        Just { changeAddress } ->
                            Address.extractStakeKeyHash changeAddress
                                |> Maybe.map (\hash -> [ ( Bytes.toHex hash, "Wallet stake key" ) ])
                                |> Maybe.withDefault []

                        Nothing ->
                            []

                keyNames : Dict String String
                keyNames =
                    Dict.fromList (voterId ++ walletSpendingCredential ++ walletStakeCredential)
            in
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
                                        (let
                                            hashHex =
                                                Bytes.toHex hash
                                         in
                                         case Dict.get hashHex keyNames of
                                            Just keyName ->
                                                [ text <| keyName ++ ": " ++ Bytes.toHex hash ]

                                            Nothing ->
                                                [ text <| Bytes.toHex hash ]
                                        )
                                )
                                expectedSignatures
                            )
                        ]
                    , Html.p [ HA.class "text-gray-800 mb-4" ]
                        [ text "Click the button below to proceed to the signing page where you can finalize and submit your voting transaction." ]
                    , ctx.signingLink tx
                        (expectedSignatures
                            |> List.map
                                (\keyHash ->
                                    { keyHash = keyHash
                                    , keyName =
                                        Dict.get (Bytes.toHex keyHash) keyNames
                                            |> Maybe.withDefault "Key hash"
                                    }
                                )
                        )
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
