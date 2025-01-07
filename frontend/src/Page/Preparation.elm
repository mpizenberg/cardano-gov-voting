module Page.Preparation exposing (ActiveProposal, JsonLdContexts, Model, Msg, init, update, view)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CredentialWitness(..), ScriptWitness(..))
import Cardano.Address as Address exposing (Address(..), Credential(..), CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.Gov as Gov exposing (ActionId)
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output)
import Cbor.Encode
import Dict exposing (Dict)
import Dict.Any
import File exposing (File)
import File.Select
import Helper exposing (prettyAddr)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Encode as JE
import List.Extra
import Markdown.Block as Md
import Markdown.Parser as Md
import Markdown.Renderer as Md
import RemoteData exposing (WebData)
import Set exposing (Set)
import Task
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


type alias Model =
    { voterStep : Step VoterPreparationForm {} VoterIdentified
    , pickProposalStep : Step {} {} ActiveProposal
    , rationaleCreationStep : Step RationaleForm {} Rationale
    , rationaleSignatureStep : Step RationaleSignatureForm {} RationaleSignature
    , permanentStorageStep : Step StorageForm {} Storage
    , feeProviderStep : Step FeeProviderForm FeeProviderTemp FeeProvider
    , buildTxStep : Step {} {} Transaction
    }


type Step prep validating done
    = Preparing prep
    | Validating prep validating
    | Done done


init : Model
init =
    { voterStep = Preparing initVoterForm
    , pickProposalStep = Preparing {}
    , rationaleCreationStep = Preparing initRationaleForm
    , rationaleSignatureStep = Preparing initRationaleSignatureForm
    , permanentStorageStep = Preparing initStorageForm
    , feeProviderStep = Preparing (ConnectedWalletFeeProvider { error = Nothing })
    , buildTxStep = Preparing {}
    }



-- Voter Step


type alias VoterPreparationForm =
    { voterType : VoterType
    , voterCred : VoterCredForm
    , error : Maybe String
    }


type VoterType
    = CcVoter
    | DrepVoter
    | SpoVoter


type VoterCredForm
    = StakeKeyVoter String
    | ScriptVoter { scriptHash : String, utxoRef : String }


type alias VoterIdentified =
    { voterType : VoterType
    , voterCred : CredentialWitness
    }


initVoterForm : VoterPreparationForm
initVoterForm =
    { voterType = DrepVoter
    , voterCred = StakeKeyVoter ""
    , error = Nothing
    }



-- Pick Proposal Step


type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    , metadata : WebData ProposalMetadata
    }


type alias ProposalMetadata =
    { title : String
    , abstract : String
    , rawJson : String
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
    , hash : String
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



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
      -- Voter Step
    | VoterTypeSelected VoterType
    | VoterCredentialUpdated VoterCredForm
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


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , proposals : WebData (Dict String ActiveProposal)
    , loadedWallet : Maybe LoadedWallet
    , feeProviderAskUtxosCmd : Cmd msg
    , jsonLdContexts : JsonLdContexts
    }


type alias JsonLdContexts =
    { ccCip136Context : JE.Value }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , changeAddress : Address
    , utxos : Utxo.RefDict Output
    }


type IpfsAnswer
    = IpfsError String
    | IpfsAddSuccessful IpfsFile


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        --
        -- Voter Step
        --
        VoterTypeSelected voterType ->
            -- If SPO is selected, make sure the cred type is a stake key
            ( updateVoterForm
                (\form ->
                    { form
                        | voterType = voterType
                        , voterCred =
                            case ( voterType, form.voterCred ) of
                                ( SpoVoter, ScriptVoter _ ) ->
                                    StakeKeyVoter ""

                                _ ->
                                    form.voterCred
                    }
                )
                model
            , Cmd.none
            )

        VoterCredentialUpdated voterCredForm ->
            ( updateVoterForm (\form -> { form | voterCred = voterCredForm }) model
            , Cmd.none
            )

        ValidateVoterFormButtonClicked ->
            case model.voterStep of
                Preparing form ->
                    ( { model | voterStep = confirmVoter form }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeVoterButtonClicked ->
            case model.voterStep of
                Done { voterType, voterCred } ->
                    let
                        voterCredForm =
                            case voterCred of
                                WithKey credHash ->
                                    StakeKeyVoter (Bytes.toHex credHash)

                                WithScript scriptHash _ ->
                                    ScriptVoter { scriptHash = Bytes.toHex scriptHash, utxoRef = "TODO" }
                    in
                    ( { model | voterStep = Preparing { voterType = voterType, voterCred = voterCredForm, error = Nothing } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        --
        -- Pick Proposal Step
        --
        PickProposalButtonClicked actionId ->
            case ( model.pickProposalStep, ctx.proposals ) of
                ( Preparing _, RemoteData.Success proposalsDict ) ->
                    case Dict.get actionId proposalsDict of
                        Just prop ->
                            ( { model | pickProposalStep = Done prop }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeProposalButtonClicked ->
            ( { model | pickProposalStep = Preparing {} }
            , Cmd.none
            )

        --
        -- Rationale Step
        --
        RationaleSummaryChange summary ->
            ( updateRationaleForm (\form -> { form | summary = summary }) model
            , Cmd.none
            )

        RationaleStatementChange statement ->
            ( updateRationaleForm (\form -> { form | rationaleStatement = statement }) model
            , Cmd.none
            )

        PrecedentDiscussionChange precedentDiscussion ->
            ( updateRationaleForm (\form -> { form | precedentDiscussion = precedentDiscussion }) model
            , Cmd.none
            )

        CounterArgumentChange argument ->
            ( updateRationaleForm (\form -> { form | counterArgumentDiscussion = argument }) model
            , Cmd.none
            )

        ConclusionChange conclusion ->
            ( updateRationaleForm (\form -> { form | conclusion = conclusion }) model
            , Cmd.none
            )

        InternalConstitutionalVoteChange constitutionalStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | constitutional = n }) constitutionalStr model
            , Cmd.none
            )

        InternalUnconstitutionalVoteChange unconstitutionalStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | unconstitutional = n }) unconstitutionalStr model
            , Cmd.none
            )

        InternalAbstainVoteChange abstainStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | abstain = n }) abstainStr model
            , Cmd.none
            )

        InternalDidNotVoteChange didNotVoteStr ->
            ( updateRationaleInternalVoteForm (\n internal -> { internal | didNotVote = n }) didNotVoteStr model
            , Cmd.none
            )

        AddRefButtonClicked ->
            ( updateRationaleForm (\form -> { form | references = initRefForm :: form.references }) model
            , Cmd.none
            )

        DeleteRefButtonClicked n ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.removeAt n form.references }) model
            , Cmd.none
            )

        ReferenceLabelChange n label ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | label = label }) form.references }) model
            , Cmd.none
            )

        ReferenceUriChange n uri ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | uri = uri }) form.references }) model
            , Cmd.none
            )

        ReferenceTypeChange n refTypeStr ->
            ( updateRationaleForm (\form -> { form | references = List.Extra.updateAt n (\ref -> { ref | type_ = refTypeFromString refTypeStr }) form.references }) model
            , Cmd.none
            )

        ValidateRationaleButtonClicked ->
            case validateRationaleForm model.rationaleCreationStep of
                Done newRationale ->
                    ( { model
                        | rationaleCreationStep = Done newRationale
                        , rationaleSignatureStep = Preparing <| resetRationaleSignatures newRationale model.rationaleSignatureStep
                      }
                    , Cmd.none
                    )

                prepOrValidating ->
                    ( { model | rationaleCreationStep = prepOrValidating }, Cmd.none )

        EditRationaleButtonClicked ->
            ( { model | rationaleCreationStep = editRationale model.rationaleCreationStep }
            , Cmd.none
            )

        --
        -- Rationale Signature Step
        --
        AddAuthorButtonClicked ->
            ( updateAuthorsForm (\authors -> initAuthorForm :: authors) model
            , Cmd.none
            )

        DeleteAuthorButtonClicked n ->
            ( updateAuthorsForm (\authors -> List.Extra.removeAt n authors) model
            , Cmd.none
            )

        AuthorNameChange n name ->
            ( updateAuthorsForm (\authors -> List.Extra.updateAt n (\author -> { author | name = name }) authors) model
            , Cmd.none
            )

        LoadJsonSignatureButtonClicked n authorName ->
            ( model
            , Cmd.map ctx.wrapMsg <|
                File.Select.file [ "application/json" ] <|
                    FileSelectedForJsonSignature n authorName
            )

        FileSelectedForJsonSignature n authorName file ->
            ( model
            , Task.attempt (handleJsonSignatureFileRead n authorName) (File.toString file)
                |> Cmd.map ctx.wrapMsg
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
            )

        SkipRationaleSignaturesButtonClicked ->
            ( { model | rationaleSignatureStep = skipRationaleSignature model.rationaleSignatureStep }
            , Cmd.none
            )

        ValidateRationaleSignaturesButtonClicked ->
            validateRationaleSignature model

        ChangeAuthorsButtonClicked ->
            ( { model | rationaleSignatureStep = Preparing <| rationaleSignatureToForm model.rationaleSignatureStep }
            , Cmd.none
            )

        --
        -- Permanent Storage Step
        --
        IpfsServerChange ipfsServer ->
            ( updateStorageForm (\form -> { form | ipfsServer = ipfsServer }) model
            , Cmd.none
            )

        AddHeaderButtonClicked ->
            ( updateStorageForm (\form -> { form | headers = ( "", "" ) :: form.headers }) model
            , Cmd.none
            )

        DeleteHeaderButtonClicked n ->
            ( updateStorageForm (\form -> { form | headers = List.Extra.removeAt n form.headers }) model
            , Cmd.none
            )

        StorageHeaderFieldChange n field ->
            ( updateStorageForm (\form -> { form | headers = List.Extra.updateAt n (\( _, v ) -> ( field, v )) form.headers }) model
            , Cmd.none
            )

        StorageHeaderValueChange n value ->
            ( updateStorageForm (\form -> { form | headers = List.Extra.updateAt n (\( f, _ ) -> ( f, value )) form.headers }) model
            , Cmd.none
            )

        PinJsonIpfsButtonClicked ->
            case model.permanentStorageStep of
                Preparing form ->
                    validateIpfsFormAndSendPinRequest ctx form model

                Validating _ _ ->
                    ( model, Cmd.none )

                Done _ ->
                    ( model, Cmd.none )

        GotIpfsAnswer (Err httpError) ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none )

                Validating form _ ->
                    ( { model | permanentStorageStep = Preparing { form | error = Just <| Debug.toString httpError } }
                    , Cmd.none
                    )

                Done _ ->
                    ( model, Cmd.none )

        GotIpfsAnswer (Ok ipfsAnswer) ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none )

                Validating form _ ->
                    handleIpfsAnswer model form ipfsAnswer

                Done _ ->
                    ( model, Cmd.none )

        AddOtherStorageButtonCLicked ->
            case model.permanentStorageStep of
                Preparing _ ->
                    ( model, Cmd.none )

                Validating _ _ ->
                    ( model, Cmd.none )

                Done storage ->
                    ( { model | permanentStorageStep = Preparing storage.config }
                    , Cmd.none
                    )

        --
        -- Fee Provider Step
        --
        FeeProviderUpdated feeProviderForm ->
            ( updateFeeProviderForm feeProviderForm model
            , Cmd.none
            )

        ValidateFeeProviderFormButtonClicked ->
            case model.feeProviderStep of
                Preparing form ->
                    case validateFeeProviderForm ctx.loadedWallet form of
                        (Validating _ _) as validating ->
                            ( { model | feeProviderStep = validating }
                            , ctx.feeProviderAskUtxosCmd
                            )

                        validated ->
                            ( { model | feeProviderStep = validated }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ReceivedFeeProviderUtxos feeProvider ->
            case model.feeProviderStep of
                Validating _ _ ->
                    ( { model | feeProviderStep = Done feeProvider }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeFeeProviderButtonClicked ->
            case model.feeProviderStep of
                Done _ ->
                    ( { model | feeProviderStep = Preparing (ConnectedWalletFeeProvider { error = Nothing }) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- Voter Step


updateVoterForm : (VoterPreparationForm -> VoterPreparationForm) -> Model -> Model
updateVoterForm f ({ voterStep } as model) =
    case voterStep of
        Preparing form ->
            { model | voterStep = Preparing (f form) }

        _ ->
            model


confirmVoter : VoterPreparationForm -> Step VoterPreparationForm {} VoterIdentified
confirmVoter form =
    case validateVoterCredForm form.voterCred of
        Ok voterCred ->
            Done
                { voterType = form.voterType
                , voterCred = voterCred
                }

        Err error ->
            Preparing { form | error = Just error }


validateVoterCredForm : VoterCredForm -> Result String CredentialWitness
validateVoterCredForm voterCredForm =
    case voterCredForm of
        StakeKeyVoter str ->
            stakeKeyHashFromStr str
                |> Result.map WithKey

        ScriptVoter _ ->
            -- Result.map2 (\hash utxo -> WithScript hash <| )
            -- (scriptHashFromStr scriptHash)
            -- (utxoRefFromStr utxoRef)
            Debug.todo "validateVoterCredForm"


stakeKeyHashFromStr : String -> Result String (Bytes CredentialHash)
stakeKeyHashFromStr str =
    -- Try to extract the stake key hash from a string that can either be:
    --  * garbage
    --  * directly a valid stake key hash in hex
    --  * a stake key address in hex
    --  * a stake key address in bech32
    if String.length str == 56 then
        -- Can only be a credential hash directly if 28 bytes
        Bytes.fromHex str
            |> Result.fromMaybe ("Invalid Hex of credential hash: " ++ str)

    else
        Address.fromString str
            |> Result.fromMaybe ("Invalid credential hash or stake address: " ++ str)
            |> Result.andThen
                (\address ->
                    case address of
                        Address.Reward stakeAddress ->
                            case stakeAddress.stakeCredential of
                                VKeyHash cred ->
                                    Ok cred

                                ScriptHash _ ->
                                    Err "This is a script address, not a Key address"

                        _ ->
                            Err "This is a full address, please use a stake (reward) address instead"
                )



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
                    Done (rationaleFromForm form)

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

        Done rationale ->
            Preparing
                { summary = rationale.summary
                , rationaleStatement = rationale.rationaleStatement
                , precedentDiscussion = Maybe.withDefault "" rationale.precedentDiscussion
                , counterArgumentDiscussion = Maybe.withDefault "" rationale.counterArgumentDiscussion
                , conclusion = Maybe.withDefault "" rationale.conclusion
                , internalVote = rationale.internalVote
                , references = rationale.references
                , error = Nothing
                }



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

        Done { authors } ->
            newRatSig authors


encodeJsonLd : Rationale -> JE.Value
encodeJsonLd rationale =
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


skipRationaleSignature : Step RationaleSignatureForm {} RationaleSignature -> Step RationaleSignatureForm {} RationaleSignature
skipRationaleSignature step =
    case step of
        Preparing ({ authors, rationale } as form) ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    Done
                        { authors = List.map (\a -> { a | signature = Nothing }) authors
                        , rationale = rationale
                        }

        Validating ({ authors, rationale } as form) _ ->
            case findDuplicate (List.map .name authors) of
                Just dup ->
                    Preparing { form | error = Just <| "There is a duplicate name in the authors list: " ++ dup }

                Nothing ->
                    Done
                        { authors = List.map (\a -> { a | signature = Nothing }) authors
                        , rationale = rationale
                        }

        Done signatures ->
            Done signatures


validateRationaleSignature : Model -> ( Model, Cmd msg )
validateRationaleSignature model =
    case model.rationaleSignatureStep of
        Preparing ({ authors } as form) ->
            case validateAuthorsForm authors of
                Err error ->
                    ( { model | rationaleSignatureStep = Preparing { form | error = Just error } }
                    , Cmd.none
                    )

                Ok _ ->
                    -- TODO: change to Validating instead, and emit a command to check signatures
                    ( { model | rationaleSignatureStep = Done <| rationaleSignatureFromForm form }
                    , Cmd.none
                    )

        Validating _ _ ->
            ( model, Cmd.none )

        Done _ ->
            ( model, Cmd.none )


rationaleSignatureFromForm : RationaleSignatureForm -> RationaleSignature
rationaleSignatureFromForm form =
    { authors = form.authors
    , rationale = form.rationale
    }


rationaleSignatureToForm : Step RationaleSignatureForm {} RationaleSignature -> RationaleSignatureForm
rationaleSignatureToForm step =
    case step of
        Preparing form ->
            form

        Validating form _ ->
            form

        Done s ->
            { authors = s.authors
            , rationale = s.rationale
            , error = Nothing
            }


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
            Set.fromList [ "ed25519" ]

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

        Done _ ->
            model


validateIpfsFormAndSendPinRequest : UpdateContext msg -> StorageForm -> Model -> ( Model, Cmd msg )
validateIpfsFormAndSendPinRequest ctx form model =
    case ( model.rationaleSignatureStep, validateIpfsForm form ) of
        ( Done ratSig, Ok _ ) ->
            ( { model | permanentStorageStep = Validating form {} }
            , Cmd.map ctx.wrapMsg <| sendPinRequest ctx.jsonLdContexts form ratSig
            )

        ( Done _, Err error ) ->
            ( { model | permanentStorageStep = Preparing { form | error = Just error } }
            , Cmd.none
            )

        _ ->
            ( { model | permanentStorageStep = Preparing { form | error = Just "Validate the rationale signature step first." } }
            , Cmd.none
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
        nonEmptyHeadersResult =
            if List.any (\( f, _ ) -> String.isEmpty f) form.headers then
                Err "Empty header fields are forbidden."

            else
                Ok ()
    in
    ipfsServerUrlSeemsLegit
        |> Result.andThen (\_ -> nonEmptyHeadersResult)


sendPinRequest : JsonLdContexts -> StorageForm -> RationaleSignature -> Cmd Msg
sendPinRequest jsonLdContexts storageForm ratSig =
    let
        encodedRationale =
            createJsonRationale jsonLdContexts ratSig.rationale ratSig.authors

        encodedBody =
            JE.object
                [ ( "ipfsServer", JE.string storageForm.ipfsServer )
                , ( "headers", JE.list (\( f, v ) -> JE.list JE.string [ f, v ]) storageForm.headers )
                , ( "fileName", JE.string "rationale-signed.json" )
                , ( "jsonContent", JE.string <| JE.encode 0 encodedRationale )
                ]

        responseToIpfsAnswer : Http.Response String -> Result String IpfsAnswer
        responseToIpfsAnswer response =
            case response of
                Http.GoodStatus_ _ body ->
                    JD.decodeString ipfsAnswerDecoder body
                        |> Result.mapError JD.errorToString

                Http.BadStatus_ meta body ->
                    case JD.decodeString ipfsAnswerDecoder body of
                        Ok answer ->
                            Ok answer

                        Err _ ->
                            Err <| "Bad status (" ++ String.fromInt meta.statusCode ++ "): " ++ meta.statusText

                Http.NetworkError_ ->
                    Err "Network error. Maybe you lost your connection, or some other network error occured."

                Http.Timeout_ ->
                    Err "The Pin request timed out."

                Http.BadUrl_ str ->
                    Err <| "Incorrect URL: " ++ str
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "accept" "application/json" ]
        , url = "/ipfs-pin/json"
        , body = Http.jsonBody encodedBody
        , expect = Http.expectStringResponse GotIpfsAnswer responseToIpfsAnswer
        , timeout = Nothing
        , tracker = Nothing
        }


ipfsAnswerDecoder : JD.Decoder IpfsAnswer
ipfsAnswerDecoder =
    JD.oneOf
        -- Error
        [ JD.map3 (\err msg code -> IpfsError <| String.fromInt code ++ " (" ++ err ++ "): " ++ msg)
            (JD.field "error" JD.string)
            (JD.field "message" JD.string)
            (JD.field "status_code" JD.int)
        , JD.map IpfsError
            (JD.field "detail" JD.string)
        , JD.map (\json -> IpfsError <| JE.encode 2 json)
            (JD.field "detail" JD.value)

        -- Blockfrost format
        , JD.map3 (\name hash size -> IpfsAddSuccessful <| IpfsFile name hash size)
            (JD.field "name" JD.string)
            (JD.field "ipfs_hash" JD.string)
            (JD.field "size" JD.string)

        -- CF format
        , JD.map3 (\name hash size -> IpfsAddSuccessful <| IpfsFile name hash size)
            (JD.field "Name" JD.string)
            (JD.field "Hash" JD.string)
            (JD.field "Size" JD.string)
        ]


handleIpfsAnswer : Model -> StorageForm -> IpfsAnswer -> ( Model, Cmd msg )
handleIpfsAnswer model form ipfsAnswer =
    case ipfsAnswer of
        IpfsError error ->
            ( { model | permanentStorageStep = Preparing { form | error = Just error } }
            , Cmd.none
            )

        IpfsAddSuccessful file ->
            ( { model | permanentStorageStep = Done { config = form, jsonFile = file } }
            , Cmd.none
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
            Done { address = changeAddress, utxos = utxos }

        ( _, ExternalFeeProvider { endpoint } ) ->
            case Url.fromString endpoint of
                Just _ ->
                    Validating feeProviderForm { address = Nothing, utxos = Nothing }

                Nothing ->
                    Preparing (ExternalFeeProvider { endpoint = endpoint, error = Just <| "The endpoint does not look like a valid URL: " ++ endpoint })



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , walletChangeAddress : Maybe Address
    , proposals : WebData (Dict String ActiveProposal)
    , jsonLdContexts : JsonLdContexts
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
        , viewBuildTxStep ctx model.buildTxStep
        , Html.hr [] []
        , Html.p [] [ text "Built with <3 by the CF, using elm-cardano" ]
        ]



--
-- Voter Identification Step
--


viewVoterIdentificationStep : ViewContext msg -> Step VoterPreparationForm {} VoterIdentified -> Html msg
viewVoterIdentificationStep ctx step =
    case step of
        Preparing form ->
            div []
                [ Html.h3 [] [ text "Voter Identification" ]
                , Html.map ctx.wrapMsg <| viewVoterForm ctx.walletChangeAddress form
                , Html.p [] [ button [ onClick <| ctx.wrapMsg ValidateVoterFormButtonClicked ] [ text "Confirm Voter" ] ]
                , case form.error of
                    Just error ->
                        Html.p [] [ Html.pre [] [ text error ] ]

                    Nothing ->
                        text ""
                ]

        Validating _ _ ->
            div []
                [ Html.h3 [] [ text "Voter Identification" ]
                , Html.p [] [ text "validating voter information ..." ]
                ]

        Done voter ->
            div []
                [ Html.h3 [] [ text "Voter Identified" ]
                , Html.map ctx.wrapMsg <| viewIdentifiedVoter voter
                ]


viewVoterForm : Maybe Address -> VoterPreparationForm -> Html Msg
viewVoterForm walletChangeAddress { voterType, voterCred } =
    div []
        [ Html.p []
            [ viewVoterTypeOption CcVoter "Constitutional Committee" (voterType == CcVoter)
            , viewVoterTypeOption DrepVoter "DRep" (voterType == DrepVoter)
            , viewVoterTypeOption SpoVoter "SPO" (voterType == SpoVoter)
            ]
        , div []
            [ Html.h4 [] [ text "Voter Credentials" ]
            , case ( voterType, voterCred ) of
                ( SpoVoter, StakeKeyVoter key ) ->
                    div []
                        [ textField "Stake key hash (or stake address)" key (\s -> VoterCredentialUpdated (StakeKeyVoter s))
                        ]

                ( _, StakeKeyVoter key ) ->
                    div []
                        [ div []
                            [ viewCredTypeOption (StakeKeyVoter "") "Stake Key Voter" True
                            , textField " -- Stake key hash (or stake address)" key (\s -> VoterCredentialUpdated (StakeKeyVoter s))
                            , case Maybe.andThen Address.extractPubKeyHash walletChangeAddress of
                                Just cred ->
                                    button [ onClick <| VoterCredentialUpdated (StakeKeyVoter <| Bytes.toHex cred) ] [ text "<- use wallet stake key" ]

                                Nothing ->
                                    text ""
                            ]
                        , div [] [ viewCredTypeOption (ScriptVoter { scriptHash = "", utxoRef = "" }) "(WIP) Script Voter" False ]
                        ]

                ( _, ScriptVoter { scriptHash, utxoRef } ) ->
                    div []
                        [ div [] [ viewCredTypeOption (StakeKeyVoter "") "Stake Key Voter" False ]
                        , div [] [ viewCredTypeOption (ScriptVoter { scriptHash = "", utxoRef = "" }) "(WIP) Script Voter" True ]
                        , Html.p []
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
    Html.span []
        [ Html.input
            [ HA.type_ "radio"
            , HA.name "cred-type"
            , HA.checked isSelected
            , onClick (VoterCredentialUpdated voterCredType)
            ]
            []
        , Html.label [] [ text label ]
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


viewIdentifiedVoter : VoterIdentified -> Html Msg
viewIdentifiedVoter { voterType, voterCred } =
    let
        voterTypeText =
            case voterType of
                CcVoter ->
                    "Constitutional Committee Voter"

                DrepVoter ->
                    "DRep Voter"

                SpoVoter ->
                    "SPO Voter"
    in
    div []
        [ Html.p [] [ text voterTypeText ]
        , case voterCred of
            WithKey cred ->
                Html.p [] [ text <| "Using key for credential hash: " ++ Bytes.toHex cred ]

            WithScript _ (NativeWitness _) ->
                Html.p [] [ text "TODO: display native script witness" ]

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

        Done { id, actionType, metadata } ->
            div []
                [ Html.h3 [] [ text "Pick a Proposal" ]
                , Html.p []
                    [ text "Picked: "
                    , cardanoScanLink id
                    , text <| ", type: " ++ actionType
                    , text ", title: "
                    , text <|
                        case metadata of
                            RemoteData.NotAsked ->
                                "not loading"

                            RemoteData.Loading ->
                                "loading ..."

                            RemoteData.Failure error ->
                                "ERROR: " ++ Debug.toString error

                            RemoteData.Success meta ->
                                meta.title
                    ]
                , Html.p [] [ button [ onClick <| ctx.wrapMsg ChangeProposalButtonClicked ] [ text "Change Proposal" ] ]
                ]


viewActiveProposal : ActiveProposal -> Html Msg
viewActiveProposal { id, actionType, metadata } =
    Html.p []
        [ button [ onClick (PickProposalButtonClicked <| Gov.actionIdToString id) ] [ text "Pick this proposal" ]
        , text " "
        , cardanoScanLink id
        , text <| ", type: " ++ actionType
        , text ", title: "
        , text <|
            case metadata of
                RemoteData.NotAsked ->
                    "not loading"

                RemoteData.Loading ->
                    "loading ..."

                RemoteData.Failure error ->
                    "ERROR: " ++ Debug.toString error

                RemoteData.Success meta ->
                    meta.title
        ]


cardanoScanLink : ActionId -> Html msg
cardanoScanLink id =
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
                    , case form.error of
                        Nothing ->
                            text ""

                        Just error ->
                            Html.p []
                                [ text "Error:"
                                , Html.pre [] [ text error ]
                                ]
                    ]

            Validating _ _ ->
                div []
                    [ Html.h3 [] [ text "Vote Rationale" ]
                    , Html.p [] [ text "validating rationale data ..." ]
                    , Html.p [] [ button [ onClick EditRationaleButtonClicked ] [ text "Edit rationale" ] ]
                    ]

            Done rationale ->
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
        , viewNumberInput "Constitutional: " constitutional InternalConstitutionalVoteChange
        , viewNumberInput "Unconstitutional: " unconstitutional InternalUnconstitutionalVoteChange
        , viewNumberInput "Abstain: " abstain InternalAbstainVoteChange
        , viewNumberInput "Did not vote: " didNotVote InternalDidNotVoteChange
        ]


viewNumberInput : String -> Int -> (String -> Msg) -> Html Msg
viewNumberInput label n msgOnInput =
    div []
        [ text label
        , Html.input
            [ HA.type_ "number"
            , HA.value (String.fromInt n)
            , Html.Events.onInput msgOnInput
            ]
            []
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

        ( Done _, Preparing form ) ->
            div []
                [ Html.h3 [] [ text "Rationale Signature" ]
                , Html.map ctx.wrapMsg <| viewRationaleSignatureForm ctx.jsonLdContexts form
                , Html.p []
                    [ button [ onClick <| ctx.wrapMsg SkipRationaleSignaturesButtonClicked ] [ text "Skip rationale signing" ]
                    , text " or "
                    , button [ onClick <| ctx.wrapMsg ValidateRationaleSignaturesButtonClicked ] [ text "Validate rationale signing" ]
                    ]
                , case form.error of
                    Nothing ->
                        text ""

                    Just error ->
                        Html.p []
                            [ text "Error:"
                            , Html.pre [] [ text error ]
                            ]
                ]

        ( Done _, Validating _ _ ) ->
            div []
                [ Html.h3 [] [ text "Rationale Signature" ]
                , Html.p [] [ text "Validating rationale author signatures ..." ]
                ]

        ( Done _, Done ratSig ) ->
            let
                jsonRationale =
                    createJsonRationale ctx.jsonLdContexts ratSig.rationale ratSig.authors
                        |> JE.encode 0

                downloadButton =
                    Html.a
                        [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode jsonRationale
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
viewRationaleSignatureForm jsonLdContexts { authors, rationale } =
    let
        jsonRationale =
            createJsonRationale jsonLdContexts rationale []
                |> JE.encode 0
    in
    div []
        [ Html.p [] [ text "Here is the JSON-LD file generated from your rationale inputs." ]
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
            , Just ( "body", encodeJsonLd rationale )
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
        ( Done _, Preparing form ) ->
            Html.map ctx.wrapMsg <|
                div []
                    [ Html.h3 [] [ text "Permanent Storage" ]
                    , Html.p []
                        [ text "Only the hash of your rationale is stored on Cardano,"
                        , text " so it’s recommended to also store the actual JSON file containing the rationale in a permanent storage solution."
                        , text " Here we provide an easy way to store it on IPFS."
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
                    , case form.error of
                        Nothing ->
                            text ""

                        Just error ->
                            Html.p []
                                [ text "Error:"
                                , Html.pre [] [ text error ]
                                ]
                    ]

        ( Done _, Validating _ _ ) ->
            div []
                [ Html.h3 [] [ text "Permanent Storage" ]
                , Html.p [] [ text "Uploading rationale to IPFS server ..." ]
                ]

        ( Done _, Done storage ) ->
            Html.map ctx.wrapMsg <|
                div []
                    [ Html.h3 [] [ text "Permanent Storage" ]
                    , let
                        link =
                            "https://ipfs.io/ipfs/" ++ storage.jsonFile.hash
                      in
                      Html.p []
                        [ text "File uploaded: "
                        , Html.a [ HA.href link, HA.download storage.jsonFile.name, HA.target "_blank" ] [ text link ]
                        ]
                    , Html.ul []
                        [ Html.li [] [ text <| "name: " ++ storage.jsonFile.name ]
                        , Html.li [] [ text <| "hash: " ++ storage.jsonFile.hash ]
                        , Html.li [] [ text <| "size: " ++ storage.jsonFile.size ++ " Bytes" ]
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

        Done { address, utxos } ->
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


viewBuildTxStep : ViewContext msg -> Step {} {} Transaction -> Html msg
viewBuildTxStep _ _ =
    text "TODO viewBuildTxStep"
