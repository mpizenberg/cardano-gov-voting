module Page.Preparation exposing (ActiveProposal, Model, Msg, init, update, view)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CredentialWitness(..), ScriptWitness(..))
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.Gov exposing (ActionId)
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo as Utxo exposing (Output)
import Dict exposing (Dict)
import Html exposing (Html, button, div, text, wbr)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


type alias Model =
    { voterStep : Step VoterPreparationForm {} VoterIdentified
    , pickProposalStep : Step {} {} ActiveProposal
    , rationaleCreationStep : Step RationaleForm {} Rationale
    , rationaleSignatureStep : Step (Dict String (Maybe AuthorWitness)) {} (Dict String AuthorWitness)
    , permanentStorageStep : Step StoragePrep {} Storage
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
    , rationaleSignatureStep = Preparing Dict.empty
    , permanentStorageStep = Preparing {}
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
    { authors : List AuthorForm
    , summary : MarkdownForm
    , rationaleStatement : MarkdownForm
    , precedentDiscussion : MarkdownForm
    , counterArgumentDiscussion : MarkdownForm
    , conclusion : MarkdownForm
    , internalVote : InternalVote
    , references : ReferencesForm
    }


type alias AuthorForm =
    {}


type alias MarkdownForm =
    String


type alias InternalVote =
    { constitutional : Int
    , unconstitutional : Int
    , abstain : Int
    , didNotVote : Int
    }


type alias ReferencesForm =
    {}


initRationaleForm : RationaleForm
initRationaleForm =
    { authors = []
    , summary = ""
    , rationaleStatement = ""
    , precedentDiscussion = ""
    , counterArgumentDiscussion = ""
    , conclusion = ""
    , internalVote =
        { constitutional = 0
        , unconstitutional = 0
        , abstain = 0
        , didNotVote = 0
        }
    , references = {}
    }


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



-- Storage Step


type alias StoragePrep =
    {}


type alias Storage =
    {}



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
      -- Fee Provider Step
    | FeeProviderUpdated FeeProviderForm
    | ValidateFeeProviderFormButtonClicked
    | ReceivedFeeProviderUtxos FeeProvider


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , loadedWallet : Maybe LoadedWallet
    , feeProviderAskUtxosCmd : Cmd msg
    }


type alias LoadedWallet =
    { wallet : Cip30.Wallet
    , changeAddress : Address
    , utxos : Utxo.RefDict Output
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        --
        -- Voter Step
        --
        VoterTypeSelected voterType ->
            ( updateVoterForm (\form -> { form | voterType = voterType }) model
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

        ScriptVoter { scriptHash, utxoRef } ->
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
        , viewPermanentStorageStep ctx model.permanentStorageStep
        , Html.hr [] []
        , viewFeeProviderStep ctx model.feeProviderStep
        , Html.hr [] []
        , viewBuildTxStep ctx model.buildTxStep
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
                , Html.map ctx.wrapMsg <| viewVoterTypeSelector form.voterType
                , Html.map ctx.wrapMsg <| viewVoterCredentialsForm form.voterCred
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
            , viewCredTypeOption (ScriptVoter { scriptHash = "", utxoRef = "" }) "(WIP) Script Voter" (not isStakeKeyVoter)
            ]
        , case credForm of
            StakeKeyVoter key ->
                div []
                    [ Html.label [] [ text "Stake key hash (or stake address)" ]
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

            WithScript cred (NativeWitness witness) ->
                Html.p [] [ text "TODO: display native script witness" ]

            WithScript cred (PlutusWitness witness) ->
                Html.p [] [ text "TODO: display Plutus script witness" ]
        ]



--
-- Proposal Selection Step
--


viewProposalSelectionStep : ViewContext msg -> Model -> Html msg
viewProposalSelectionStep ctx model =
    text "TODO viewProposalSelectionStep"



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
                    , viewAuthorsForm form.authors
                    , viewSummaryForm form.summary
                    , viewStatementForm form.rationaleStatement
                    , viewPrecedentDiscussionForm form.precedentDiscussion
                    , viewCounterArgumentForm form.counterArgumentDiscussion
                    , viewConclusionForm form.conclusion
                    , viewInternalVoteForm form.internalVote
                    , viewReferencesForm form.references
                    ]

            Validating _ _ ->
                div []
                    [ Html.h3 [] [ text "Vote Rationale" ]
                    , Html.p [] [ text "validating rationale data ..." ]
                    ]

            Done rationale ->
                div []
                    [ Html.h3 [] [ text "Vote Rationale" ]
                    , Html.p [] [ text "TODO: display rationale" ]
                    ]


viewAuthorsForm : List AuthorForm -> Html Msg
viewAuthorsForm authors =
    div []
        [ Html.h4 [] [ text "Authors" ]
        , text "TODO: view authors form"
        ]


viewSummaryForm : MarkdownForm -> Html Msg
viewSummaryForm form =
    div []
        [ Html.h4 [] [ text "Summary" ]
        , div []
            [ Html.input
                [ HA.type_ "text"
                , HA.value form
                , Html.Events.onInput RationaleSummaryChange
                ]
                []
            ]
        ]


viewStatementForm : MarkdownForm -> Html Msg
viewStatementForm form =
    div []
        [ Html.h4 [] [ text "Rationale Statement" ]
        , div []
            [ Html.input
                [ HA.type_ "text"
                , HA.value form
                , Html.Events.onInput RationaleStatementChange
                ]
                []
            ]
        ]


viewPrecedentDiscussionForm : MarkdownForm -> Html Msg
viewPrecedentDiscussionForm form =
    div []
        [ Html.h4 [] [ text "Precedent Discussion" ]
        , div []
            [ Html.input
                [ HA.type_ "text"
                , HA.value form
                , Html.Events.onInput PrecedentDiscussionChange
                ]
                []
            ]
        ]


viewCounterArgumentForm : MarkdownForm -> Html Msg
viewCounterArgumentForm form =
    div []
        [ Html.h4 [] [ text "Counter Argument Discussion" ]
        , div []
            [ Html.input
                [ HA.type_ "text"
                , HA.value form
                , Html.Events.onInput CounterArgumentChange
                ]
                []
            ]
        ]


viewConclusionForm : MarkdownForm -> Html Msg
viewConclusionForm form =
    div []
        [ Html.h4 [] [ text "Conclusion" ]
        , div []
            [ Html.input
                [ HA.type_ "text"
                , HA.value form
                , Html.Events.onInput ConclusionChange
                ]
                []
            ]
        ]


viewInternalVoteForm : InternalVote -> Html Msg
viewInternalVoteForm { constitutional, unconstitutional, abstain, didNotVote } =
    div []
        [ Html.h4 [] [ text "Internal Vote" ]
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


viewReferencesForm : ReferencesForm -> Html Msg
viewReferencesForm _ =
    div []
        [ Html.h4 [] [ text "References" ]
        , text "TODO: viewReferencesForm"
        ]



--
-- Storage Step
--


viewPermanentStorageStep : ViewContext msg -> Step StoragePrep {} Storage -> Html msg
viewPermanentStorageStep ctx step =
    text "TODO viewPermanentStorageStep"



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

        Done feeProvider ->
            div []
                [ Html.h3 [] [ text "Fee Provider" ]
                , Html.p [] [ text "TODO: display address and utxos" ]
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
viewBuildTxStep ctx step =
    text "TODO viewBuildTxStep"
