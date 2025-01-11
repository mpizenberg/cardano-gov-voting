module Page.MultisigRegistration exposing (Model, Msg(..), UpdateContext, ViewContext, initialModel, update, view)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CertificateIntent(..), CredentialWitness(..), ScriptWitness(..), SpendSource(..), TxIntent(..), WitnessSource(..), dummyBytes)
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov as Gov exposing (CostModels)
import Cardano.Script as Script
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference, TransactionId)
import Cardano.Value
import Dict exposing (Dict)
import Helper exposing (shortenedHex)
import Html exposing (Html, button, div, text)
import Html.Attributes as HA
import Html.Events exposing (onCheck, onClick)
import List.Extra
import Natural



-- ###################################################################
-- MODEL
-- ###################################################################


type alias Model =
    { minCount : Int
    , hashes : List String
    , register : Bool
    , createOutputRef : Bool
    , txWithFakeWitnesses : Maybe Transaction
    , summary : Maybe MultisigSummary
    , error : Maybe String
    }


type alias MultisigSummary =
    { scriptHash : Bytes CredentialHash
    , scriptRefInput : Maybe OutputReference
    }


initialModel : Model
initialModel =
    { minCount = 1
    , hashes = []
    , register = True
    , createOutputRef = True
    , txWithFakeWitnesses = Nothing
    , summary = Nothing
    , error = Nothing
    }



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | MinCountChange String
    | AddKeyButtonClicked
    | DeleteKeyButtonClicked Int
    | KeyHashChange Int String
    | ToggleRegister Bool
    | ToggleCreateOutputRef Bool
    | BuildTxButtonClicked


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe LoadedWallet
    , costModels : Maybe CostModels
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

        MinCountChange countAsString ->
            case String.toInt countAsString of
                Just count ->
                    ( { model | minCount = count }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        AddKeyButtonClicked ->
            ( { model | hashes = "" :: model.hashes }
            , Cmd.none
            )

        DeleteKeyButtonClicked n ->
            ( { model | hashes = List.Extra.removeAt n model.hashes }
            , Cmd.none
            )

        KeyHashChange n hash ->
            ( { model | hashes = List.Extra.updateAt n (\_ -> hash) model.hashes }
            , Cmd.none
            )

        ToggleRegister register ->
            ( { model | register = register }
            , Cmd.none
            )

        ToggleCreateOutputRef create ->
            ( { model | createOutputRef = create }
            , Cmd.none
            )

        BuildTxButtonClicked ->
            validateFormAndBuildAndSubmitTx ctx model


validateFormAndBuildAndSubmitTx : UpdateContext msg -> Model -> ( Model, Cmd msg )
validateFormAndBuildAndSubmitTx ctx model =
    let
        keyCount =
            List.length model.hashes
    in
    if model.minCount < 0 then
        ( { model | error = Just "The minimum number of required signatures must be >= 0" }, Cmd.none )

    else if model.minCount > keyCount then
        ( { model | error = Just <| "The minimum number of required signatures must be <= to the number of keys in the multisig: " ++ String.fromInt keyCount }, Cmd.none )

    else
        case ( validateKeyHashes model.hashes [], ctx.wallet, ctx.costModels ) of
            ( Ok credHashes, Just w, Just costModels ) ->
                case buildMultisigTx w costModels credHashes model of
                    Err err ->
                        ( { model
                            | txWithFakeWitnesses = Nothing
                            , summary = Nothing
                            , error = Just err
                          }
                        , Cmd.none
                        )

                    Ok ( tx, scriptHash ) ->
                        ( { model
                            | txWithFakeWitnesses = Just tx
                            , summary =
                                Just
                                    { scriptHash = scriptHash

                                    -- , scriptRefInput = locateScriptRef scriptHash tx.body.outputs
                                    , scriptRefInput = Nothing
                                    }
                            , error = Nothing
                          }
                        , Cmd.none
                        )

            ( Err err, _, _ ) ->
                ( { model | error = Just err }, Cmd.none )

            ( Ok _, Nothing, _ ) ->
                ( { model | error = Just "You need to connect a wallet to pay for the fees" }, Cmd.none )

            ( Ok _, _, Nothing ) ->
                ( { model | error = Just "Cost models are missing, something went wrong sorry" }, Cmd.none )


validateKeyHashes : List String -> List (Bytes CredentialHash) -> Result String (List (Bytes CredentialHash))
validateKeyHashes hashes accum =
    case hashes of
        [] ->
            Ok accum

        str :: otherHashes ->
            -- Can only be a credential hash directly if 28 bytes
            if String.length str == 56 then
                case Bytes.fromHex str of
                    Nothing ->
                        Err <| "Invalid key hash: " ++ str

                    Just hash ->
                        validateKeyHashes otherHashes (hash :: accum)

            else
                Err <| "A key hash must have 56 hex characters (28 bytes) and this one has " ++ String.fromInt (String.length str)


buildMultisigTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> Model -> Result String ( Transaction, Bytes CredentialHash )
buildMultisigTx w costModels creds model =
    let
        credCount =
            List.length creds

        -- Create a native multisig script from the credentials and minimum count
        multisigNativeScript =
            if model.minCount == 1 then
                Script.ScriptAny <| List.map Script.ScriptPubkey creds

            else if model.minCount == credCount then
                Script.ScriptAll <| List.map Script.ScriptPubkey creds

            else
                Script.ScriptNofK model.minCount <| List.map Script.ScriptPubkey creds

        -- Compute the script hash
        scriptHash =
            Script.hash (Script.Native multisigNativeScript)

        -- Create DRep credential from the script hash
        drepCred =
            Address.ScriptHash scriptHash

        -- DRep deposit amount
        -- TODO: parameterize
        depositAmount =
            Natural.fromSafeInt 500000000

        -- Tx intent to register as a DRep
        registrationIntent =
            IssueCertificate <|
                RegisterDrep
                    { drep =
                        WithScript scriptHash
                            (NativeWitness
                                { script = WitnessValue multisigNativeScript
                                , expectedSigners = creds
                                }
                            )
                    , deposit = depositAmount

                    -- TODO: Improve with registration form
                    , info = Nothing
                    }

        -- Create an output with the script for future reference
        overpayedOutputWithScript =
            { address =
                Address.Shelley
                    { networkId = Address.extractNetworkId w.changeAddress |> Maybe.withDefault Testnet
                    , paymentCredential = drepCred
                    , stakeCredential = Nothing
                    }

            -- Temporary 2 Ada output, but we optimize it right after
            , amount = Cardano.Value.onlyLovelace <| Natural.fromSafeInt 2000000
            , datumOption = Nothing
            , referenceScript = Just (Script.Native multisigNativeScript)
            }

        -- Output with the multisig native script, with the minimum amount of Ada
        outputWithScript =
            { overpayedOutputWithScript
                | amount = Cardano.Value.onlyLovelace <| Utxo.minAda overpayedOutputWithScript
            }

        txIntents =
            case ( model.register, model.createOutputRef ) of
                ( False, False ) ->
                    Err "You need to at least register or create an output with the script ref"

                ( True, False ) ->
                    Ok
                        [ Spend <| FromWallet w.changeAddress <| Cardano.Value.onlyLovelace depositAmount
                        , registrationIntent
                        ]

                ( False, True ) ->
                    Ok
                        [ Spend <| FromWallet w.changeAddress outputWithScript.amount
                        , SendToOutput outputWithScript
                        ]

                ( True, True ) ->
                    Ok
                        [ Spend <| FromWallet w.changeAddress <| Cardano.Value.onlyLovelace depositAmount
                        , registrationIntent
                        , Spend <| FromWallet w.changeAddress outputWithScript.amount
                        , SendToOutput outputWithScript
                        ]
    in
    txIntents
        |> Result.andThen
            (Cardano.finalizeAdvanced
                { govState = Cardano.emptyGovernanceState
                , localStateUtxos = w.utxos
                , coinSelectionAlgo = CoinSelection.largestFirst
                , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
                , costModels = costModels
                }
                (Cardano.AutoFee { paymentSource = w.changeAddress })
                []
                >> Result.mapError Debug.toString
            )
        |> Result.map (\tx -> ( tx, scriptHash ))


locateScriptRef : Bytes CredentialHash -> Transaction -> OutputReference
locateScriptRef scriptHash tx =
    let
        outputs =
            tx.body.outputs
    in
    Debug.todo "compute TxId, find output index"



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , signingLink : Transaction -> List String -> List (Html msg) -> Html msg
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div []
        [ Html.h2 [] [ text "Registering a multisig DRep" ]
        , Html.p []
            [ text "This page aims to facilitate registration of multisig DReps."
            , text " The goal is to build a Tx that both"
            , text " (1) registers the multisig as a DRep,"
            , text " and (2) saves the multisig script into a reference output."
            ]
        , Html.p []
            [ text "Saving the multisig in a reference output serves two purposes,"
            , text " (1) paying less fees on subsequent transactions,"
            , text " and (2) avoid possible multiple representations of the same multisig due to different CBOR encodings."
            ]
        , Html.map ctx.wrapMsg <| viewMultisigForm model
        , Html.hr [] []
        , case ( model.txWithFakeWitnesses, model.summary ) of
            ( Nothing, _ ) ->
                Html.p [] [ text "Tx to sign: waiting for you to build it first" ]

            ( Just tx, Nothing ) ->
                Html.p [] [ text "Something went wrong. The Tx and its summary should both be something or nothing!" ]

            ( Just tx, Just summary ) ->
                let
                    txWithoutSignatures =
                        Transaction.updateSignatures (always Nothing) tx

                    -- The placeholder vkey witnesses (to compute fees) should start with the 28 bytes
                    -- of the expected public key hashes.
                    -- Except in the very unlikely case where the hash looks like ascii (char < 128)
                    -- which is a 1/2^28 probability.
                    expectedSignatures =
                        Maybe.withDefault [] tx.witnessSet.vkeywitness
                            |> List.map (\{ vkey } -> Bytes.toHex vkey |> String.slice 0 (2 * 28))
                in
                div []
                    [ Html.p [] [ text "Tx to sign:" ]
                    , Html.pre [] [ text <| prettyTx txWithoutSignatures ]
                    , viewImportantSummaryTx summary
                    , ctx.signingLink txWithoutSignatures expectedSignatures [ text "Sign & submit the Tx on the signing page" ]
                    ]
        , case model.error of
            Nothing ->
                text ""

            Just err ->
                Html.p []
                    [ text "Error:"
                    , Html.pre [] [ text err ]
                    ]
        ]


viewMultisigForm : Model -> Html Msg
viewMultisigForm { minCount, hashes, register, createOutputRef } =
    Html.div []
        [ Helper.viewNumberInput "Minimum number of required signatures: " minCount MinCountChange
        , Html.p []
            [ text "List of public key hashes: "
            , button [ onClick AddKeyButtonClicked ] [ text "Add a key" ]
            ]
        , div [] (List.indexedMap viewOneKeyForm hashes)
        , Html.p []
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.id "register"
                , HA.name "register"
                , HA.checked register
                , onCheck ToggleRegister
                ]
                []
            , Html.label [ HA.for "register" ] [ text " Register as DRep" ]
            ]
        , Html.p []
            [ Html.input
                [ HA.type_ "checkbox"
                , HA.id "refOutput"
                , HA.name "refOutput"
                , HA.checked createOutputRef
                , onCheck ToggleCreateOutputRef
                ]
                []
            , Html.label [ HA.for "refOutput" ] [ text " Create an output reference" ]
            ]
        , Html.p [] [ button [ onClick BuildTxButtonClicked ] [ text "Build Tx" ] ]
        ]


viewOneKeyForm : Int -> String -> Html Msg
viewOneKeyForm n hash =
    Html.p []
        [ button [ onClick (DeleteKeyButtonClicked n) ] [ text "Delete" ]
        , Html.text " key hash: "
        , Html.input
            [ HA.type_ "text"
            , HA.value hash
            , Html.Events.onInput (KeyHashChange n)
            ]
            []
        ]


viewImportantSummaryTx : MultisigSummary -> Html msg
viewImportantSummaryTx { scriptHash, scriptRefInput } =
    -- Display:
    --  * the script hash
    --  * TODO: the reference input for the script, this needs compute the Tx ID
    let
        scriptRef =
            case scriptRefInput of
                Nothing ->
                    "none"

                Just { transactionId, outputIndex } ->
                    Bytes.toHex transactionId ++ "#" ++ String.fromInt outputIndex
    in
    div []
        [ Html.h4 [] [ text "Multisig Summary" ]
        , Html.p [] [ text "This is the important info to note as it will be useful to reuse the multisig!" ]
        , Html.p [] [ text <| "Script hash: " ++ Bytes.toHex scriptHash ]
        , Html.p [] [ text <| "Script reference input: " ++ scriptRef ]
        ]
