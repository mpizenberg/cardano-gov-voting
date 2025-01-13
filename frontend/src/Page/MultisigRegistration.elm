module Page.MultisigRegistration exposing (LoadedWallet, Model, Msg(..), RegisterTxSummary, UnregisterTxSummary, UpdateContext, ViewContext, initialModel, update, view)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CertificateIntent(..), CredentialWitness(..), ScriptWitness(..), SpendSource(..), TxIntent(..), WitnessSource(..))
import Cardano.Address as Address exposing (Address, Credential, CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov exposing (CostModels)
import Cardano.Script as Script exposing (NativeScript)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Uplc as Uplc
import Cardano.Utxo as Utxo exposing (Output, OutputReference)
import Cardano.Value
import Cbor.Encode
import Helper
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
    , registerTxSummary : Maybe RegisterTxSummary
    , unregisterTxSummary : Maybe UnregisterTxSummary
    , error : Maybe String
    }


type alias RegisterTxSummary =
    { txWithFakeWitnesses : Transaction
    , nativeScript : NativeScript
    , scriptHash : Bytes CredentialHash
    , scriptRefInput : Maybe OutputReference
    }


type alias UnregisterTxSummary =
    { txWithFakeWitnesses : Transaction
    , scriptHash : Bytes CredentialHash
    }


initialModel : Model
initialModel =
    { minCount = 1
    , hashes = []
    , register = True
    , createOutputRef = True
    , registerTxSummary = Nothing
    , unregisterTxSummary = Nothing
    , error = Nothing
    }



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = MinCountChange String
    | AddKeyButtonClicked
    | DeleteKeyButtonClicked Int
    | KeyHashChange Int String
    | ToggleRegister Bool
    | ToggleCreateOutputRef Bool
    | BuildRegistrationTxButtonClicked
    | BuildUnregistrationTxButtonClicked


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

        BuildRegistrationTxButtonClicked ->
            validateFormAndBuildRegister ctx model

        BuildUnregistrationTxButtonClicked ->
            validateFormAndBuildUnregister ctx model


validateFormAndBuildRegister : UpdateContext msg -> Model -> ( Model, Cmd msg )
validateFormAndBuildRegister ctx model =
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
                case buildRegisterTx w costModels credHashes model of
                    Err err ->
                        ( { model
                            | registerTxSummary = Nothing
                            , error = Just err
                          }
                        , Cmd.none
                        )

                    Ok ( tx, script, scriptHash ) ->
                        ( { model
                            | registerTxSummary =
                                Just
                                    { txWithFakeWitnesses = tx
                                    , nativeScript = script
                                    , scriptHash = scriptHash

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


validateFormAndBuildUnregister : UpdateContext msg -> Model -> ( Model, Cmd msg )
validateFormAndBuildUnregister ctx model =
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
                case buildUnregisterTx w costModels credHashes model of
                    Err err ->
                        ( { model
                            | unregisterTxSummary = Nothing
                            , error = Just err
                          }
                        , Cmd.none
                        )

                    Ok ( tx, _, scriptHash ) ->
                        ( { model
                            | unregisterTxSummary =
                                Just
                                    { txWithFakeWitnesses = tx
                                    , scriptHash = scriptHash
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


type alias MultisigConfig =
    { sortedCredentials : List (Bytes CredentialHash)
    , minCount : Int
    , nativeScript : NativeScript
    , scriptHash : Bytes CredentialHash
    , drepCred : Credential
    }


nativeMultisig : List (Bytes CredentialHash) -> Model -> MultisigConfig
nativeMultisig unsortedCreds model =
    let
        -- Sort the credentials to be deterministic, regardless of the keys orders
        creds =
            List.sortBy Bytes.toHex unsortedCreds

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
    in
    { sortedCredentials = creds
    , minCount = model.minCount
    , nativeScript = multisigNativeScript
    , scriptHash = scriptHash
    , drepCred = drepCred
    }


buildRegisterTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> Model -> Result String ( Transaction, NativeScript, Bytes CredentialHash )
buildRegisterTx w costModels unsortedCreds model =
    let
        { sortedCredentials, nativeScript, scriptHash, drepCred } =
            nativeMultisig unsortedCreds model

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
                                { script = WitnessValue nativeScript
                                , expectedSigners = sortedCredentials
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
            , referenceScript = Just (Script.Native nativeScript)
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
        |> Result.map (\tx -> ( tx, nativeScript, scriptHash ))


buildUnregisterTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> Model -> Result String ( Transaction, NativeScript, Bytes CredentialHash )
buildUnregisterTx w costModels unsortedCreds model =
    let
        { sortedCredentials, nativeScript, scriptHash } =
            nativeMultisig unsortedCreds model

        -- DRep deposit amount
        -- TODO: parameterize
        refundAmount =
            Natural.fromSafeInt 500000000

        -- Tx intent to unregister as a DRep
        unregistrationIntent =
            IssueCertificate <|
                UnregisterDrep
                    { drep =
                        WithScript scriptHash
                            (NativeWitness
                                { script = WitnessValue nativeScript
                                , expectedSigners = sortedCredentials
                                }
                            )
                    , refund = refundAmount
                    }
    in
    [ unregistrationIntent, SendTo w.changeAddress <| Cardano.Value.onlyLovelace refundAmount ]
        |> Cardano.finalizeAdvanced
            { govState = Cardano.emptyGovernanceState
            , localStateUtxos = w.utxos
            , coinSelectionAlgo = CoinSelection.largestFirst
            , evalScriptsCosts = Uplc.evalScriptsCosts Uplc.defaultVmConfig
            , costModels = costModels
            }
            (Cardano.AutoFee { paymentSource = w.changeAddress })
            []
        |> Result.mapError Debug.toString
        |> Result.map (\tx -> ( tx, nativeScript, scriptHash ))


locateScriptRef : Bytes CredentialHash -> Transaction -> OutputReference
locateScriptRef scriptHash tx =
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
            , text " and (2) avoid possible misrepresentations of the same multisig due to different CBOR encodings."
            ]
        , Html.hr [] []
        , Html.h3 [] [ text "Multisig Configuration" ]
        , Html.map ctx.wrapMsg <| viewMultisigConfigForm model
        , Html.hr [] []
        , Html.h3 [] [ text "DRep Registration" ]
        , Html.map ctx.wrapMsg <| viewRegisterForm model
        , case model.registerTxSummary of
            Nothing ->
                text ""

            Just summary ->
                let
                    txWithoutSignatures =
                        Transaction.updateSignatures (always Nothing) summary.txWithFakeWitnesses

                    -- The placeholder vkey witnesses (to compute fees) should start with the 28 bytes
                    -- of the expected public key hashes.
                    -- Except in the very unlikely case where the hash looks like ascii (char < 128)
                    -- which is a 1/2^28 probability.
                    expectedSignatures =
                        Maybe.withDefault [] summary.txWithFakeWitnesses.witnessSet.vkeywitness
                            |> List.map (\{ vkey } -> Bytes.toHex vkey |> String.slice 0 (2 * 28))
                in
                div []
                    [ Html.p [] [ text "Tx to sign:" ]
                    , Html.pre [] [ text <| prettyTx txWithoutSignatures ]
                    , viewImportantSummaryTx summary
                    , ctx.signingLink txWithoutSignatures expectedSignatures [ text "Sign & submit the Tx on the signing page" ]
                    ]
        , Html.hr [] []
        , Html.h3 [] [ text "DRep Unregistration" ]
        , Html.map ctx.wrapMsg <|
            Html.p [] [ button [ onClick BuildUnregistrationTxButtonClicked ] [ text "Build Unegistration Tx" ] ]
        , case model.unregisterTxSummary of
            Nothing ->
                text ""

            Just summary ->
                let
                    txWithoutSignatures =
                        Transaction.updateSignatures (always Nothing) summary.txWithFakeWitnesses

                    -- The placeholder vkey witnesses (to compute fees) should start with the 28 bytes
                    -- of the expected public key hashes.
                    -- Except in the very unlikely case where the hash looks like ascii (char < 128)
                    -- which is a 1/2^28 probability.
                    expectedSignatures =
                        Maybe.withDefault [] summary.txWithFakeWitnesses.witnessSet.vkeywitness
                            |> List.map (\{ vkey } -> Bytes.toHex vkey |> String.slice 0 (2 * 28))
                in
                div []
                    [ Html.p [] [ text "Tx to sign:" ]
                    , Html.pre [] [ text <| prettyTx txWithoutSignatures ]
                    , ctx.signingLink txWithoutSignatures expectedSignatures [ text "Sign & submit the Tx on the signing page" ]
                    ]
        , Html.hr [] []
        , Html.h3 [] [ text "Summary" ]
        , case model.error of
            Nothing ->
                text ""

            Just err ->
                Html.p []
                    [ text "Error:"
                    , Html.pre [] [ text err ]
                    ]
        ]


viewMultisigConfigForm : Model -> Html Msg
viewMultisigConfigForm { minCount, hashes } =
    Html.div []
        [ Helper.viewNumberInput "Minimum number of required signatures: " minCount MinCountChange
        , Html.p []
            [ text "List of public key hashes: "
            , button [ onClick AddKeyButtonClicked ] [ text "Add a key" ]
            ]
        , div [] (List.indexedMap viewOneKeyForm hashes)
        ]


viewRegisterForm : Model -> Html Msg
viewRegisterForm { register, createOutputRef } =
    Html.div []
        [ Html.p []
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
        , Html.p [] [ button [ onClick BuildRegistrationTxButtonClicked ] [ text "Build Registration Tx" ] ]
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


viewImportantSummaryTx : RegisterTxSummary -> Html msg
viewImportantSummaryTx { nativeScript, scriptHash, scriptRefInput } =
    -- Display:
    --  * the script hash
    --  * TODO: the reference input for the script, this needs compute the Tx ID
    let
        scriptBytes =
            Cbor.Encode.encode (Script.encodeNativeScript nativeScript)
                |> Bytes.fromBytes

        scriptRef =
            case scriptRefInput of
                Nothing ->
                    "none"

                Just { transactionId, outputIndex } ->
                    Bytes.toHex transactionId ++ "#" ++ String.fromInt outputIndex
    in
    div []
        [ Html.p [] [ text "This is the important info to note as it will be useful to reuse the multisig!" ]
        , Html.p [] [ text <| "DRep ID: TODO" ]
        , Html.p [] [ text <| "Script hash: " ++ Bytes.toHex scriptHash ]
        , Html.p [] [ text <| "Script bytes: " ++ Bytes.toHex scriptBytes ]
        , Html.p [] [ text <| "Script reference input: " ++ scriptRef ]
        ]
