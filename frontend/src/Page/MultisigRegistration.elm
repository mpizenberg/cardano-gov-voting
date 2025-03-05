module Page.MultisigRegistration exposing (LoadedWallet, Model, Msg(..), RegisterTxSummary, UnregisterTxSummary, UpdateContext, ViewContext, initialModel, update, view)

{-| This module handles the registration and unregistration of multisig DReps (Delegated Representatives)
in the Cardano governance system.

A multisig DRep is represented by a native script that requires M-of-N signatures from a set of public keys
to take governance actions. This module helps users:

1.  Configure the multisig parameters (required signature count and public key hashes)
2.  Register the multisig as a DRep by paying the registration deposit
3.  Optionally create a script reference output to optimize transaction fees
4.  Unregister the multisig DRep and reclaim the deposit

Key design considerations:

  - Uses native scripts for maximum compatibility with wallets
  - Enforces deterministic script creation by sorting credential hashes
  - Optimizes for M-of-N scenarios by using ScriptAny/ScriptAll when possible
  - Supports script references to reduce fees for large multisigs (5+ keys)

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (CertificateIntent(..), CredentialWitness(..), ScriptWitness(..), SpendSource(..), TxFinalized, TxIntent(..), WitnessSource(..))
import Cardano.Address as Address exposing (Address, Credential(..), CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.CoinSelection as CoinSelection
import Cardano.Gov as Gov exposing (CostModels)
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


{-| Core configuration for a multisig DRep:

  - minCount: Minimum number of required signatures (M in M-of-N)
  - hashes: List of public key hashes that can sign (N total keys)
  - register: Whether to register as DRep in this transaction
  - createOutputRef: Whether to create a reference script output

-}
type alias Model =
    { minCount : Int
    , hashes : List String
    , register : Bool
    , createOutputRef : Bool
    , registerTxSummary : Maybe RegisterTxSummary
    , unregisterTxSummary : Maybe UnregisterTxSummary
    , error : Maybe String
    }


{-| Important information produced when building a registration transaction:

  - Expected signatures needed from the multisig participants
  - The native script and its hash for future reference
  - Location of the script reference output if created
  - The DRep ID that will represent this multisig in governance

-}
type alias RegisterTxSummary =
    { tx : Transaction
    , expectedSignatures : List (Bytes CredentialHash)
    , nativeScript : NativeScript
    , scriptHash : Bytes CredentialHash
    , scriptRefInput : Maybe OutputReference
    , drepId : Gov.Id
    }


type alias UnregisterTxSummary =
    { tx : Transaction
    , expectedSignatures : List (Bytes CredentialHash)
    , scriptHash : Bytes CredentialHash
    }


initialModel : Model
initialModel =
    { minCount = 1
    , hashes = []
    , register = True
    , createOutputRef = False
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

                    Ok ( { tx, expectedSignatures }, script, scriptHash ) ->
                        ( { model
                            | registerTxSummary =
                                Just
                                    { tx = tx
                                    , expectedSignatures = expectedSignatures
                                    , nativeScript = script
                                    , scriptHash = scriptHash
                                    , scriptRefInput =
                                        Transaction.locateScriptWithHash scriptHash tx.body.outputs
                                            |> Maybe.map (\( index, _ ) -> OutputReference (Transaction.computeTxId tx) index)
                                    , drepId = Gov.DrepId (ScriptHash scriptHash)
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

                    Ok ( { tx, expectedSignatures }, _, scriptHash ) ->
                        ( { model
                            | unregisterTxSummary =
                                Just
                                    { tx = tx
                                    , expectedSignatures = expectedSignatures
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


{-| Creates the native script configuration from a list of credentials and minimum signature count.
Key aspects:

  - Sorts credentials for deterministic script creation
  - Optimizes script type based on required signatures:
      - Uses ScriptAny for 1-of-N
      - Uses ScriptAll for N-of-N
      - Uses ScriptNofK for M-of-N

-}
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


buildRegisterTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> Model -> Result String ( TxFinalized, NativeScript, Bytes CredentialHash )
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
            , referenceScript = Just (Script.refFromScript <| Script.Native nativeScript)
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


buildUnregisterTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> Model -> Result String ( TxFinalized, NativeScript, Bytes CredentialHash )
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



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , signingLink : Transaction -> List (Bytes CredentialHash) -> List (Html msg) -> Html msg
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
     div [ HA.class "container mx-auto " ]
        [ Html.h2 [ HA.class "text-2xl font-bold py-4" ] [ text "Registering a multisig DRep" ]
        , Html.p []
            [ text "This page aims to facilitate registration of multisig DReps."
            , text " The goal is to build a Tx that can both"
            , text " (1) register the multisig as a DRep,"
            , text " and (2) save the multisig script into a reference output."
            , text " For multisigs with 5 keys or more, it’s starting to be interesting to use a script reference"
            , text " instead of an inline script to pay less fees."
            ]
        , Html.h3 [ HA.class "text-xl font-bold py-4" ] [ text "Multisig Configuration" ]
        , Html.map ctx.wrapMsg <| viewMultisigConfigForm model
        , Html.h3 [ HA.class "text-xl font-bold py-4" ] [ text "DRep Registration" ]
        , Html.map ctx.wrapMsg <| viewRegisterForm model
        , case model.registerTxSummary of
            Nothing ->
                text ""

            Just summary ->
                div []
                    [ Html.p [] [ text <| "Tx ID: " ++ (Bytes.toHex <| Transaction.computeTxId summary.tx) ]
                    , Html.p [] [ text "Tx details: (₳ amounts are in lovelaces)" ]
                    , Html.pre [] [ text <| prettyTx summary.tx ]
                    , viewImportantSummaryTx summary
                    , ctx.signingLink summary.tx summary.expectedSignatures [ text "Sign & submit the Tx on the signing page" ]
                    ]   
        , Html.h3 [ HA.class "text-xl font-bold py-4" ] [ text "DRep Unregistration" ]
        , Html.map ctx.wrapMsg <|
            Html.p [] [ Helper.viewButton "Build Unregistration Tx" BuildUnregistrationTxButtonClicked ]
        , case model.unregisterTxSummary of
            Nothing ->
                text ""

            Just summary ->
                div []
                    [ Html.p [] [ text <| "Tx ID: " ++ (Bytes.toHex <| Transaction.computeTxId summary.tx) ]
                    , Html.p [] [ text "Tx details: (₳ amounts are in lovelaces)" ]
                    , Html.pre [] [ text <| prettyTx summary.tx ]
                    , ctx.signingLink summary.tx summary.expectedSignatures [ text "Sign & submit the Tx on the signing page" ]
                    ]
        , case model.error of
            Nothing ->
                text ""

            Just err ->
                div []
                    [ Html.h3 [ HA.class "" ]
                        [ text "Error:"
                        , Html.pre [] [ text err ]
                        ]
                    ]
        ]


viewMultisigConfigForm : Model -> Html Msg
viewMultisigConfigForm { minCount, hashes } =
    Html.div []
        [ Html.p [ HA.class "py-4 flex items-center" ]
            [ text "Minimum number of required signatures: "
            , Helper.viewNumberInputInline minCount MinCountChange
            ]
        , Html.p [ HA.class "py-4" ]
            [ text "List of public key hashes: "
            , Helper.viewButton "Add a key" AddKeyButtonClicked
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
        , Html.p [HA.class "mt-4"] [ Helper.viewButton "Build Registration Tx" BuildRegistrationTxButtonClicked ]
        ]


viewOneKeyForm : Int -> String -> Html Msg
viewOneKeyForm n hash =
    Html.p [ HA.class "flex items-center py-2" ]
        [ Html.text "Key hash: "
        , Helper.textFieldInline "" hash (KeyHashChange n)
        , Helper.viewButton "Delete" (DeleteKeyButtonClicked n)
        ]

viewImportantSummaryTx : RegisterTxSummary -> Html msg
viewImportantSummaryTx { nativeScript, scriptHash, scriptRefInput, drepId } =
    -- Display:
    --  * the script hash
    --  * the reference input for the script, this needs compute the Tx ID
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
        [ Html.p [] [ Html.strong [] [ text "Important info to note as it will be useful to reuse the multisig:" ] ]
        , Html.p [] [ text <| "DRep ID: " ++ Gov.idToBech32 drepId ]
        , Html.p [] [ text <| "Native script hash: " ++ Bytes.toHex scriptHash ]
        , Html.p [] [ text <| "Native script bytes: " ++ Bytes.toHex scriptBytes ]
        , Html.p [] [ text <| "Script reference UTxO: " ++ scriptRef ]
        ]