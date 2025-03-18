module Page.MultisigRegistration exposing (LoadedWallet, Model, Msg, UpdateContext, ViewContext, initialModel, update, view)

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
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onCheck)
import List.Extra
import Natural



-- ###################################################################
-- MODEL
-- ###################################################################


type Model
    = Model InnerModel


{-| Core configuration for a multisig DRep:

  - minCount: Minimum number of required signatures (M in M-of-N)
  - hashes: List of public key hashes that can sign (N total keys)
  - register: Whether to register as DRep in this transaction
  - createOutputRef: Whether to create a reference script output

-}
type alias InnerModel =
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
    Model
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
update ctx msg (Model model) =
    innerUpdate ctx msg model
        |> Tuple.mapFirst Model


innerUpdate : UpdateContext msg -> Msg -> InnerModel -> ( InnerModel, Cmd msg )
innerUpdate ctx msg model =
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


validateFormAndBuildRegister : UpdateContext msg -> InnerModel -> ( InnerModel, Cmd msg )
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


validateFormAndBuildUnregister : UpdateContext msg -> InnerModel -> ( InnerModel, Cmd msg )
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
nativeMultisig : List (Bytes CredentialHash) -> InnerModel -> MultisigConfig
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


buildRegisterTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> InnerModel -> Result String ( TxFinalized, NativeScript, Bytes CredentialHash )
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
                        [ Spend <|
                            FromWallet
                                { address = w.changeAddress
                                , value = Cardano.Value.onlyLovelace depositAmount
                                , guaranteedUtxos = []
                                }
                        , registrationIntent
                        ]

                ( False, True ) ->
                    Ok
                        [ Spend <|
                            FromWallet
                                { address = w.changeAddress
                                , value = Cardano.Value.onlyLovelace depositAmount
                                , guaranteedUtxos = []
                                }
                        , SendToOutput outputWithScript
                        ]

                ( True, True ) ->
                    Ok
                        [ Spend <|
                            FromWallet
                                { address = w.changeAddress
                                , value = Cardano.Value.onlyLovelace depositAmount
                                , guaranteedUtxos = []
                                }
                        , registrationIntent
                        , Spend <|
                            FromWallet
                                { address = w.changeAddress
                                , value = outputWithScript.amount
                                , guaranteedUtxos = []
                                }
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


buildUnregisterTx : LoadedWallet -> CostModels -> List (Bytes CredentialHash) -> InnerModel -> Result String ( TxFinalized, NativeScript, Bytes CredentialHash )
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
view ctx (Model model) =
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
                    [ text "Registering a multisig DRep" ]
                , Html.p
                    [ HA.style "font-size" "1.25rem"
                    , HA.style "line-height" "1.6"
                    , HA.style "max-width" "640px"
                    , HA.style "margin-bottom" "2rem"
                    ]
                    [ text "This page facilitates registration of multisig DReps. "
                    , text "You can build a transaction that either: "
                    , text "(1) registers the multisig as a DRep, "
                    , text "(2) saves the multisig script into a reference output, "
                    , text "or both. For multisigs with 5+ keys, using a script reference instead of an inline script can reduce fees."
                    ]
                , Html.div
                    [ HA.style "display" "flex"
                    , HA.style "gap" "1rem"
                    , HA.style "flex-wrap" "wrap"
                    ]
                    [ Html.map ctx.wrapMsg (Helper.viewButton "Configure Multisig" AddKeyButtonClicked) ]
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
            [ Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Multisig Configuration" ]
            , Html.map ctx.wrapMsg <| viewMultisigConfigForm model
            , Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "DRep Registration" ]
            , Html.map ctx.wrapMsg <| viewRegisterForm model
            , case model.registerTxSummary of
                Nothing ->
                    text ""

                Just summary ->
                    Helper.formContainer
                        [ Html.p [ HA.class "mb-2" ]
                            [ Html.strong [ HA.class "font-medium" ] [ text "Transaction ID: " ]
                            , Html.span [ HA.class "font-mono" ] [ text <| Bytes.toHex <| Transaction.computeTxId summary.tx ]
                            ]
                        , Html.p [ HA.class "mb-2" ] [ text "Transaction generated successfully", Html.span [ HA.style "color" "red" ] [ text " (₳ displayed as lovelaces):" ] ]
                        , Html.pre
                            [ HA.class "bg-gray-50 p-4 rounded-md border overflow-auto mt-2 text-sm whitespace-pre-wrap break-words"
                            , HA.style "border-color" "#C6C6C6"
                            , HA.style "max-height" "300px"
                            , HA.style "word-break" "break-all"
                            ]
                            [ text <| prettyTx summary.tx ]
                        , viewImportantSummaryTx summary
                        , div [ HA.class "mt-4" ]
                            [ ctx.signingLink summary.tx
                                summary.expectedSignatures
                                [ Html.span [ HA.class "text-blue-600 hover:text-blue-800 underline" ]
                                    [ text "Sign & submit the transaction on the signing page" ]
                                ]
                            ]
                        ]
            , Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "DRep Unregistration" ]
            , Html.map ctx.wrapMsg <|
                Helper.formContainer
                    [ Html.p [] [ Helper.viewButton "Build Unregistration Tx" BuildUnregistrationTxButtonClicked ] ]
            , case model.unregisterTxSummary of
                Nothing ->
                    text ""

                Just summary ->
                    Helper.formContainer
                        [ Html.p [ HA.class "mb-2" ]
                            [ Html.strong [ HA.class "font-medium" ] [ text "Transaction ID: " ]
                            , Html.span [ HA.class "font-mono" ] [ text <| Bytes.toHex <| Transaction.computeTxId summary.tx ]
                            ]
                        , Html.p [ HA.class "mb-2" ] [ text "Transaction generated successfully", Html.span [ HA.style "color" "red" ] [ text " (₳ displayed as lovelaces):" ] ]
                        , Html.pre
                            [ HA.class "bg-gray-50 p-4 rounded-md border overflow-auto mt-2 text-sm whitespace-pre-wrap break-words"
                            , HA.style "border-color" "#C6C6C6"
                            , HA.style "max-height" "300px"
                            , HA.style "word-break" "break-all"
                            ]
                            [ text <| prettyTx summary.tx ]
                        , div [ HA.class "mt-4" ]
                            [ ctx.signingLink summary.tx
                                summary.expectedSignatures
                                [ Html.span [ HA.class "text-blue-600 hover:text-blue-800 underline" ]
                                    [ text "Sign & submit the transaction on the signing page" ]
                                ]
                            ]
                        ]
            , case model.error of
                Nothing ->
                    text ""

                Just err ->
                    Html.div [ HA.class "mt-4 p-4 bg-red-50 border rounded-md", HA.style "border-color" "#C6C6C6" ]
                        [ Html.p [ HA.class "text-red-600 font-medium mb-2" ] [ text "Error:" ]
                        , Html.pre [ HA.class "text-sm whitespace-pre-wrap" ] [ text err ]
                        ]
            ]
        ]


viewMultisigConfigForm : InnerModel -> Html Msg
viewMultisigConfigForm { minCount, hashes } =
    Helper.formContainer
        [ Html.div [ HA.class "flex items-center mb-4" ]
            [ Html.label [ HA.class "mr-3 font-medium" ] [ text "Minimum number of required signatures: " ]
            , Html.input
                [ HA.type_ "number"
                , HA.class "border rounded px-3 py-1 w-20 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
                , HA.style "border-color" "#C6C6C6"
                , HA.value (String.fromInt minCount)
                , HA.min "1"
                , Html.Events.onInput MinCountChange
                ]
                []
            ]
        , Html.div [ HA.class "mb-4" ]
            [ Html.div [ HA.class "flex items-center mb-2 mr-2" ]
                [ Helper.viewButton "Add public key hash" AddKeyButtonClicked
                ]
            ]
        , div [ HA.class "space-y-2" ] (List.indexedMap viewOneKeyForm hashes)
        ]


viewRegisterForm : InnerModel -> Html Msg
viewRegisterForm { register, createOutputRef } =
    Helper.formContainer
        [ Html.div [ HA.class "space-y-3 mb-4" ]
            [ Html.div [ HA.class "flex items-center" ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.id "register"
                    , HA.name "register"
                    , HA.checked register
                    , HA.class "mr-2 h-4 w-4"
                    , onCheck ToggleRegister
                    ]
                    []
                , Html.label [ HA.for "register", HA.class "select-none" ] [ text "Register as DRep" ]
                ]
            , Html.div [ HA.class "flex items-center" ]
                [ Html.input
                    [ HA.type_ "checkbox"
                    , HA.id "refOutput"
                    , HA.name "refOutput"
                    , HA.checked createOutputRef
                    , HA.class "mr-2 h-4 w-4"
                    , onCheck ToggleCreateOutputRef
                    ]
                    []
                , Html.label [ HA.for "refOutput", HA.class "select-none" ] [ text "Create an output reference" ]
                ]
            ]
        , Html.div [ HA.class "mt-4" ]
            [ Helper.viewButton "Build Registration Tx" BuildRegistrationTxButtonClicked ]
        ]


viewOneKeyForm : Int -> String -> Html Msg
viewOneKeyForm n hash =
    Html.div [ HA.class "flex items-center gap-2" ]
        [ Html.div [ HA.class "flex-1" ]
            [ Html.label [ HA.class "text-sm text-gray-600 mr-2" ] [ text "Key hash: " ]
            , Helper.textFieldInline hash (KeyHashChange n)
            ]
        , Helper.viewButton "Delete" (DeleteKeyButtonClicked n)
        ]


viewImportantSummaryTx : RegisterTxSummary -> Html msg
viewImportantSummaryTx { nativeScript, scriptHash, scriptRefInput, drepId } =
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
    div [ HA.class "mt-6 p-4 bg-blue-50 border rounded-md", HA.style "border-color" "#C6C6C6" ]
        [ Html.p [ HA.class "text-blue-800 font-medium mb-3" ]
            [ text "Important information to save for future multisig operations:" ]
        , Html.div [ HA.class "space-y-2" ]
            [ Html.div []
                [ Html.span [ HA.class "font-medium mr-2" ] [ text "DRep ID:" ]
                , Html.span [ HA.class "font-mono" ] [ text (Gov.idToBech32 drepId) ]
                ]
            , Html.div []
                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Native script hash:" ]
                , Html.span [ HA.class "font-mono" ] [ text (Bytes.toHex scriptHash) ]
                ]
            , Html.div []
                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Native script bytes:" ]
                , Html.span [ HA.class "font-mono break-all" ] [ text (Bytes.toHex scriptBytes) ]
                ]
            , Html.div []
                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Script reference UTxO:" ]
                , Html.span [ HA.class "font-mono" ] [ text scriptRef ]
                ]
            ]
        ]
