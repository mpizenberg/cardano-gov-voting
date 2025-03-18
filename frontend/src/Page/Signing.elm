module Page.Signing exposing (Model, Msg, UpdateContext, ViewContext, addWalletSignatures, getTxInfo, initialModel, recordSubmittedTx, resetSubmission, update, view)

{-| This module handles the signing process for Cardano transactions, particularly
focusing on complex scenarios like Native or Plutus script multi-signatures.

Key design features:

  - Supports both connected wallet signing and file-based signature collection
  - Tracks expected signers
  - Allows transaction submission even with partial signatures (useful for M-of-N schemes)
  - Handles signature deduplication and verification against expected signers

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash, NetworkId(..))
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Utxo exposing (TransactionId)
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Helper exposing (shortenedHex)
import Html exposing (Html, a, div, text)
import Html.Attributes as HA
import Json.Decode as JD
import Json.Encode as JE
import Task
import Url



-- ###################################################################
-- MODEL
-- ###################################################################


{-| Represents the two possible states of the signing page:

  - MissingTx: No transaction loaded yet
  - LoadedTx: Active transaction being processed for signatures

-}
type Model
    = MissingTx
    | LoadedTx LoadedTxModel


{-| Core model for an active signing session.
Tracks the transaction, expected signers, collected signatures,
submission status and any errors that occur.
-}
type alias LoadedTxModel =
    { tx : Transaction
    , txId : Bytes TransactionId
    , expectedSigners : Dict String { keyName : String, keyHash : Bytes CredentialHash }
    , vkeyWitnesses : Dict String VKeyWitness
    , txSubmitted : Maybe (Bytes TransactionId)
    , error : Maybe String
    }


initialModel : List { keyName : String, keyHash : Bytes CredentialHash } -> Maybe Transaction -> Model
initialModel expectedSigners maybeTx =
    case maybeTx of
        Just tx ->
            LoadedTx
                { tx = tx
                , txId = Transaction.computeTxId tx
                , expectedSigners =
                    expectedSigners
                        |> List.map (\{ keyName, keyHash } -> ( Bytes.toHex keyHash, { keyName = keyName, keyHash = keyHash } ))
                        |> Dict.fromList
                , vkeyWitnesses = Dict.empty
                , txSubmitted = Nothing
                , error = Nothing
                }

        Nothing ->
            MissingTx


getTxInfo : Model -> Maybe { tx : Transaction, txId : Bytes TransactionId }
getTxInfo model =
    case model of
        LoadedTx loadedTxModel ->
            Just { tx = loadedTxModel.tx, txId = loadedTxModel.txId }

        _ ->
            Nothing



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | SignTxButtonClicked
    | LoadSignedTxButtonClicked
    | SignedTxFileSelected File
    | LoadedSignedTxJson String
    | SubmitTxButtonClicked


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , walletSignTx : Transaction -> Cmd msg
    , walletSubmitTx : Transaction -> Cmd msg
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case ( msg, model ) of
        ( SignTxButtonClicked, LoadedTx loadedTxModel ) ->
            ( model
            , ctx.walletSignTx loadedTxModel.tx
            )

        ( SignTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( LoadSignedTxButtonClicked, LoadedTx _ ) ->
            ( model
            , Cmd.map ctx.wrapMsg <|
                File.Select.file [] SignedTxFileSelected
            )

        ( LoadSignedTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( SignedTxFileSelected file, LoadedTx _ ) ->
            ( model
            , Task.attempt handleSignedTxFileRead (File.toString file)
                |> Cmd.map ctx.wrapMsg
            )

        ( SignedTxFileSelected _, _ ) ->
            ( model, Cmd.none )

        ( LoadedSignedTxJson txJsonStr, LoadedTx loadedTxModel ) ->
            case extractVkeyWitnesses loadedTxModel.txId txJsonStr of
                Ok sigs ->
                    ( addWalletSignatures sigs model, Cmd.none )

                Err error ->
                    ( LoadedTx { loadedTxModel | error = Just error }, Cmd.none )

        ( LoadedSignedTxJson _, _ ) ->
            ( model, Cmd.none )

        ( SubmitTxButtonClicked, LoadedTx { tx, vkeyWitnesses } ) ->
            let
                signedTx =
                    Transaction.updateSignatures (\_ -> Just <| Dict.values vkeyWitnesses) tx
            in
            ( model
            , ctx.walletSubmitTx signedTx
            )

        ( SubmitTxButtonClicked, _ ) ->
            ( model, Cmd.none )

        ( NoMsg, _ ) ->
            ( model, Cmd.none )


handleSignedTxFileRead : Result x String -> Msg
handleSignedTxFileRead result =
    case result of
        Err _ ->
            NoMsg

        Ok signedTxJson ->
            LoadedSignedTxJson signedTxJson


{-| Extracts VKey witnesses from a serialized transaction JSON, performing several validations:

1.  Ensures the transaction can be properly decoded
2.  Verifies the transaction ID matches the expected one
3.  Returns the list of witnesses if all checks pass

-}
extractVkeyWitnesses : Bytes TransactionId -> String -> Result String (List VKeyWitness)
extractVkeyWitnesses txId rawJson =
    let
        txJsonDecoder =
            JD.field "cborHex" JD.string
                |> JD.andThen
                    (\txHex ->
                        case Bytes.fromHex txHex |> Maybe.andThen Transaction.deserialize of
                            Just tx ->
                                JD.succeed tx

                            Nothing ->
                                JD.fail <| "Unable to decode Tx hex: " ++ txHex
                    )
    in
    case JD.decodeString txJsonDecoder rawJson of
        Ok tx ->
            let
                decodedTxId =
                    Transaction.computeTxId tx
            in
            if decodedTxId /= txId then
                Err <| "The wrong Tx was uploaded. It has a Tx ID of " ++ Bytes.toHex decodedTxId ++ " instead of " ++ Bytes.toHex txId

            else
                Ok <| Maybe.withDefault [] tx.witnessSet.vkeywitness

        Err error ->
            Err <| "Error while decoding the Tx: " ++ JD.errorToString error


{-| Adds new signatures to the model, with important filtering logic:

  - Only accepts signatures from expected signers
  - Handles cases where wallets might provide more signatures than needed
  - Updates existing signatures for the same key hash

-}
addWalletSignatures : List VKeyWitness -> Model -> Model
addWalletSignatures newVkeyWitnesses model =
    case model of
        LoadedTx loadedTxModel ->
            let
                -- Filter out unexpected signatures.
                -- Because sometimes, a wallet will provide more signatures than strictly needed.
                -- For example for some 1-of-n native multisig, a wallet might provide multiple signatures
                -- even if we only want 1, and only paid fees for one.
                expectedVkeyWitnesses =
                    List.filter keyHashIsExpected newVkeyWitnesses

                keyHashIsExpected vkeyWitness =
                    Dict.member (Bytes.toHex <| Transaction.hashVKey vkeyWitness.vkey) loadedTxModel.expectedSigners

                updatedWitnesses =
                    List.foldl (\w acc -> Dict.insert (keyHashHex w.vkey) w acc)
                        loadedTxModel.vkeyWitnesses
                        expectedVkeyWitnesses

                keyHashHex vkey =
                    Transaction.hashVKey vkey
                        |> Bytes.toHex
            in
            LoadedTx { loadedTxModel | vkeyWitnesses = updatedWitnesses }

        _ ->
            model


recordSubmittedTx : Bytes TransactionId -> Model -> Model
recordSubmittedTx txId model =
    case model of
        LoadedTx loadedTxModel ->
            LoadedTx { loadedTxModel | txSubmitted = Just txId }

        _ ->
            model


resetSubmission : String -> Model -> Model
resetSubmission error model =
    case model of
        MissingTx ->
            MissingTx

        LoadedTx loadedTxModel ->
            LoadedTx { loadedTxModel | error = Just error, txSubmitted = Nothing }



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    , networkId : NetworkId
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div [ HA.class "container mx-auto" ]
        [ Html.h2 [ HA.class "text-3xl font-medium my-4" ] [ text "Signing the Transaction" ]
        , Html.p [ HA.class "mb-4" ]
            [ text "This page aims to facilitate complex signatures, "
            , text "such as Native or Plutus scripts multi-sig."
            ]
        , case model of
            MissingTx ->
                Helper.formContainer
                    [ Html.p [] [ text "No transaction loaded. Please go back to the preparation page and create a transaction first." ] ]

            LoadedTx { tx, txId, expectedSigners, vkeyWitnesses, txSubmitted, error } ->
                let
                    gatheredSignaturesSection =
                        Helper.formContainer
                            [ Html.div [ HA.class "mb-4" ] (viewExpectedSignatures expectedSigners vkeyWitnesses)
                            , Html.p [ HA.class "text-gray-600 italic" ] [ text "At least one signature is required to pay the transaction fees." ]
                            ]

                    signSection =
                        let
                            downloadButton label description fileName someTx =
                                Html.a
                                    [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode (txUnsignedJson description someTx)
                                    , HA.download fileName
                                    , HA.class "inline-block"
                                    ]
                                    [ Helper.viewButton label (ctx.wrapMsg NoMsg) ]

                            txUnsignedJson description someTx =
                                JE.encode 2 <|
                                    JE.object
                                        [ ( "type", JE.string "Tx ConwayEra" )
                                        , ( "description", JE.string description )
                                        , ( "cborHex", JE.string <| Bytes.toHex <| Transaction.serialize someTx )
                                        ]
                        in
                        Helper.formContainer
                            [ if ctx.wallet == Nothing then
                                Html.p [ HA.class "mb-4 text-amber-600" ]
                                    [ text "If you want to sign with your web wallet, you need to connect it (see the page top)." ]

                              else if Dict.isEmpty vkeyWitnesses then
                                Html.p [ HA.class "mb-4" ]
                                    [ Helper.viewButton "Sign with connected wallet" (ctx.wrapMsg SignTxButtonClicked) ]

                              else
                                let
                                    signedTx =
                                        Transaction.updateSignatures (\_ -> Just <| Dict.values vkeyWitnesses) tx
                                in
                                Html.p [ HA.class "mb-4 flex items-center gap-3" ]
                                    [ Helper.viewButton "Sign with connected wallet" (ctx.wrapMsg SignTxButtonClicked)
                                    , downloadButton "Download partially signed Tx" "signed" "tx-signed.json" signedTx
                                    ]
                            , Html.p [ HA.class "mb-4" ]
                                [ text "If additional signatures are required, please ask the relevant parties"
                                , text " to partially sign the transaction,"
                                , text " and use the button below to load their signatures."
                                ]
                            , Html.p [ HA.class "mb-4 flex items-center" ]
                                [ downloadButton "Download unsigned Tx" "unsigned" "tx-unsigned.json" tx
                                , div [ HA.style "margin-left" "1rem" ]
                                    [ Helper.viewButton "Load signed Tx file" (ctx.wrapMsg LoadSignedTxButtonClicked) ]
                                ]
                            ]
                in
                div []
                    [ Helper.formContainer
                        [ Html.p [ HA.class "mb-2" ]
                            [ Html.strong [] [ text "Transaction ID: " ]
                            , Html.span [ HA.class "font-mono" ] [ text <| Bytes.toHex txId ]
                            ]
                        , Html.p [ HA.class "mb-2" ] [ text "Transaction details: (₳ amounts are in lovelaces)" ]
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
                        ]
                    , if Dict.isEmpty expectedSigners then
                        div []
                            [ Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Gathered Signatures" ]
                            , gatheredSignaturesSection
                            , signSection
                            , Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Transaction Submission" ]
                            , Helper.formContainer
                                [ Html.p [ HA.class "mb-4" ] [ text "Expected signers are unknown. Submit when you believe the transaction is ready." ]
                                , Html.p [] [ Helper.viewButton "Submit Transaction" (ctx.wrapMsg SubmitTxButtonClicked) ]
                                ]
                            ]

                      else if Dict.isEmpty (Dict.diff expectedSigners vkeyWitnesses) then
                        div []
                            [ Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Expected Signatures" ]
                            , gatheredSignaturesSection
                            , Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Transaction Submission" ]
                            , Helper.formContainer
                                [ Html.p [ HA.class "mb-4" ] [ text "All required signatures have been collected." ]
                                , Helper.viewButton "Submit Transaction" (ctx.wrapMsg SubmitTxButtonClicked)
                                ]
                            ]

                      else
                        div []
                            [ Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Expected Signatures" ]
                            , gatheredSignaturesSection
                            , signSection
                            , Html.h3 [ HA.class "text-xl font-medium mt-6 mb-2" ] [ text "Transaction Submission" ]
                            , Helper.formContainer
                                [ Html.p [ HA.class "mb-4" ] [ text "Not all expected signatures are gathered yet, but you can still submit if ready." ]
                                , Helper.viewButton "Submit Transaction Anyway" (ctx.wrapMsg SubmitTxButtonClicked)
                                ]
                            ]
                    , case txSubmitted of
                        Nothing ->
                            text ""

                        Just _ ->
                            Helper.formContainer
                                [ div
                                    [ HA.style "border-radius" "0.375rem"
                                    ]
                                    [ Html.p
                                        [ HA.style "color" "#15803d"
                                        , HA.style "font-weight" "600"
                                        , HA.style "font-size" "1.25rem"
                                        , HA.style "margin-bottom" "0.75rem"
                                        ]
                                        [ text "Transaction submitted successfully!" ]
                                    , Html.p [ HA.style "margin-bottom" "1rem" ]
                                        [ text "Your vote has been recorded on the Cardano blockchain." ]
                                    , div
                                        [ HA.style "border" "1px solid #C6C6C6"
                                        , HA.style "border-radius" "0.375rem"
                                        , HA.style "padding" "1rem"
                                        , HA.style "margin-bottom" "1rem"
                                        , HA.style "display" "inline-block"
                                        ]
                                        [ Html.p
                                            [ HA.style "font-size" "0.875rem"
                                            , HA.style "color" "#4b5563"
                                            , HA.style "margin-bottom" "0.25rem"
                                            ]
                                            [ text "Transaction ID:" ]
                                        , Html.p [ HA.style "font-family" "monospace", HA.style "font-weight" "500" ]
                                            [ text <| Bytes.toHex txId ]
                                        ]
                                    , Html.p
                                        [ HA.style "color" "#4b5563"
                                        , HA.style "margin-bottom" "0.75rem"
                                        ]
                                        [ text "Track your transaction:" ]
                                    , div [ HA.style "display" "flex", HA.style "gap" "1rem", HA.style "flex-wrap" "wrap" ]
                                        [ let
                                            cardanoScanBaseUrl =
                                                case ctx.networkId of
                                                    Mainnet ->
                                                        "https://cardanoscan.io/transaction/"

                                                    Testnet ->
                                                        "https://preview.cardanoscan.io/transaction/"
                                          in
                                          a
                                            [ HA.href (cardanoScanBaseUrl ++ Bytes.toHex txId)
                                            , HA.target "_blank"
                                            , HA.style "display" "inline-flex"
                                            , HA.style "align-items" "center"
                                            , HA.style "justify-content" "center"
                                            , HA.style "white-space" "nowrap"
                                            , HA.style "border-radius" "9999px"
                                            , HA.style "font-size" "0.875rem"
                                            , HA.style "font-weight" "500"
                                            , HA.style "transition" "all 0.2s"
                                            , HA.style "outline" "none"
                                            , HA.style "background-color" "#272727"
                                            , HA.style "color" "#f7fafc"
                                            , HA.style "padding" "0.75rem 1.5rem"
                                            ]
                                            [ text "View on CardanoScan" ]
                                        ]
                                    ]
                                ]
                    , viewError error
                    ]
        ]


viewExpectedSignatures : Dict String { keyName : String, keyHash : Bytes CredentialHash } -> Dict String VKeyWitness -> List (Html msg)
viewExpectedSignatures expectedSigners vkeyWitnesses =
    let
        viewExpectedSigner : String -> { keyName : String, keyHash : Bytes CredentialHash } -> Html msg
        viewExpectedSigner keyHash { keyName } =
            case Dict.get keyHash vkeyWitnesses of
                Just witness ->
                    Html.div [ HA.class "bg-green-50 border p-3 rounded-md mb-2 flex items-center", HA.style "border-color" "#C6C6C6" ]
                        [ Html.div [ HA.class "font-bold", HA.style "margin-left" "6px", HA.style "margin-right" "6px" ] [ text "✓" ]
                        , Html.div [ HA.class "font-mono text-sm" ]
                            [ Html.div [] [ text <| keyName ++ ": " ++ shortenedHex 8 keyHash ]
                            , Html.div [ HA.class "text-gray-600" ]
                                [ text <| "VKey: " ++ shortenedHex 8 (Bytes.toHex witness.vkey) ]
                            , Html.div [ HA.class "text-gray-600" ]
                                [ text <| "Signature: " ++ shortenedHex 8 (Bytes.toHex witness.signature) ]
                            ]
                        ]

                Nothing ->
                    Html.div [ HA.class "bg-gray-50 border p-3 rounded-md mb-2 flex items-center", HA.style "border-color" "#C6C6C6" ]
                        [ Html.div [ HA.class "mr-2 text-gray-600" ] [ text "□" ]
                        , Html.div [ HA.class "font-mono text-sm" ]
                            [ text <| keyName ++ ": " ++ shortenedHex 8 keyHash ]
                        ]
    in
    Dict.map viewExpectedSigner expectedSigners
        |> Dict.values


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
