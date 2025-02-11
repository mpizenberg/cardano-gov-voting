module Page.Signing exposing (LoadedTxModel, Model(..), Msg(..), UpdateContext, ViewContext, addWalletSignatures, initialModel, recordSubmittedTx, resetSubmission, update, view)

{-| This module handles the signing process for Cardano transactions, particularly
focusing on complex scenarios like Native or Plutus script multi-signatures.

Key design features:

  - Supports both connected wallet signing and file-based signature collection
  - Tracks expected signers
  - Allows transaction submission even with partial signatures (useful for M-of-N schemes)
  - Handles signature deduplication and verification against expected signers

-}

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Utxo exposing (TransactionId)
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Helper exposing (shortenedHex)
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
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
    , expectedSigners : Dict String { keyHash : Bytes CredentialHash }
    , vkeyWitnesses : Dict String VKeyWitness
    , txSubmitted : Maybe (Bytes TransactionId)
    , error : Maybe String
    }


initialModel : List (Bytes CredentialHash) -> Maybe Transaction -> Model
initialModel expectedSigners maybeTx =
    case maybeTx of
        Just tx ->
            LoadedTx
                { tx = tx
                , txId = Transaction.computeTxId tx
                , expectedSigners =
                    expectedSigners
                        |> List.map (\hash -> ( Bytes.toHex hash, { keyHash = hash } ))
                        |> Dict.fromList
                , vkeyWitnesses = Dict.empty
                , txSubmitted = Nothing
                , error = Nothing
                }

        Nothing ->
            MissingTx



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
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    div []
        [ Html.h2 [] [ text "Signing the Tx" ]
        , Html.p []
            [ text "This page aims to facilitate complex signatures, "
            , text "such as Native or Plutus scripts multi-sig."
            ]
        , case model of
            MissingTx ->
                Html.p [] [ text "TODO: button to load the Tx from a file" ]

            LoadedTx { tx, txId, expectedSigners, vkeyWitnesses, txSubmitted, error } ->
                let
                    gatheredSignaturesSection =
                        div []
                            [ Html.div [] (viewExpectedSignatures expectedSigners vkeyWitnesses)
                            , Html.p [] [ text "Remark that at least one of these is to pay the Tx fees." ]
                            ]

                    signSection =
                        let
                            downloadButton label description fileName someTx =
                                Html.a
                                    [ HA.href <| "data:application/json;charset=utf-8," ++ Url.percentEncode (txUnsignedJson description someTx)
                                    , HA.download fileName
                                    ]
                                    [ Html.button [] [ text label ] ]

                            txUnsignedJson description someTx =
                                JE.encode 2 <|
                                    JE.object
                                        [ ( "type", JE.string "Tx ConwayEra" )
                                        , ( "description", JE.string description )
                                        , ( "cborHex", JE.string <| Bytes.toHex <| Transaction.serialize someTx )
                                        ]
                        in
                        div []
                            [ if ctx.wallet == Nothing then
                                Html.p [] [ text "If you want to sign with your web wallet, you need to connect it (see the page top)." ]

                              else if Dict.isEmpty vkeyWitnesses then
                                Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SignTxButtonClicked ] [ text "Sign Tx with connected wallet" ] ]

                              else
                                let
                                    signedTx =
                                        Transaction.updateSignatures (\_ -> Just <| Dict.values vkeyWitnesses) tx
                                in
                                Html.p []
                                    [ Html.button [ onClick <| ctx.wrapMsg SignTxButtonClicked ] [ text "Sign Tx with connected wallet" ]
                                    , text " "
                                    , downloadButton "Download partially signed Tx" "signed" "tx-signed.json" signedTx
                                    ]
                            , Html.p []
                                [ text <| "If additional signatures are required, please ask the relevant parties"
                                , text <| " to partially sign the transaction,"
                                , text <| " and use the button below to load their signatures in the app."
                                ]
                            , Html.p []
                                [ downloadButton "Download unsigned Tx" "unsigned" "tx-unsigned.json" tx
                                , text " "
                                , Html.button [ onClick <| ctx.wrapMsg LoadSignedTxButtonClicked ] [ text "Load signed Tx file" ]
                                ]
                            ]
                in
                div []
                    [ Html.p [] [ text <| "Tx ID: " ++ Bytes.toHex txId ]
                    , Html.p [] [ Html.pre [] [ text <| prettyTx tx ] ]
                    , if Dict.isEmpty expectedSigners then
                        div []
                            [ Html.h3 [] [ text <| "Gathered Signatures" ]
                            , gatheredSignaturesSection
                            , signSection
                            , Html.h3 [] [ text "Tx Submission" ]
                            , Html.p [] [ text "Expected signers are unknown so whenever you think it’s ready for submission, go for it!" ]
                            , Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Tx when ready!" ] ]
                            ]

                      else if Dict.isEmpty (Dict.diff expectedSigners vkeyWitnesses) then
                        div []
                            [ Html.h3 [] [ text <| "Expected Signatures" ]
                            , gatheredSignaturesSection
                            , Html.h3 [] [ text "Tx Submission" ]
                            , Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Tx" ] ]
                            ]

                      else
                        div []
                            [ Html.h3 [] [ text <| "Expected Signatures" ]
                            , gatheredSignaturesSection
                            , signSection
                            , Html.h3 [] [ text "Tx Submission" ]
                            , Html.p [] [ text "Not all expected signatures are gathered yet, but if you still think it’s ready, go for it!" ]
                            , Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Tx anyway!" ] ]
                            ]
                    , case txSubmitted of
                        Nothing ->
                            text ""

                        Just _ ->
                            Html.p [] [ text <| "Tx submitted! Tx ID: " ++ Bytes.toHex txId ]
                    , viewError error
                    ]
        ]


viewExpectedSignatures : Dict String { keyHash : Bytes CredentialHash } -> Dict String VKeyWitness -> List (Html msg)
viewExpectedSignatures expectedSigners vkeyWitnesses =
    let
        viewExpectedSigner hash =
            case Dict.get hash vkeyWitnesses of
                Just witness ->
                    Html.pre []
                        [ text <|
                            "✅ vkey hash: "
                                ++ shortenedHex 8 hash
                                ++ " { vkey: "
                                ++ shortenedHex 8 (Bytes.toHex witness.vkey)
                                ++ ", signature: "
                                ++ shortenedHex 8 (Bytes.toHex witness.signature)
                                ++ " }"
                        ]

                Nothing ->
                    Html.pre [] [ text <| "[_] vkey hash: " ++ shortenedHex 8 hash ]
    in
    Dict.keys expectedSigners
        |> List.map viewExpectedSigner


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
