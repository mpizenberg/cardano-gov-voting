module Page.Signing exposing (LoadedTxModel, Model(..), Msg(..), UpdateContext, ViewContext, addWalletSignatures, initialModel, recordSubmittedTx, update, view)

import Blake2b exposing (blake2b224)
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (dummyBytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Utxo exposing (TransactionId)
import Dict exposing (Dict)
import Helper exposing (shortenedHex)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)



-- ###################################################################
-- MODEL
-- ###################################################################


type Model
    = MissingTx
    | LoadedTx LoadedTxModel


type alias LoadedTxModel =
    { tx : Transaction
    , txId : Bytes TransactionId
    , expectedSigners : Dict String { keyHash : Bytes CredentialHash }
    , vkeyWitnesses : Dict String VKeyWitness
    , txSubmitted : Maybe (Bytes TransactionId)
    }


initialModel : List String -> Maybe Transaction -> Model
initialModel expectedSigners maybeTx =
    case maybeTx of
        Just tx ->
            LoadedTx
                { tx = tx
                , txId = Transaction.computeTxId tx
                , expectedSigners =
                    List.filterMap Bytes.fromHex expectedSigners
                        |> List.map (\hash -> ( Bytes.toHex hash, { keyHash = hash } ))
                        |> Dict.fromList
                , vkeyWitnesses = Dict.empty
                , txSubmitted = Nothing
                }

        Nothing ->
            MissingTx



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | SignTxButtonClicked
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


addWalletSignatures : List VKeyWitness -> Model -> Model
addWalletSignatures newVkeyWitnesses model =
    case model of
        LoadedTx loadedTxModel ->
            let
                keyHashHex vkey =
                    Bytes.toU8 vkey
                        |> blake2b224 Nothing
                        |> Bytes.fromU8
                        |> Bytes.toHex

                updatedWitnesses =
                    List.foldl (\w acc -> Dict.insert (keyHashHex w.vkey) w acc)
                        loadedTxModel.vkeyWitnesses
                        newVkeyWitnesses
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

            LoadedTx { tx, txId, expectedSigners, vkeyWitnesses, txSubmitted } ->
                div []
                    [ Html.p [] [ text <| Bytes.toHex txId ]
                    , Html.p [] [ Html.pre [] [ text <| prettyTx tx ] ]
                    , Html.h3 [] [ text <| "Expected Signatures" ]
                    , Html.div [] (viewExpectedSignatures expectedSigners vkeyWitnesses)
                    , Html.p []
                        [ text <| "Remark that at least one of these is to pay the Tx fees."
                        , text <| " If you are paying the fees, or signing the vote with your wallet,"
                        , text <| " make sure it’s connected and use the button below to sign."
                        ]
                    , Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SignTxButtonClicked ] [ text "Sign Tx with connected wallet" ] ]
                    , Html.p []
                        [ text <| "If additional signatures are required, please ask the relevant parties"
                        , text <| " to partially sign the transaction,"
                        , text <| " and use the button below to load their signatures in the app."
                        ]
                    , Html.p [] [ text <| "TODO: button to download unsigned Tx and another to upload the signed Tx from disk" ]
                    , Html.h3 [] [ text "Tx Submission" ]
                    , if Dict.isEmpty expectedSigners then
                        div []
                            [ Html.p [] [ text "Expected signers are unknown so whenever you think it’s ready for submission, go for it!" ]
                            , Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Tx anyway!" ] ]
                            ]

                      else if Dict.isEmpty (Dict.diff expectedSigners vkeyWitnesses) then
                        Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Tx" ] ]

                      else
                        div []
                            [ Html.p [] [ text "Not all expected signatures are gathered yet, but if you still think it’s ready, go for it!" ]
                            , Html.p [] [ Html.button [ onClick <| ctx.wrapMsg SubmitTxButtonClicked ] [ text "Submit Tx anyway!" ] ]
                            ]
                    , case txSubmitted of
                        Nothing ->
                            text ""

                        Just _ ->
                            Html.p [] [ text <| "Tx submitted! Tx ID: " ++ Bytes.toHex txId ]
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
