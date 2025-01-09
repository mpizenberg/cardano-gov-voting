module Page.Signing exposing (..)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano exposing (dummyBytes)
import Cardano.Address exposing (Address, CredentialHash)
import Cardano.Cip30 as Cip30
import Cardano.Transaction as Transaction exposing (Transaction, VKeyWitness)
import Cardano.TxExamples exposing (prettyTx)
import Cardano.Utxo as Utxo exposing (Output, TransactionId)
import Dict exposing (Dict)
import Helper exposing (shortenedHex)
import Html exposing (Html, div, text)



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
    }


initialModel : List String -> Maybe Transaction -> Model
initialModel expectedSigners maybeTx =
    case maybeTx of
        Just tx ->
            LoadedTx
                { tx = tx
                , txId = dummyBytes 32 "TODO: Tx ID from Tx"
                , expectedSigners =
                    List.filterMap Bytes.fromHex expectedSigners
                        |> List.map (\hash -> ( Bytes.toHex hash, { keyHash = hash } ))
                        |> Dict.fromList
                , vkeyWitnesses = Dict.empty
                }

        Nothing ->
            MissingTx



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update _ _ =
    Debug.todo ""



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    , wallet : Maybe Cip30.Wallet
    }


view : ViewContext msg -> Model -> Html msg
view _ model =
    div []
        [ Html.h2 [] [ text "Signing the vote Tx" ]
        , Html.p []
            [ text "This page aims to facilitate complex signatures, "
            , text "such as Native or Plutus scripts multi-sig."
            ]
        , case model of
            MissingTx ->
                Html.p [] [ text "TODO: button to load the Tx from a file" ]

            LoadedTx { tx, expectedSigners, vkeyWitnesses } ->
                -- let
                --     ( proposal, vote ) =
                --         Debug.todo "extract proposal and vote from Tx"
                -- in
                div []
                    [ Html.p [] [ text <| "Tx ID: TODO" ]
                    , Html.p [] [ Html.pre [] [ text <| prettyTx tx ] ]
                    , Html.h3 [] [ text <| "Expected Signatures" ]
                    , Html.div [] (viewExpectedSignatures expectedSigners vkeyWitnesses)
                    , Html.p []
                        [ text <| "Remark that at least one of these is to pay the Tx fees."
                        , text <| " If you are paying the fees, or signing the vote with your wallet,"
                        , text <| " make sure it’s connected and use the button below to sign."
                        ]
                    , Html.p [] [ text <| "TODO: button to sign with connected wallet" ]
                    , Html.p []
                        [ text <| "If additional signatures are required, please ask the relevant parties"
                        , text <| " to partially sign the transaction,"
                        , text <| " and use the button below to load their signatures in the app."
                        ]
                    , Html.p [] [ text <| "TODO: button to load the signed Tx from disk" ]
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
