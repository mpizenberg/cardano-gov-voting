module Api exposing (ActiveProposal, ApiProvider, CcInfo, DrepInfo, IpfsAnswer(..), IpfsFile, ProposalMetadata, ProtocolParams, ScriptInfo, defaultApiProvider)

import Bytes as ElmBytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Credential(..), CredentialHash)
import Cardano.Gov exposing (ActionId, CostModels)
import Cardano.Pool as Pool
import Cardano.Script as Script exposing (PlutusVersion(..), Script)
import Cardano.Transaction as Transaction exposing (Transaction)
import Cardano.Utxo exposing (TransactionId)
import Dict exposing (Dict)
import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra
import Natural exposing (Natural)
import RemoteData exposing (RemoteData)


type alias ApiProvider msg =
    { loadProtocolParams : (Result Http.Error ProtocolParams -> msg) -> Cmd msg
    , loadGovProposals : (Result Http.Error (List ActiveProposal) -> msg) -> Cmd msg
    , loadProposalMetadata : String -> (Result String ProposalMetadata -> msg) -> Cmd msg
    , retrieveTxs : List (Bytes TransactionId) -> (Result Http.Error (Dict String Transaction) -> msg) -> Cmd msg
    , getScriptInfo : Bytes CredentialHash -> (Result Http.Error ScriptInfo -> msg) -> Cmd msg
    , getDrepInfo : Credential -> (Result Http.Error DrepInfo -> msg) -> Cmd msg
    , getCcInfo : Credential -> (Result Http.Error CcInfo -> msg) -> Cmd msg
    , getPoolLiveStake : Bytes Pool.Id -> (Result Http.Error { pool : Bytes Pool.Id, stake : Int } -> msg) -> Cmd msg
    , ipfsAdd : { rpc : String, headers : List ( String, String ), file : File } -> (Result String IpfsAnswer -> msg) -> Cmd msg
    , convertToPdf : String -> (Result Http.Error ElmBytes.Bytes -> msg) -> Cmd msg
    }



-- Protocol Parameters


type alias ProtocolParams =
    { costModels : CostModels
    , drepDeposit : Natural
    }


protocolParamsDecoder : Decoder ProtocolParams
protocolParamsDecoder =
    JD.map4
        (\v1 v2 v3 drepDeposit ->
            { costModels = CostModels (Just v1) (Just v2) (Just v3)
            , drepDeposit = drepDeposit
            }
        )
        (JD.at [ "result", "plutusCostModels", "plutus:v1" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v2" ] <| JD.list JD.int)
        (JD.at [ "result", "plutusCostModels", "plutus:v3" ] <| JD.list JD.int)
        (JD.at [ "result", "delegateRepresentativeDeposit", "ada", "lovelace" ] <| JD.map Natural.fromSafeInt JD.int)



-- Governance Proposals


type alias ActiveProposal =
    { id : ActionId
    , actionType : String
    , metadataUrl : String
    , metadataHash : String
    , metadata : RemoteData String ProposalMetadata
    }


type alias ProposalMetadata =
    { title : Maybe String
    , abstract : Maybe String
    , raw : String
    }


proposalsDecoder : Decoder (List ActiveProposal)
proposalsDecoder =
    JD.field "result" <|
        JD.list <|
            JD.map5 ActiveProposal
                (JD.map2
                    (\id index ->
                        { transactionId = Bytes.fromHexUnchecked id
                        , govActionIndex = index
                        }
                    )
                    (JD.at [ "proposal", "transaction", "id" ] JD.string)
                    (JD.at [ "proposal", "index" ] JD.int)
                )
                (JD.at [ "action", "type" ] JD.string)
                (JD.at [ "metadata", "url" ] JD.string)
                (JD.at [ "metadata", "hash" ] JD.string)
                (JD.succeed RemoteData.Loading)


decodeProposalMetadata : String -> ProposalMetadata
decodeProposalMetadata raw =
    let
        titleAndAbstractDecoder : Decoder ( Maybe String, Maybe String )
        titleAndAbstractDecoder =
            JD.map2 Tuple.pair
                (JD.maybe <| JD.at [ "body", "title" ] JD.string)
                (JD.maybe <| JD.at [ "body", "abstract" ] JD.string)
    in
    JD.decodeString titleAndAbstractDecoder raw
        |> Result.map (\( title, abstract ) -> ProposalMetadata title abstract raw)
        |> Result.withDefault (ProposalMetadata Nothing Nothing raw)



-- Retrieve Txs


koiosTxCborDecoder : Decoder (Dict String Transaction)
koiosTxCborDecoder =
    let
        singleTxDecoder : Decoder ( String, Transaction )
        singleTxDecoder =
            JD.map2 Tuple.pair
                (JD.field "tx_hash" JD.string)
                (JD.field "cbor" JD.string)
                |> JD.andThen
                    (\( hashHex, cborHex ) ->
                        case Bytes.fromHex cborHex |> Maybe.andThen Transaction.deserialize of
                            Just tx ->
                                JD.succeed ( hashHex, tx )

                            Nothing ->
                                JD.fail <| "Failed to deserialize Tx: " ++ cborHex
                    )
    in
    JD.list singleTxDecoder
        |> JD.map Dict.fromList



-- Script Info


type alias ScriptInfo =
    { scriptHash : Bytes CredentialHash
    , script : Script
    , nativeCborEncodingMatchesHash : Maybe Bool
    }


koiosFirstScriptInfoDecoder : Decoder ScriptInfo
koiosFirstScriptInfoDecoder =
    JD.list koiosScriptInfoDecoder
        |> JD.andThen
            (\list ->
                case list of
                    [] ->
                        JD.fail "No script info found"

                    first :: _ ->
                        JD.succeed first
            )


koiosScriptInfoDecoder : Decoder ScriptInfo
koiosScriptInfoDecoder =
    JD.map4
        (\hashHex scriptType maybeNative maybePlutusBytes ->
            case Bytes.fromHex hashHex of
                Nothing ->
                    Err <| "Unable to decode the script hash: " ++ hashHex

                Just hash ->
                    if List.member scriptType [ "multisig", "timelock" ] then
                        case maybeNative of
                            Nothing ->
                                Err "Missing native script in Koios response"

                            Just script ->
                                Ok
                                    { scriptHash = hash
                                    , script = Script.Native script
                                    , nativeCborEncodingMatchesHash = Just <| Script.hash (Script.Native script) == hash
                                    }

                    else
                        let
                            plutusVersion =
                                case scriptType of
                                    "plutusV1" ->
                                        Ok PlutusV1

                                    "plutusV2" ->
                                        Ok PlutusV2

                                    "plutusV3" ->
                                        Ok PlutusV3

                                    _ ->
                                        Err <| "Unknown script type: " ++ scriptType
                        in
                        case ( plutusVersion, maybePlutusBytes |> Maybe.andThen Bytes.fromHex ) of
                            ( Ok version, Just bytes ) ->
                                Ok
                                    { scriptHash = hash
                                    , script = Script.Plutus { version = version, script = bytes }
                                    , nativeCborEncodingMatchesHash = Nothing
                                    }

                            ( Err error, _ ) ->
                                Err error

                            ( _, Nothing ) ->
                                Err <| "Missing (or invalid) script CBOR bytes: " ++ Debug.toString maybePlutusBytes
        )
        (JD.field "script_hash" JD.string)
        (JD.field "type" JD.string)
        (JD.field "value" <| JD.maybe Script.jsonDecodeNativeScript)
        (JD.field "bytes" <| JD.maybe JD.string)
        |> JD.andThen
            (\result ->
                case result of
                    Err error ->
                        JD.fail error

                    Ok info ->
                        JD.succeed info
            )



-- DRep Info


type AnyDrepInfo
    = Abstain
    | NoConfidence
    | Registered DrepInfo


type alias DrepInfo =
    { credential : Credential

    -- TODO: eventually need to change to Natural,
    -- but it’s not possible with regular http requests,
    -- because Elm will call JSON.parse() which will silently
    -- lose precision on integers > 2^53
    , votingPower : Int
    }


ogmiosSpecificDrepInfoDecoder : Credential -> Decoder DrepInfo
ogmiosSpecificDrepInfoDecoder cred =
    let
        extractRegistered anyDrep =
            case anyDrep of
                Registered info ->
                    Just info

                _ ->
                    Nothing
    in
    JD.field "result" (JD.list ogmiosAnyDrepInfoDecoder)
        |> JD.map (List.filterMap extractRegistered)
        |> JD.map (List.Extra.find (\drep -> drep.credential == cred))
        |> JD.andThen
            (\drepFound ->
                case drepFound of
                    Nothing ->
                        JD.fail <| "DRep not in the response: " ++ Debug.toString cred

                    Just drepInfo ->
                        JD.succeed drepInfo
            )


ogmiosAnyDrepInfoDecoder : Decoder AnyDrepInfo
ogmiosAnyDrepInfoDecoder =
    JD.field "type" JD.string
        |> JD.andThen
            (\drepType ->
                case drepType of
                    "abstain" ->
                        JD.succeed Abstain

                    "noConfidence" ->
                        JD.succeed NoConfidence

                    "registered" ->
                        JD.map Registered ogmiosRegisteredDrepInfoDecoder

                    _ ->
                        JD.fail <| "Unknown DRep type: " ++ drepType
            )


ogmiosRegisteredDrepInfoDecoder : Decoder DrepInfo
ogmiosRegisteredDrepInfoDecoder =
    JD.map2 (\cred stake -> { credential = cred, votingPower = stake })
        ogmiosCredDecoder
        (JD.at [ "stake", "ada", "lovelace" ] JD.int)


ogmiosCredDecoder : Decoder Credential
ogmiosCredDecoder =
    JD.map2 Tuple.pair (JD.field "from" JD.string) (JD.field "id" JD.string)
        |> JD.andThen
            (\( from, id ) ->
                case ( from, Bytes.fromHex id ) of
                    ( "verificationKey", Just hash ) ->
                        JD.succeed <| VKeyHash hash

                    ( "script", Just hash ) ->
                        JD.succeed <| ScriptHash hash

                    _ ->
                        JD.fail <| "Invalid credential: " ++ from ++ ", " ++ id
            )



-- CC Info


type alias CcInfo =
    { coldCred : Credential
    , hotCred : Credential
    , status : String
    , epochMandateEnd : Int
    }


ogmiosSpecificCcInfoDecoder : Credential -> Decoder CcInfo
ogmiosSpecificCcInfoDecoder hotCred =
    JD.at [ "result", "members" ] (JD.list ogmiosCcInfoDecoder)
        |> JD.map (List.Extra.find (\info -> info.hotCred == hotCred))
        |> JD.andThen
            (\maybeInfo ->
                case maybeInfo of
                    Nothing ->
                        JD.fail <| "Hot cred not present in CC members: " ++ Debug.toString hotCred

                    Just info ->
                        JD.succeed info
            )


ogmiosCcInfoDecoder : Decoder CcInfo
ogmiosCcInfoDecoder =
    JD.map4 CcInfo
        -- Cold credential at the top, then Hot credential inside the "delegate" field
        ogmiosCredDecoder
        (JD.field "delegate" ogmiosCredDecoder)
        (JD.field "status" JD.string)
        (JD.at [ "mandate", "epoch" ] JD.int)



-- Pool Live Stake


poolStakeDecoder : Bytes Pool.Id -> Decoder { pool : Bytes Pool.Id, stake : Int }
poolStakeDecoder poolId =
    JD.map (\stake -> { pool = poolId, stake = stake })
        (JD.at [ "result", Pool.toBech32 poolId, "stake", "ada", "lovelace" ] JD.int)



-- IPFS


type IpfsAnswer
    = IpfsError String
    | IpfsAddSuccessful IpfsFile


type alias IpfsFile =
    { name : String
    , cid : String
    , size : String
    }


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



-- Default API Provider


defaultApiProvider : ApiProvider msg
defaultApiProvider =
    -- Get protocol parameters via Koios
    { loadProtocolParams =
        \toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/protocolParameters" )
                            ]
                        )
                , expect = Http.expectJson toMsg protocolParamsDecoder
                }

    -- Get governance proposals via Koios
    , loadGovProposals =
        \toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/governanceProposals" )
                            ]
                        )
                , expect = Http.expectJson toMsg proposalsDecoder
                }

    -- Load the metadata associated with a governance proposal
    , loadProposalMetadata =
        \url toMsg ->
            let
                decodeData : Http.Response String -> Result String ProposalMetadata
                decodeData response =
                    case response of
                        Http.GoodStatus_ _ body ->
                            Ok <| decodeProposalMetadata body

                        Http.NetworkError_ ->
                            Err "Network error. Maybe you lost your connection, or the request was blocked by CORS on the server."

                        _ ->
                            Err <| Debug.toString response

                adjustedUrl =
                    -- Differentiate HTTP and IPFS protocols to adjust the IPFS URL to a gateway
                    if String.startsWith "ipfs://" url then
                        "https://ipfs.io/ipfs/" ++ String.dropLeft 7 url

                    else
                        url
            in
            Http.get
                { url = adjustedUrl
                , expect = Http.expectStringResponse toMsg decodeData
                }

    -- Retrieve transactions via Koios by proxying with the server (to avoid CORS errors)
    , retrieveTxs =
        \txIds toMsg ->
            Http.post
                { url = "/proxy/json"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "url", JE.string "https://preview.koios.rest/api/v1/tx_cbor" )
                            , ( "method", JE.string "POST" )
                            , ( "body", JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) txIds ) ] )
                            ]
                        )
                , expect = Http.expectJson toMsg koiosTxCborDecoder
                }

    -- Retrieve script info via Koios by proxying with the server (to avoid CORS errors)
    , getScriptInfo =
        \scriptHash toMsg ->
            Http.post
                { url = "/proxy/json"
                , body =
                    Http.jsonBody
                        (JE.object
                            -- [ ( "url", JE.string "https://api.koios.rest/api/v1/script_info" )
                            [ ( "url", JE.string "https://preview.koios.rest/api/v1/script_info" )
                            , ( "method", JE.string "POST" )
                            , ( "body", JE.object [ ( "_script_hashes", JE.list (JE.string << Bytes.toHex) [ scriptHash ] ) ] )
                            ]
                        )
                , expect = Http.expectJson toMsg koiosFirstScriptInfoDecoder
                }

    -- Retrieve DRep info via Koios using an Ogmios endpoint
    , getDrepInfo =
        \cred toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/delegateRepresentatives" )
                            , ( "params"
                              , case cred of
                                    VKeyHash hash ->
                                        JE.object [ ( "keys", JE.list JE.string <| [ Bytes.toHex hash ] ) ]

                                    ScriptHash hash ->
                                        JE.object [ ( "scripts", JE.list JE.string <| [ Bytes.toHex hash ] ) ]
                              )
                            ]
                        )
                , expect = Http.expectJson toMsg (ogmiosSpecificDrepInfoDecoder cred)
                }

    -- Retrieve CC member info
    , getCcInfo =
        \cred toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/constitutionalCommittee" )
                            ]
                        )
                , expect = Http.expectJson toMsg (ogmiosSpecificCcInfoDecoder cred)
                }

    -- Retrieve Pool live stake (end of previous epoch)
    , getPoolLiveStake =
        \poolId toMsg ->
            Http.post
                { url = "https://preview.koios.rest/api/v1/ogmios"
                , body =
                    Http.jsonBody
                        (JE.object
                            [ ( "jsonrpc", JE.string "2.0" )
                            , ( "method", JE.string "queryLedgerState/stakePools" )
                            , ( "params"
                              , JE.object
                                    [ ( "includeStake", JE.bool True )
                                    , ( "stakePools"
                                      , JE.list
                                            (\p -> JE.object [ ( "id", JE.string <| Bytes.toHex p ) ])
                                            [ poolId ]
                                      )
                                    ]
                              )
                            ]
                        )
                , expect = Http.expectJson toMsg (poolStakeDecoder poolId)
                }

    -- Make a request to an IPFS RPC
    , ipfsAdd =
        \{ rpc, headers, file } toMsg ->
            Http.request
                { method = "POST"
                , headers = List.map (\( k, v ) -> Http.header k v) headers
                , url = rpc ++ "/add"
                , body = Http.multipartBody [ Http.filePart "file" file ]
                , expect = Http.expectStringResponse toMsg responseToIpfsAnswer
                , timeout = Nothing
                , tracker = Nothing
                }

    -- Convert the JSON LD file into a pretty PDF on the server
    , convertToPdf =
        \jsonFileContent toMsg ->
            Http.post
                { url = "/pretty-gov-pdf"
                , body = Http.stringBody "application/json" jsonFileContent
                , expect = Http.expectBytesResponse toMsg bytesResponseToResult
                }
    }


bytesResponseToResult : Http.Response ElmBytes.Bytes -> Result Http.Error ElmBytes.Bytes
bytesResponseToResult response =
    case response of
        Http.BadUrl_ str ->
            Err <| Http.BadUrl str

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ meta _ ->
            Err <| Http.BadStatus meta.statusCode

        Http.GoodStatus_ _ bytes ->
            Ok bytes
