module Api exposing (ActiveProposal, ApiProvider, CcInfo, DrepInfo, IpfsAnswer(..), IpfsFile, PoolInfo, ProtocolParams, defaultApiProvider)

import Bytes as ElmBytes
import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (Credential(..), CredentialHash)
import Cardano.Gov exposing (ActionId, CostModels)
import Cardano.Pool as Pool
import Cardano.Transaction exposing (Transaction)
import Cardano.Utxo exposing (TransactionId)
import ConcurrentTask exposing (ConcurrentTask)
import ConcurrentTask.Http
import File exposing (File)
import Http
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import List.Extra
import Natural exposing (Natural)
import ProposalMetadata exposing (ProposalMetadata)
import RemoteData exposing (RemoteData)
import ScriptInfo exposing (ScriptInfo)


type alias ApiProvider msg =
    { loadProtocolParams : (Result Http.Error ProtocolParams -> msg) -> Cmd msg
    , loadGovProposals : (Result Http.Error (List ActiveProposal) -> msg) -> Cmd msg
    , loadProposalMetadata : String -> ConcurrentTask String ProposalMetadata
    , retrieveTx : Bytes TransactionId -> ConcurrentTask ConcurrentTask.Http.Error (Bytes Transaction)
    , getScriptInfo : Bytes CredentialHash -> ConcurrentTask ConcurrentTask.Http.Error ScriptInfo
    , getDrepInfo : Credential -> (Result Http.Error DrepInfo -> msg) -> Cmd msg
    , getCcInfo : Credential -> (Result Http.Error CcInfo -> msg) -> Cmd msg
    , getPoolLiveStake : Bytes Pool.Id -> (Result Http.Error PoolInfo -> msg) -> Cmd msg
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



-- Retrieve Txs


koiosFirstTxBytesDecoder : Decoder { txId : Bytes TransactionId, txCbor : Bytes a }
koiosFirstTxBytesDecoder =
    let
        oneTxBytesDecoder =
            JD.map2 (\txId cbor -> { txId = txId, txCbor = cbor })
                (JD.field "tx_hash" Bytes.jsonDecoder)
                (JD.field "cbor" Bytes.jsonDecoder)
    in
    JD.list oneTxBytesDecoder
        |> JD.andThen
            (\txs ->
                case txs of
                    [] ->
                        JD.fail "The Tx was not found. Maybe the request was done for the wrong network?"

                    [ tx ] ->
                        JD.succeed tx

                    _ ->
                        JD.fail "The server unexpectedly returned more than 1 transaction."
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


type alias PoolInfo =
    { pool : Bytes Pool.Id
    , stake : Int
    }


poolStakeDecoder : Bytes Pool.Id -> Decoder PoolInfo
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
            Err "Network error. Maybe you lost your connection, or an IPFS gateway did not respond in time, or some other network error occured."

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
    , loadProposalMetadata = taskLoadProposalMetadata

    -- Retrieve transactions via Koios by proxying with the server (to avoid CORS errors)
    , retrieveTx = taskRetrieveTx

    -- Retrieve script info via Koios by proxying with the server (to avoid CORS errors)
    , getScriptInfo = taskGetScriptInfo

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



-- Task port requests


taskLoadProposalMetadata : String -> ConcurrentTask String ProposalMetadata
taskLoadProposalMetadata url =
    let
        adjustedUrl =
            -- Differentiate HTTP and IPFS protocols to adjust the IPFS URL to a gateway
            if String.startsWith "ipfs://" url then
                "https://ipfs.io/ipfs/" ++ String.dropLeft 7 url

            else
                url
    in
    ConcurrentTask.Http.get
        { url = adjustedUrl
        , headers = []
        , expect = ConcurrentTask.Http.expectString
        , timeout = Nothing
        }
        |> ConcurrentTask.onError
            (\httpError ->
                case httpError of
                    -- NetworkError is potentially a CORS issue, so we proxy through the server
                    ConcurrentTask.Http.NetworkError ->
                        ConcurrentTask.Http.post
                            { url = "/proxy/json"
                            , headers = []
                            , body =
                                ConcurrentTask.Http.jsonBody
                                    (JE.object
                                        [ ( "url", JE.string adjustedUrl )
                                        , ( "method", JE.string "GET" )
                                        ]
                                    )
                            , expect = ConcurrentTask.Http.expectString
                            , timeout = Nothing
                            }

                    _ ->
                        ConcurrentTask.fromResult (Err httpError)
            )
        |> ConcurrentTask.map ProposalMetadata.fromRaw
        |> ConcurrentTask.onError
            (\httpError ->
                case httpError of
                    ConcurrentTask.Http.NetworkError ->
                        Err "Network error. Maybe you lost your connection, or the request was blocked by CORS on the server."
                            |> ConcurrentTask.fromResult

                    _ ->
                        Err (Debug.toString httpError)
                            |> ConcurrentTask.fromResult
            )


taskRetrieveTx : Bytes TransactionId -> ConcurrentTask ConcurrentTask.Http.Error (Bytes a)
taskRetrieveTx txId =
    let
        thisTxDecoder : Decoder (Bytes a)
        thisTxDecoder =
            koiosFirstTxBytesDecoder
                |> JD.andThen
                    (\tx ->
                        if tx.txId == txId then
                            JD.succeed tx.txCbor

                        else
                            JD.fail <| "The retrieved Tx (" ++ Bytes.toHex tx.txId ++ ") does not correspond to the expected one (" ++ Bytes.toHex txId ++ ")"
                    )
    in
    ConcurrentTask.Http.post
        { url = "/proxy/json"
        , headers = []
        , body =
            ConcurrentTask.Http.jsonBody
                (JE.object
                    [ ( "url", JE.string "https://preview.koios.rest/api/v1/tx_cbor" )
                    , ( "method", JE.string "POST" )
                    , ( "body", JE.object [ ( "_tx_hashes", JE.list (JE.string << Bytes.toHex) [ txId ] ) ] )
                    ]
                )
        , expect = ConcurrentTask.Http.expectJson thisTxDecoder
        , timeout = Nothing
        }


taskGetScriptInfo : Bytes CredentialHash -> ConcurrentTask ConcurrentTask.Http.Error ScriptInfo
taskGetScriptInfo scriptHash =
    ConcurrentTask.Http.post
        { url = "/proxy/json"
        , headers = []
        , body =
            ConcurrentTask.Http.jsonBody
                (JE.object
                    [ ( "url", JE.string "https://preview.koios.rest/api/v1/script_info" )
                    , ( "method", JE.string "POST" )
                    , ( "body", JE.object [ ( "_script_hashes", JE.list (JE.string << Bytes.toHex) [ scriptHash ] ) ] )
                    ]
                )
        , expect = ConcurrentTask.Http.expectJson ScriptInfo.koiosFirstScriptInfoDecoder
        , timeout = Nothing
        }
