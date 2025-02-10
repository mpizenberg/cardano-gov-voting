module ScriptInfo exposing (ScriptInfo, koiosFirstScriptInfoDecoder, storageDecoder, storageEncode)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address exposing (CredentialHash)
import Cardano.Script as Script exposing (PlutusVersion(..), Script)
import Cbor.Decode
import Cbor.Encode
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


type alias ScriptInfo =
    { scriptHash : Bytes CredentialHash
    , script : Script
    , nativeCborEncodingMatchesHash : Maybe Bool
    }


storageEncode : ScriptInfo -> Value
storageEncode { scriptHash, script } =
    JE.object
        [ ( "scriptHash", Bytes.jsonEncode scriptHash )
        , ( "scriptCbor", Bytes.jsonEncode <| Bytes.fromBytes <| Cbor.Encode.encode <| Script.toCbor script )
        ]


storageDecoder : Decoder ScriptInfo
storageDecoder =
    let
        scriptFromBytesDecoder bytes =
            case Cbor.Decode.decode Script.fromCbor bytes of
                Nothing ->
                    JD.fail "Unable to decode the script"

                Just script ->
                    JD.succeed script

        checkEncoding hash script =
            case script of
                Script.Native _ ->
                    { scriptHash = hash
                    , script = script
                    , nativeCborEncodingMatchesHash = Just <| Script.hash script == hash
                    }

                Script.Plutus _ ->
                    { scriptHash = hash
                    , script = script
                    , nativeCborEncodingMatchesHash = Nothing
                    }
    in
    JD.map2 checkEncoding
        (JD.field "scriptHash" Bytes.jsonDecoder)
        (JD.field "scriptCbor" (JD.map Bytes.toBytes Bytes.jsonDecoder)
            |> JD.andThen scriptFromBytesDecoder
        )


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
