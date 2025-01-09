module Helper exposing (prettyAddr, shortenedBytes, shortenedHex)

import Bytes.Comparable as Bytes exposing (Bytes)
import Cardano.Address as Address exposing (Address)


prettyAddr : Address -> String
prettyAddr address =
    let
        addrHex =
            Bytes.toHex (Address.toBytes address)
    in
    String.slice 0 8 addrHex
        ++ "..."
        ++ String.slice -8 (String.length addrHex) addrHex


shortenedBytes : Int -> Bytes a -> String
shortenedBytes width bytes =
    shortenedHex width <| Bytes.toHex bytes


shortenedHex : Int -> String -> String
shortenedHex width bytesHex =
    String.slice 0 width bytesHex
        ++ "..."
        ++ String.slice -width (String.length bytesHex) bytesHex
