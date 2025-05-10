module ProposalMetadata exposing (Body, ProposalMetadata, decoder, encode, fromRaw)

{-| Helper module to handle proposals metadata following [CIP-108](https://cips.cardano.org/cip/CIP-0108).
-}

import Bytes.Comparable as Bytes
import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


{-| Proposal metadata, following [CIP-108](https://cips.cardano.org/cip/CIP-0108).
We keep the raw metadata in order to be able to display it
even if the metadata itself doesn’t follow CIP-108.
-}
type alias ProposalMetadata =
    { raw : String
    , computedHash : String
    , body : Body
    }


{-| Body of the CIP-108 metadata JSON object.
All fields are optional here to better handle mistakes when creating the metadata.
-}
type alias Body =
    { title : Maybe String
    , abstract : Maybe String
    }


noBody : Body
noBody =
    { title = Nothing
    , abstract = Nothing
    }


{-| JSON encoder, simply using the raw metadata.
-}
encode : ProposalMetadata -> Value
encode { raw } =
    JE.string raw


{-| JSON decoder for proposal metadata.
-}
decoder : Decoder ProposalMetadata
decoder =
    JD.map fromRaw JD.string


{-| Extract proposal metadata, trying to decode its CIP-108 structure.
-}
fromRaw : String -> ProposalMetadata
fromRaw raw =
    let
        computedHash =
            Bytes.fromText raw
                |> Bytes.blake2b256
                |> Bytes.toHex
    in
    JD.decodeString (JD.field "body" bodyDecoder) raw
        |> Result.map (ProposalMetadata raw computedHash)
        |> Result.withDefault (ProposalMetadata raw computedHash noBody)


bodyDecoder : Decoder Body
bodyDecoder =
    JD.map2 Body
        (JD.maybe <| JD.field "title" JD.string)
        (JD.maybe <| JD.field "abstract" JD.string)
