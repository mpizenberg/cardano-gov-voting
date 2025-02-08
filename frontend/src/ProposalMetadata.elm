module ProposalMetadata exposing (Body, ProposalMetadata, decoder, encode, fromRaw)

import Json.Decode as JD exposing (Decoder, Value)
import Json.Encode as JE


type alias ProposalMetadata =
    { raw : String
    , body : Body
    }


type alias Body =
    { title : Maybe String
    , abstract : Maybe String
    }


noBody : Body
noBody =
    { title = Nothing
    , abstract = Nothing
    }


encode : ProposalMetadata -> Value
encode { raw } =
    JE.string raw


decoder : Decoder ProposalMetadata
decoder =
    JD.map fromRaw JD.string


fromRaw : String -> ProposalMetadata
fromRaw raw =
    JD.decodeString (JD.field "body" bodyDecoder) raw
        |> Result.map (ProposalMetadata raw)
        |> Result.withDefault (ProposalMetadata raw noBody)


bodyDecoder : Decoder Body
bodyDecoder =
    JD.map2 Body
        (JD.maybe <| JD.field "title" JD.string)
        (JD.maybe <| JD.field "abstract" JD.string)
