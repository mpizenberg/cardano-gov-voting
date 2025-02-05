module Page.Pdf exposing (GovMetadataFile(..), Model, Msg(..), UpdateContext, ViewContext, initialModel, update, view)

import Api
import Bytes as ElmBytes
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, div, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import Page.Preparation as Preparation exposing (InternalVote, Rationale, Reference, ReferenceType(..))
import Platform.Cmd as Cmd
import Task



-- ###################################################################
-- MODEL
-- ###################################################################


type alias Model =
    { fileContent : Maybe { raw : String, name : String, decoded : GovMetadataFile }
    , pdfBytes : Maybe ElmBytes.Bytes
    , error : Maybe String
    }


initialModel : Model
initialModel =
    { fileContent = Nothing
    , pdfBytes = Nothing
    , error = Nothing
    }


type GovMetadataFile
    = VoteRationale Rationale -- CIP-136



-- ###################################################################
-- UPDATE
-- ###################################################################


type Msg
    = NoMsg
    | LoadJsonButtonClicked
    | JsonFileSelected File
    | LoadedJson String String
    | ConvertToPdfButtonClicked String
    | GotPdfFile (Result Http.Error ElmBytes.Bytes)


type alias UpdateContext msg =
    { wrapMsg : Msg -> msg
    }


update : UpdateContext msg -> Msg -> Model -> ( Model, Cmd msg )
update ctx msg model =
    case msg of
        NoMsg ->
            ( model, Cmd.none )

        LoadJsonButtonClicked ->
            ( model
            , File.Select.file [] JsonFileSelected
                |> Cmd.map ctx.wrapMsg
            )

        JsonFileSelected file ->
            ( model
            , Task.attempt (handleJsonFileJustRead <| File.name file) (File.toString file)
                |> Cmd.map ctx.wrapMsg
            )

        LoadedJson filename fileContent ->
            case checkJsonMetadataType fileContent of
                Err error ->
                    ( { initialModel | error = Just <| "Unrecognized JSON LD metadata: " ++ error }
                    , Cmd.none
                    )

                Ok govMetadataFile ->
                    ( { model | fileContent = Just { raw = fileContent, name = filename, decoded = govMetadataFile } }
                    , Cmd.none
                    )

        ConvertToPdfButtonClicked rawFileContent ->
            ( model
            , Api.defaultApiProvider.convertToPdf rawFileContent GotPdfFile
                |> Cmd.map ctx.wrapMsg
            )

        GotPdfFile result ->
            case result of
                Err httpError ->
                    ( { model | error = Just <| "An error happened when trying to convert the JSON LD to a PDF: " ++ Debug.toString httpError }
                    , Cmd.none
                    )

                Ok elmBytes ->
                    ( { model | pdfBytes = Just elmBytes, error = Nothing }
                    , File.Download.bytes "metadata.pdf" "application/pdf" elmBytes
                    )


handleJsonFileJustRead : String -> Result x String -> Msg
handleJsonFileJustRead filename result =
    case result of
        Err _ ->
            NoMsg

        Ok jsonFileContent ->
            LoadedJson filename jsonFileContent


checkJsonMetadataType : String -> Result String GovMetadataFile
checkJsonMetadataType fileContent =
    -- Attempt to decode the file content into
    -- one of the valid shapes for GovMetadataFile
    let
        -- Decoder for the whole structure where body contains a Rationale
        govMetadataDecoder : Decoder GovMetadataFile
        govMetadataDecoder =
            JD.field "body" decodeRationale
                |> JD.map VoteRationale
    in
    case JD.decodeString govMetadataDecoder fileContent of
        Ok metadata ->
            Ok metadata

        Err error ->
            Err (JD.errorToString error)


decodeRationale : Decoder Rationale
decodeRationale =
    JD.map7 Rationale
        (JD.field "summary" JD.string)
        (JD.field "rationaleStatement" JD.string)
        (JD.maybe (JD.field "precedentDiscussion" JD.string))
        (JD.maybe (JD.field "counterargumentDiscussion" JD.string))
        (JD.maybe (JD.field "conclusion" JD.string))
        (JD.oneOf
            [ JD.field "internalVote" decodeInternalVote
            , JD.succeed Preparation.noInternalVote
            ]
        )
        (JD.oneOf
            [ JD.field "references" (JD.list decodeReference)
            , JD.succeed []
            ]
        )


decodeInternalVote : Decoder InternalVote
decodeInternalVote =
    JD.map4 InternalVote
        (JD.field "constitutional" JD.int)
        (JD.field "unconstitutional" JD.int)
        (JD.field "abstain" JD.int)
        (JD.field "didNotVote" JD.int)


decodeReference : Decoder Reference
decodeReference =
    JD.map3 Reference
        (JD.field "@type" decodeRefType)
        (JD.field "label" JD.string)
        (JD.field "uri" JD.string)


decodeRefType : Decoder ReferenceType
decodeRefType =
    JD.string
        |> JD.andThen
            (\str ->
                case str of
                    "Other" ->
                        JD.succeed OtherRefType

                    "GovernanceMetadata" ->
                        JD.succeed GovernanceMetadataRefType

                    "RelevantArticles" ->
                        JD.succeed RelevantArticlesRefType

                    _ ->
                        JD.fail "Invalid reference type"
            )



-- ###################################################################
-- VIEW
-- ###################################################################


type alias ViewContext msg =
    { wrapMsg : Msg -> msg
    }


view : ViewContext msg -> Model -> Html msg
view ctx model =
    let
        extLink href content =
            Html.a [ HA.href href, HA.target "_blank", HA.rel "noopener noreferrer" ]
                [ text content ]

        fileStatusSection =
            div []
                [ Html.h3 [] [ text "File Status" ]
                , case model.fileContent of
                    Nothing ->
                        Html.p [] [ text "No file loaded yet" ]

                    Just { name, decoded } ->
                        div []
                            [ Html.p [] [ text <| "Loaded file: " ++ name ]
                            , case decoded of
                                VoteRationale rationale ->
                                    Html.p [] [ text <| "Rationale with summary: " ++ rationale.summary ]
                            ]
                ]

        pdfConversionSection =
            case model.fileContent of
                Just { raw, decoded } ->
                    case decoded of
                        VoteRationale _ ->
                            div []
                                [ Html.h3 [] [ text "PDF conversion" ]
                                , Html.button [ onClick <| ConvertToPdfButtonClicked raw ] [ text "Convert to PDF" ]
                                ]

                _ ->
                    text ""
    in
    Html.map ctx.wrapMsg <|
        div []
            [ Html.h2 [] [ text "Generate pretty PDFs for governance metadata" ]
            , Html.p []
                [ text "This page aims to help generate pretty PDFs for different kinds of governance metadata JSON files."
                , text " These are metadata documents following and extending the "
                , extLink "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0100" "CIP-100 standard"
                , text "."
                , text " In particular, it should work for vote rationales, following the "
                , extLink "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0136" "CIP-136 standard"
                , text "."
                ]
            , Html.button [ onClick LoadJsonButtonClicked ] [ text "Load JSONLD file" ]
            , fileStatusSection
            , pdfConversionSection
            , viewError model.error
            ]


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
