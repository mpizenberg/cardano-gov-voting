module Page.Pdf exposing (GovMetadataFile(..), Model, Msg(..), UpdateContext, ViewContext, initialModel, update, view)

{-| This module handles the conversion of governance metadata JSON-LD files to PDF format.
It specifically supports CIP-136 vote rationale documents, with potential for extension
to other governance metadata types in the future.

Key Features

  - Loads and validates JSON-LD files according to CIP standards
  - Converts valid governance metadata to formatted PDF documents
  - Provides immediate download of generated PDFs

-}

import Api
import Bytes as ElmBytes
import File exposing (File)
import File.Download
import File.Select
import Html exposing (Html, div, button, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder)
import Page.Preparation as Preparation exposing (InternalVote, Rationale, Reference, ReferenceType(..))
import Platform.Cmd as Cmd
import Task
import Helper



-- ###################################################################
-- MODEL
-- ###################################################################


{-| The application state, tracking:

  - The loaded JSON file content and its decoded representation
  - The generated PDF bytes (if conversion has occurred)
  - Any error messages that need to be displayed

-}
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


{-| Represents the different types of governance metadata files that can be processed.
Currently only supports CIP-136 Vote Rationales, but designed to be extensible for
future governance metadata types.
-}
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


{-| Handles JSON validation and decoding for different metadata types.
Currently implements validation for CIP-136 vote rationales, with a structure
that allows for easy addition of other metadata types.
-}
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


{-| Decodes a Rationale according to the CIP-136 specification.
Includes required fields (summary, rationaleStatement) and optional fields
(precedentDiscussion, counterargumentDiscussion, conclusion, internalVote, references).
-}
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
            Html.a 
                [ HA.href href
                , HA.target "_blank"
                , HA.rel "noopener noreferrer"
                , HA.class "text-blue-600 hover:text-blue-800 underline"
                ] 
                [ text content ]

        fileStatusSection =
            case model.fileContent of
                Nothing ->
                    Helper.formContainer
                        [ Html.h3 [ HA.class "text-xl font-medium mb-2" ] [ text "File Status" ]
                        , Html.p [ HA.class "text-gray-600 italic" ] [ text "No file loaded yet" ]
                        ]

                Just { name, decoded } ->
                    Helper.formContainer
                        [ Html.h3 [ HA.class "text-xl font-medium mb-2" ] [ text "File Status" ]
                        , Html.div [ HA.class "mb-2" ]
                            [ Html.span [ HA.class "font-medium mr-2" ] [ text "Loaded file:" ]
                            , text name 
                            ]
                        , case decoded of
                            VoteRationale rationale ->
                                Html.div []
                                    [ Html.div [ HA.class "p-4 bg-blue-50 border border-blue-200 rounded-md" ]
                                        [ Html.h4 [ HA.class "font-medium mb-2" ] [ text "Vote Rationale" ]
                                        , Html.div [ HA.class "space-y-1" ]
                                            [ Html.div []
                                                [ Html.span [ HA.class "font-medium mr-2" ] [ text "Summary:" ]
                                                , text (String.left 80 rationale.summary ++ 
                                                    if String.length rationale.summary > 80 then "..." else "")
                                                ]
                                            , if not (List.isEmpty rationale.references) then
                                                Html.div []
                                                    [ Html.span [ HA.class "font-medium mr-2" ] [ text "References:" ]
                                                    , text (String.fromInt (List.length rationale.references) ++ " included")
                                                    ]
                                              else
                                                text ""
                                            ]
                                        ]
                                    ]
                        ]

        pdfConversionSection =
            case model.fileContent of
                Just { raw, decoded } ->
                    case decoded of
                        VoteRationale _ ->
                            Helper.formContainer
                                [ Html.h3 [ HA.class "text-xl font-medium mb-2" ] [ text "PDF Conversion" ]
                                , Html.p [ HA.class "mb-4" ] 
                                    [ text "Convert the loaded JSON-LD metadata file to a nicely formatted PDF document." ]
                                , Html.div [ HA.class "flex items-center" ]
                                    [ Helper.viewButton "Generate PDF" (ConvertToPdfButtonClicked raw)
                                    , if model.pdfBytes /= Nothing then
                                        Html.span [ HA.class "ml-4 text-green-600" ] [ text "✓ PDF generated and downloaded" ]
                                      else
                                        text ""
                                    ]
                                ]

                _ ->
                    text ""
    in
    Html.map ctx.wrapMsg <|
        div [ HA.class "container mx-auto" ]
            [ Html.h2 [ HA.class "text-3xl font-medium my-4" ] [ text "Generate Pretty PDFs for Governance Metadata" ]
            , Helper.formContainer
                [ Html.p [ HA.class "mb-4" ]
                    [ text "This page helps you generate well-formatted PDF documents from governance metadata JSON files. "
                    , text "It supports metadata documents following the "
                    , extLink "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0100" "CIP-100 standard"
                    , text ", particularly vote rationales that follow the "
                    , extLink "https://github.com/cardano-foundation/CIPs/tree/master/CIP-0136" "CIP-136 standard"
                    , text "."
                    ]
                , Html.div [ HA.class "mt-4 mb-2" ] 
                    [ Helper.viewButton "Load JSON-LD File" LoadJsonButtonClicked ]
                ]
            , fileStatusSection
            , pdfConversionSection
            , case model.error of
                Nothing ->
                    text ""
                    
                Just err ->
                    Html.div [ HA.class "mt-4 p-4 bg-red-50 border border-red-200 rounded-md" ]
                        [ Html.p [ HA.class "text-red-600 font-medium mb-2" ] [ text "Error:" ]
                        , Html.pre [ HA.class "text-sm whitespace-pre-wrap bg-white p-2 border border-red-100 rounded" ] 
                            [ text err ]
                        ]
            ]

viewError : Maybe String -> Html msg
viewError error =
    case error of
        Nothing ->
            text ""

        Just err ->
            Html.div [ HA.class "mt-4 p-4 bg-red-50 border border-red-200 rounded-md" ]
                [ Html.p [ HA.class "text-red-600 font-medium mb-2" ] [ text "Error:" ]
                , Html.pre [ HA.class "text-sm whitespace-pre-wrap bg-white p-2 border border-red-100 rounded" ] 
                    [ text err ]
                ]