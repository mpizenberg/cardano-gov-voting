module Page.Disclaimer exposing (Model, Msg, initialModel, update, view)

{-| This module provides a legal disclaimer page with important information about
the Cardano Signature Verification tool.

The disclaimer outlines the legal terms of use including:
- Services provided "as is"
- No liability for errors, omissions, or damages
- User responsibility to conduct due diligence

This page follows the structure of other pages in the application for consistency.
-}

import Html exposing (Html, div, h2, h3, p, text)
import Html.Attributes as HA
import Helper


-- ###################################################################
-- MODEL
-- ###################################################################


{-| Simple model for the disclaimer page
-}
type alias Model =
    {}


initialModel : Model
initialModel =
    {}


-- ###################################################################
-- UPDATE
-- ###################################################################


{-| No messages are needed for this static page
-}
type Msg
    = NoOp


{-| Update function that maintains the current state
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


-- ###################################################################
-- VIEW
-- ###################################################################


{-| Renders the disclaimer with the legal text
-}
view : Model -> Html Msg
view _ =
    div [ HA.style "max-width" "1024px", HA.style "margin" "0 auto", HA.style "padding" "0 1rem" ]
        [ h2 [ 
            HA.style "font-size" "1.875rem", 
            HA.style "font-weight" "500",
            HA.style "margin-top" "1rem",
            HA.style "margin-bottom" "1.5rem"
          ] 
          [ text "Disclaimer" ]
        , Helper.formContainer
            [ h3 [ 
                HA.style "font-size" "1.25rem", 
                HA.style "font-weight" "500",
                HA.style "margin-bottom" "1rem" 
              ] 
              [ text "Cardano Signature Verification" ]
            , p [ HA.style "margin-bottom" "1rem" ] 
                [ text "By using our signature verification tool, you agree to the following:" ]
            , div []
              [ p [ HA.style "margin-bottom" "1rem" ] 
                  [ Html.strong [ HA.style "font-weight" "500" ] [ text "Services Provided \"As Is\": " ]
                  , text "To the maximum extent permitted by applicable law, our services are provided on an \"as is\" and \"as available\" basis. We expressly disclaim all warranties of any kind, whether express or implied, including any warranties of merchantability, fitness for a particular purpose, title, and non-infringement. This includes the information, content, and materials contained within our services." 
                  ]
              , p [ HA.style "margin-bottom" "1rem" ] 
                  [ Html.strong [ HA.style "font-weight" "500" ] [ text "No Liability for Errors or Omissions: " ]
                  , text "We are not responsible for any errors or omissions in the information provided by the tool, nor do we provide a guarantee of completeness, accuracy, timeliness, or of the results obtained from its use." 
                  ]
              , p [ HA.style "margin-bottom" "1rem" ] 
                  [ Html.strong [ HA.style "font-weight" "500" ] [ text "User Responsibility: " ]
                  , text "It is your responsibility to conduct additional due diligence as needed. We are not liable for any decisions or actions taken in reliance on the information provided by our service." 
                  ]
              , p [ HA.style "margin-bottom" "1rem" ] 
                  [ Html.strong [ HA.style "font-weight" "500" ] [ text "No Liability for Damages: " ]
                  , text "We will not be liable for any consequential, special, or similar damages, even if advised of the possibility of such damages." 
                  ]
              ]
            , p [ 
                HA.style "font-weight" "500", 
                HA.style "font-style" "italic",
                HA.style "color" "#4b5563"
              ] 
              [ text "By using our services, you acknowledge and agree to the terms of this disclaimer." ]
            ]
        ]