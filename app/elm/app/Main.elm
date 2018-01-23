module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseOver, onClick)


-- MODEL


type alias Model =
    { rightButton : String
    , leftButton : String
    , response : String
    }


model : Model
model =
    Model "YES" "NO" ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [ style [ ( "color", "#CB8F36" ) ] ] [ text "IS FERNANDO HECK GAY?" ]
        , div [ class "daniwrapper" ]
            [ div [ class "danibutton" ] [ button [ class "danibutton", onMouseOver Left, onClick (Click model.leftButton) ] [ text model.leftButton ] ]
            , div [ class "danibutton" ] [ button [ class "danibutton", onMouseOver Right, onClick (Click model.rightButton) ] [ text model.rightButton ] ]
            ]
        , h1 [ class "danitext" ] [ text model.response ]
        ]



-- UPDATE


type Msg
    = Left
    | Right
    | Click String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Left ->
            { model | leftButton = "YES", rightButton = "NO" }

        Right ->
            { model | leftButton = "NO", rightButton = "YES" }

        Click text ->
            { model | response = text }



-- PROGRAM


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }
