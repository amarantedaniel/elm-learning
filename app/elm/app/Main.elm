module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput)
import Json.Decode as Json


type FilterState
    = All
    | Active
    | Completed


type alias Todo =
    { title : String
    , completed : Bool
    , editing : Bool
    }


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    }


initialModel : Model
initialModel =
    { todos = []
    , todo =
        { title = ""
        , completed = False
        , editing = False
        }
    , filter = All
    }


newTodo : Todo
newTodo =
    { title = ""
    , completed = False
    , editing = False
    }



-- Update


type Msg
    = UpdateTitle String
    | Add
    | Complete Todo
    | Delete Todo
    | Filter FilterState


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTitle str ->
            let
                todo =
                    model.todo

                updatedTodo =
                    { todo | title = str }
            in
                { model | todo = updatedTodo }

        Add ->
            { model
                | todos = model.todo :: model.todos
                , todo = newTodo
            }

        Complete todo ->
            model

        Delete todo ->
            model

        Filter filterState ->
            model


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not the right keycode"
    in
        on "keydown" (keyCode |> Json.andThen isEnter)



-- View


view : Model -> Html Msg
view model =
    section [ class "todoapp" ]
        [ header [ class "header" ]
            [ h1 [] [ text "todos" ]
            , input
                [ class "new-todo"
                , placeholder "What needs to be done?"
                , value model.todo.title
                , autofocus True
                , onEnter Add
                , onInput UpdateTitle
                ]
                []
            ]
        , section [ class "main" ]
            [ ul [ class "todo-list" ]
                (List.map todoView model.todos)
            ]
        ]


todoView : Todo -> Html Msg
todoView todo =
    li [ classList [ ( "completed", todo.completed ) ] ]
        [ div [ class "view" ]
            [ input [ class "toggle", type_ "checkbox", checked todo.completed ] []
            , label [] [ text todo.title ]
            , button [ class "destroy" ] []
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
