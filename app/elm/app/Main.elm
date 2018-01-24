module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck)
import Json.Decode as Json


type FilterState
    = All
    | Active
    | Completed


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    , editing : Bool
    }


type alias Model =
    { todos : List Todo
    , todo : Todo
    , filter : FilterState
    , nextId : Int
    }


initialModel : Model
initialModel =
    { todos = []
    , todo = newTodo
    , filter = All
    , nextId = 1
    }


newTodo : Todo
newTodo =
    { id = 0
    , title = ""
    , completed = False
    , editing = False
    }



-- Update


type Msg
    = UpdateTitle String
    | Add
    | Toggle Todo
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
                , todo = { newTodo | id = model.nextId }
                , nextId = model.nextId + 1
            }

        Toggle todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.id == todo.id then
                        { thisTodo | completed = not todo.completed }
                    else
                        thisTodo
            in
                { model | todos = List.map updateTodo model.todos }

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
            [ input
                [ class "toggle"
                , type_ "checkbox"
                , checked todo.completed
                , onCheck (\_ -> Toggle todo)
                ]
                []
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
