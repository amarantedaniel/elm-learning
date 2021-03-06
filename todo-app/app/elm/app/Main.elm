module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
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


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- Update


type Msg
    = UpdateTitle String
    | Add
    | Toggle Todo
    | Delete Todo
    | Filter FilterState
    | ClearCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTitle str ->
            let
                todo =
                    model.todo

                updatedTodo =
                    { todo | title = str }
            in
                { model | todo = updatedTodo } ! []

        Add ->
            { model
                | todos = model.todo :: model.todos
                , todo = { newTodo | id = model.nextId }
                , nextId = model.nextId + 1
            }
                ! []

        Toggle todo ->
            let
                updateTodo thisTodo =
                    if thisTodo.id == todo.id then
                        { thisTodo | completed = not todo.completed }
                    else
                        thisTodo
            in
                { model | todos = List.map updateTodo model.todos } ! []

        Delete todo ->
            { model | todos = List.filter (\mappedTodo -> todo.id /= mappedTodo.id) model.todos } ! []

        Filter filterState ->
            { model | filter = filterState } ! []

        ClearCompleted ->
            { model | todos = List.filter (\todo -> not todo.completed) model.todos } ! []


filteredTodos : Model -> List Todo
filteredTodos model =
    let
        matchesFilter =
            case model.filter of
                All ->
                    (\_ -> True)

                Active ->
                    (\todo -> todo.completed == False)

                Completed ->
                    (\todo -> todo.completed == True)
    in
        List.filter matchesFilter model.todos


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
                (List.map todoView (filteredTodos model))
            ]
        , footer [ class "footer" ]
            [ span [ class "todo-count" ]
                [ strong [] [ text (toString (List.length (List.filter (\todo -> todo.completed == False) model.todos))) ]
                , text " items left"
                ]
            , ul [ class "filters" ]
                [ filterItemView model All
                , filterItemView model Active
                , filterItemView model Completed
                ]
            , button
                [ class "clear-completed"
                , onClick ClearCompleted
                ]
                [ text "Clear completed" ]
            ]
        ]


filterItemView : Model -> FilterState -> Html Msg
filterItemView model filterState =
    li []
        [ a
            [ classList [ ( "selected", (model.filter == filterState) ) ]
            , href "#"
            , onClick (Filter filterState)
            ]
            [ text (toString filterState) ]
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
            , button
                [ class "destroy"
                , onClick (Delete todo)
                ]
                []
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
