module Main exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type Msg
    = Add String
    | InputText String
    | ToggleDone Int


type alias Todo =
    { id : Int
    , title : String
    , done : Bool
    }


type alias TodoList =
    { todos : List Todo
    , title : String
    }


init : TodoList
init =
    { todos = []
    , title = ""
    }



-- UPDATE


update : Msg -> TodoList -> TodoList
update msg list =
    case msg of
        Add string ->
            { list | todos = Todo (List.length list.todos) string True :: list.todos, title = "" }

        InputText string ->
            { list | title = string }

        ToggleDone id ->
            { list | todos = List.map (toggleDone id) list.todos }


toggleDone id todo =
    if todo.id == id then
        { todo | done = not todo.done }

    else
        todo



-- VIEW


view : TodoList -> Html Msg
view list =
    div
        [ style "text-align" "center"
        , style "font-family" ""
        ]
        [ input
            [ placeholder "Title"
            , value list.title
            , onInput InputText
            ]
            []
        , button [ onClick (Add list.title) ] [ text "Add" ]
        , div []
            [ cards list.todos
            ]
        ]


cards : List Todo -> Html Msg
cards todos =
    div [] (List.map card todos)


card : Todo -> Html Msg
card todo =
    div [ style "margin" "5px" ]
        [ span
            [ if todo.done == True then
                style "text-decoration" "none"

              else
                style "text-decoration" "line-through"
            ]
            [ text ("" ++ String.fromInt todo.id ++ ": ")
            , text todo.title
            ]
        , button [ onClick (ToggleDone todo.id) ]
            [ if todo.done == True then
                text "x"

              else
                text "o"
            ]
        ]
