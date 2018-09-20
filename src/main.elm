module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Browser


main =
    Browser.element
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
    { table : List (List Cell)
    , rowNumber : Int
    , columnNumber : Int
    }

type alias Cell =
  { status : Status
  , visalization : Visualization
  }

type Status
  = Number Int
  | Mine

type Visualization
  = Hidden
  | Shown
  | Flagged

type Msg
    = ShowMine Int Int
    | Initialize


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowMine i j->
      (model, Cmd.none)

    Initialize ->
      (model, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ ul []
      [ li [] [text ("row Number: " ++ (String.fromInt model.rowNumber))]
      , li [] [text ("column Number: " ++ (String.fromInt model.columnNumber))]
      ]
    , showTable model
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : () -> (Model, Cmd Msg)
init _ = 
    ( initializeModel 10 10
    , Cmd.none
    )

-- MY FUNCTIONS

initializeModel : Int -> Int -> Model
initializeModel nRows nColumn =
  let
    table = List.repeat nRows (List.repeat nColumn (Cell Mine Hidden))
    -- TODO inizializzala
  in
    Model table nRows nColumn

showTable : Model -> Html Msg
showTable model =
  table [class "myTable"] (List.map showLine model.table)

showLine : List Cell -> Html Msg
showLine list =
  tr [] (List.map showCell list)

showCell : Cell -> Html Msg
showCell cell =
  td []
    [ 
      case cell.visalization of
          Hidden ->
            text ""              
      
          Flagged ->
            text "F"

          Shown ->
            case cell.status of
              Number 0 ->
                text ""

              Number n ->
                text (String.fromInt n)
                  
              Mine ->
                text "M" 
    ]