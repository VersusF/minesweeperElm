module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Debug
import Random
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
  , mineNumber : Int
  }

type alias Cell =
  { status : Status
  , visalization : Visualization
  }

type Status
  = Number Int
  | Mine
  | Empty

type Visualization
  = Hidden
  | Shown
  | Flagged

type Msg
  = ShowMine Int Int
  | Initialize
  | PopolateTable (List (Int, Int))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowMine i j ->
      (model, Cmd.none)

    Initialize ->
      (model, Cmd.none)

    PopolateTable list ->
      let
        table = createTableWithMines model.rowNumber model.rowNumber list
        _ = Debug.log "Lista" list
      in
        ({model | table = table}, Cmd.none)


view : Model -> Html Msg
view model =
  div []
    [ ul []
      [ li [] [text ("Row number: " ++ (String.fromInt model.rowNumber))]
      , li [] [text ("Column number: " ++ (String.fromInt model.columnNumber))]
      , li [] [text ("Mines number: " ++ (String.fromInt model.mineNumber))]
      ]
    , showTable model
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


init : () -> (Model, Cmd Msg)
init _ = 
  let
    model = initializeModel 10 10 20
  in
    ( model
    , Random.generate PopolateTable (minesLocation model)
    )

-- MY FUNCTIONS

createTableWithMines : Int -> Int -> List (Int, Int) -> List (List Cell)
createTableWithMines nRow nCol mines =
  let
    emptyTable = List.repeat nRow (List.repeat nCol (Cell Empty Hidden))
  in
    List.foldl addMine emptyTable mines

addMine : (Int, Int) -> List (List Cell) -> List (List Cell)
addMine (x, y) table =
  table

minesLocation : Model -> Random.Generator (List (Int, Int))
minesLocation model =
-- TODO prendi i numeri dal model
  let
    rowMax = model.rowNumber - 1
    colMax = model.columnNumber - 1
  in
    Random.list model.mineNumber (Random.pair (Random.int 0 rowMax) (Random.int 0 colMax))

initializeModel : Int -> Int -> Int -> Model
initializeModel nRows nColumn nMines =
  Model [] nRows nColumn nMines

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

            Empty ->
              text ""
    ]
