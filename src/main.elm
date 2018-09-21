module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Debug
import Array exposing (Array)
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
  { table : Table
  , rowNumber : Int
  , columnNumber : Int
  , mineNumber : Int
  , playerStatus : PlayerStatus
  }

type alias Table = Array (Array Cell)

type alias Cell =
  { status : Status
  , visalization : Visualization
  }

type PlayerStatus
  = Playing
  | Win
  | Loose

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
        tableMines = createTableWithMines model.rowNumber model.rowNumber list
        tableNumbers = createTableWithNumbers tableMines
      in
        ({model | table = tableNumbers}, Cmd.none)


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

createTableWithNumbers : Table -> Table
createTableWithNumbers table =
  Array.indexedMap (createRowWithNumbers table) table

createRowWithNumbers : Table -> Int -> Array Cell -> Array Cell
createRowWithNumbers table i row =
  Array.indexedMap (createCellWithNumber table i) row

createCellWithNumber : Table -> Int -> Int -> Cell -> Cell
createCellWithNumber table i j cell =
  case cell.status of
    Empty ->
      let
        nearMines = List.length (List.filter isMine (getNearCells table i j))
      in
        Cell (Number nearMines) Hidden

    _ ->
      cell

getNearCells : Table -> Int -> Int -> List Cell
getNearCells table i j =
  let
    iLower = if i == 0 then 0 else i - 1
    jLower = if j == 0 then 0 else j - 1
    rows = Array.slice iLower (i+2) table
    smallTable = Array.map (Array.slice jLower (j+2)) rows
    rowsList = Array.map Array.toList smallTable
  in
    Array.foldl List.append [] rowsList


isMine : Cell -> Bool
isMine cella =
  case cella.status of
      Mine -> True
      _ -> False          

createTableWithMines : Int -> Int -> List (Int, Int) -> Table
createTableWithMines nRow nCol mines =
  let
    emptyTable = Array.repeat nRow (Array.repeat nCol (Cell Empty Hidden))
  in
    List.foldl addMine emptyTable mines

addMine : (Int, Int) -> Table -> Table
addMine (i, j) table =
  case Array.get i table of
    Just row ->
      case Array.get j row of
        Just cell ->
          case cell.status of
            Empty ->
              let
                -- TODO they should be hidden
                newRow = Array.set j (Cell Mine Hidden) row
              in
                Array.set i newRow table

            Mine ->
              let
                newJ = modBy (Array.length row) (j + 1)
              in
                case newJ of
                  0 ->
                    let
                      newI = modBy (Array.length table) (i + 1)
                    in
                      addMine (newI, newJ) table 
              
                  _ ->
                    addMine (i, newJ) table

            _ ->
              table
          
        Nothing ->
          table

    Nothing ->
      table

minesLocation : Model -> Random.Generator (List (Int, Int))
minesLocation model =
  let
    rowMax = model.rowNumber - 1
    colMax = model.columnNumber - 1
  in
    Random.list model.mineNumber (Random.pair (Random.int 0 rowMax) (Random.int 0 colMax))

initializeModel : Int -> Int -> Int -> Model
initializeModel nRows nColumn nMines =
  Model Array.empty nRows nColumn nMines Playing

showTable : Model -> Html Msg
showTable model =
  table [class "myTable"] (List.map showLine (Array.toList model.table))

showLine : Array Cell -> Html Msg
showLine list =
  tr [] (List.map showCell (Array.toList list))

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
