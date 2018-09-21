module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug
import Array exposing (Array)
import Random
import Browser
import Json.Decode as Json


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
  , visualization : Visualization
  }

type PlayerStatus
  = Playing
  | Win
  | Lost

type Status
  = Number Int
  | Mine
  | Empty

type Visualization
  = Hidden
  | Shown
  | Flagged

type Msg
  = ShowCell Int Int
  | FlagCell Int Int
  | Initialize
  | PopolateTable (List (Int, Int))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShowCell i j ->
      let
        newTable = showCell model.table i j
      in
        ( { model 
            | table = newTable
            , playerStatus = 
              case getCell model.table i j of
                Just cell -> 
                  if isMine cell then 
                    Lost 
                  else if isTableComplete newTable then
                    Win
                  else
                    Playing

                Nothing -> Playing
            }
        , Cmd.none
        )

    FlagCell i j ->
      ({model | table = flagCell model.table i j}, Cmd.none)

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
  case model.playerStatus of
    Playing -> viewPlayingBoard model
        
    Win -> viewWinningBoard

    Lost -> viewLosingBoard

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

isTableComplete : Table -> Bool
isTableComplete table =
  -- The table is complete if all numbers are shown
  -- aka there are no hidden cell -> no hidden rows
  0 == Array.length (Array.filter isRowHidden table)

isRowHidden : Array Cell -> Bool
isRowHidden row =
  -- A row is hidden if there is at least one hidden cell
  0 /= Array.length (Array.filter isCellHidden row)

isCellHidden : Cell -> Bool
isCellHidden cell =
  -- A cell is hidden only if it's a hidden number
  case cell.status of
    Number _ -> cell.visualization == Hidden
    _ -> False

getCell : Table -> Int -> Int -> Maybe Cell
getCell table i j =
  case Array.get i table of
    Just row ->
      Array.get j row
    
    Nothing ->
      Nothing


viewPlayingBoard : Model -> Html Msg
viewPlayingBoard model =
  div []
    [ ul []
      [ li [] [text ("Row number: " ++ (String.fromInt model.rowNumber))]
      , li [] [text ("Column number: " ++ (String.fromInt model.columnNumber))]
      , li [] [text ("Mines number: " ++ (String.fromInt model.mineNumber))]
      ]
    , vieTable model
    ]

viewWinningBoard : Html Msg
viewWinningBoard =
  div []
    [h1 [class "title"] [text "Congratulations! You win!"]]

viewLosingBoard : Html Msg
viewLosingBoard =
  div []
    [h1 [class "title"] [text "Game Over"]]


flagCell : Table -> Int -> Int -> Table
flagCell table i j =
  case Array.get i table of
    Just row ->
      case Array.get j row of
        Just cell ->
          let
            newRow = Array.set j {cell | visualization = Flagged} row
          in
            Array.set i newRow table
          
        Nothing ->
          table

    Nothing ->
      table

showCell : Table -> Int -> Int -> Table
showCell table i j =
  case Array.get i table of
    Just row ->
      case Array.get j row of
        Just cell ->
          let
            newRow = Array.set j {cell | visualization = Shown} row
            newTable = Array.set i newRow table
          in
            case cell.status of
              Number 0 ->
                showNearCells newTable i j
          
              _ -> 
                newTable
          
        Nothing ->
          table

    Nothing ->
      table

showNearCells : Table -> Int -> Int -> Table
showNearCells table i j =
  table

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
isMine cell =
  case cell.status of
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

vieTable : Model -> Html Msg
vieTable model =
  table [class "myTable"] (List.indexedMap viewRow (Array.toList model.table))

viewRow : Int -> Array Cell -> Html Msg
viewRow i list =
  tr [] (List.indexedMap (viewCell i) (Array.toList list))

viewCell : Int -> Int -> Cell -> Html Msg
viewCell i j cell =
  case cell.visualization of
    Hidden ->
      td 
        [ class "hiddenCell"
        , onRightClick (FlagCell i j)
        , onClick (ShowCell i j)
        ]
        [text ""]     

    Flagged ->
      td
        [ class "flaggedCell"
        , onClick (ShowCell i j)
        ]
        [text "F"]
      

    Shown ->
      case cell.status of
        Number 0 ->
          td [class "number0cell"] [text ""]

        Number n ->
          td [class "numberCell"] [text (String.fromInt n)]
            
        Mine ->
          td [class "mineCell"] [text "M"] 

        Empty ->
          td [] []


onRightClick message =
  custom
    "contextmenu"
    ( Json.succeed 
      { message = message
      , preventDefault = True
      , stopPropagation = True
      }
    )