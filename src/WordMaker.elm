module WordMaker exposing (..)

import String
import List
import Dict exposing(Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App

import Utils exposing (..)

main =
  App.beginnerProgram { model = initialModel "a surprise awaits you in narnia", view = view, update = update }


-- MODEL

type alias Letter = Char

type alias Word = List Letter
type alias LetterTile =
  { letter: Letter }

type alias Model =
  { sentence: List Word
  , positionedTiles: Dict Int LetterTile
  , bankedTiles: Dict Int LetterTile
  , selectedBankTile: Maybe Int
  , selectedPositionedTile: Maybe Int
  }

makeLetterTile : Letter -> LetterTile
makeLetterTile letter =
  { letter = letter }

initialModel : String -> Model
initialModel sentence =
  { sentence = sentence |> String.words |> List.map String.toList
  , positionedTiles = Dict.empty
  , bankedTiles = sentence
      |> String.filter (\letter -> letter /= ' ')
      |> String.toList
      |> Utils.shuffleList
      |> List.map makeLetterTile
      |> List.indexedMap (,)
      |> Dict.fromList
  , selectedBankTile = Nothing
  , selectedPositionedTile = Nothing
  }


-- UPDATE

type Msg
  = ClickBankTile Int
  | ClickPositionedTile Int
  | ClearSelected
  | ClickSlotWithBanked Int Int
  | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickBankTile index ->
      { model |
        selectedBankTile = Just index,
        selectedPositionedTile = Nothing
      }

    ClickPositionedTile index ->
      { model |
        selectedBankTile = Nothing,
        selectedPositionedTile = Just index
      }

    ClearSelected ->
      { model |
        selectedBankTile = Nothing,
        selectedPositionedTile = Nothing
      }

    ClickSlotWithBanked index fromIndex ->
      case Dict.get fromIndex model.bankedTiles of
        Just tile ->
          { model |
            bankedTiles = model.bankedTiles |> Dict.remove fromIndex,
            positionedTiles = model.positionedTiles |> Dict.insert index tile
          }
        Nothing ->
          model

    NoOp ->
      model


-- VIEW

type TileType = Banked | Positioned

view : Model -> Html Msg
view model =
  div
    [ id "main" ]
    [ h1 [] [text "Solve the Puzzle"]
    , div [class "puzzle"] (List.indexedMap (viewWordSlot model) model.sentence)
    , h2 [] [text "Letter Bank"]
    , div [class "tile-bank"] (Dict.values (Dict.map (viewTile Banked model.selectedBankTile) model.bankedTiles))
    , div [] [text (toString model.selectedPositionedTile)]
    ]

viewLetterSlot : Model -> Int -> Letter -> Html Msg
viewLetterSlot model index letter =
  let
    (isBankedSelected, selectedIndex) = selectedBanked model
  in
    case Dict.get index model.positionedTiles of
      Just tile ->
        viewTile Positioned model.selectedPositionedTile index tile
      Nothing ->
        div
          [ class "empty-slot"
          , onClick (if isBankedSelected then (ClickSlotWithBanked index selectedIndex) else NoOp)
          ]
          []

viewWordSlot : Model -> Int -> Word -> Html Msg
viewWordSlot model wordIndex word =
  div [ class "word" ] (List.indexedMap (\index letter -> viewLetterSlot model (index + (calcWordStartIndex model.sentence wordIndex)) letter) word)

viewTile : TileType -> Maybe Int -> Int -> LetterTile -> Html Msg
viewTile tileType selectedIndex index tile =
  let
    selected = isSelected selectedIndex index
  in
    div
      [ classList [ ("tile", True), ("selected", selected) ]
      , onClick (if selected then (ClearSelected) else if tileType == Banked then (ClickBankTile index) else (ClickPositionedTile index))
      ]
      [ text (String.fromChar (.letter tile)) ]

isSelected : Maybe Int -> Int -> Bool
isSelected selectedIndex index =
  case selectedIndex of
    Just value ->
      value == index
    Nothing ->
      False

selectedBanked : Model -> (Bool, Int)
selectedBanked model =
  case model.selectedBankTile of
    Just value -> (True, value)
    Nothing -> (False, -1)

selectedPositioned : Model -> (Bool, Int)
selectedPositioned model =
  case model.selectedPositionedTile of
    Just value -> (True, value)
    Nothing -> (False, -1)

calcWordStartIndex : List Word -> Int -> Int
calcWordStartIndex sentence wordIndex =
  sentence
    |> List.take wordIndex
    |> List.map (List.length)
    |> List.foldr (+) 0
