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
  , selectedBankedTile: Maybe Int
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
  , selectedBankedTile = Nothing
  , selectedPositionedTile = Nothing
  }


-- UPDATE

type Msg
  = ClickBankTile Int
  | ClickPositionedTile Int
  | ClearSelected
  | ClickSlotWithBanked Int Int
  | ClickSlotWithPositioned Int Int
  | ClickBankWithPositioned Int
  | ClickBankWithBanked Int
  | NoOp

update : Msg -> Model -> Model
update msg model =
  case msg of
    ClickBankTile index ->
      { model |
        selectedBankedTile = Just index,
        selectedPositionedTile = Nothing
      }

    ClickPositionedTile index ->
      { model |
        selectedBankedTile = Nothing,
        selectedPositionedTile = Just index
      }

    ClearSelected ->
      { model |
        selectedBankedTile = Nothing,
        selectedPositionedTile = Nothing
      }

    ClickSlotWithBanked index fromIndex ->
      case Dict.get fromIndex model.bankedTiles of
        Just tile ->
          { model |
            bankedTiles = model.bankedTiles
              |> Dict.remove fromIndex
              |> Dict.values
              |> List.indexedMap (,)
              |> Dict.fromList
          , positionedTiles = model.positionedTiles |> Dict.insert index tile
          , selectedBankedTile = Nothing
          }
        Nothing ->
          model

    ClickSlotWithPositioned index fromIndex ->
      case Dict.get fromIndex model.positionedTiles of
        Just tile ->
          { model |
            positionedTiles = model.positionedTiles
              |> Dict.insert index tile
              |> Dict.remove fromIndex
          , selectedPositionedTile = Nothing
          }
        Nothing ->
          model

    ClickBankWithPositioned fromIndex ->
      case Dict.get fromIndex model.positionedTiles of
        Just tile ->
          { model |
            bankedTiles = model.bankedTiles |> Dict.insert (Dict.size model.bankedTiles) tile
          , positionedTiles = model.positionedTiles |> Dict.remove fromIndex
          , selectedPositionedTile = Nothing
          }
        Nothing ->
          model

    ClickBankWithBanked fromIndex ->
      case Dict.get fromIndex model.bankedTiles of
        Just tile ->
          { model |
            bankedTiles = model.bankedTiles
              |> Dict.remove fromIndex
              |> Dict.insert (Dict.size model.bankedTiles) tile
              |> Dict.values
              |> List.indexedMap (,)
              |> Dict.fromList
          , selectedBankedTile = Nothing
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
    , div
        [ classList [ ("tile-bank", True), ("tile-selected", (isTileSelected model)) ]
        , onClick
          ( if (isPositionedSelected model) then
              (ClickBankWithPositioned (getSelectedPositionedIndex model))
            else if (isBankedSelected model) then
              (ClickBankWithBanked (getSelectedBankedIndex model))
            else
              NoOp
          )
        ]
        (Dict.values (Dict.map (viewTile Banked False model.selectedBankedTile) model.bankedTiles))
    , div [] [text ("positioned index: " ++ (toString model.selectedPositionedTile))]
    , div [] [text ("banked index: " ++ (toString model.selectedBankedTile))]
    ]

viewLetterSlot : Model -> Int -> Letter -> Html Msg
viewLetterSlot model index letter =
  let
    (isBankedSelected, selectedBankIndex) = selectedBanked model
    (isPositionedSelected, selectedPositionedIndex) = selectedPositioned model
  in
    case Dict.get index model.positionedTiles of
      Just tile ->
        viewTile Positioned (letter == tile.letter) model.selectedPositionedTile index tile
      Nothing ->
        div
          [ class "empty-slot"
          , onClick
            ( if isBankedSelected then
                (ClickSlotWithBanked index selectedBankIndex)
              else if isPositionedSelected then
                (ClickSlotWithPositioned index selectedPositionedIndex)
              else
                NoOp
            )
          ]
          []

viewWordSlot : Model -> Int -> Word -> Html Msg
viewWordSlot model wordIndex word =
  div [ class "word" ] (List.indexedMap (\index letter -> viewLetterSlot model (index + (calcWordStartIndex model.sentence wordIndex)) letter) word)

viewTile : TileType -> Bool -> Maybe Int -> Int -> LetterTile -> Html Msg
viewTile tileType correct selectedIndex index tile =
  let
    selected = isSelected selectedIndex index
  in
    div
      [ classList [ ("tile", True), ("selected", selected), ("correct", correct), ("incorrect", not correct && tileType == Positioned) ]
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

isTileSelected : Model -> Bool
isTileSelected model =
  let
    (isBankedSelected, selectedBankIndex) = selectedBanked model
    (isPositionedSelected, selectedPositionedIndex) = selectedPositioned model
  in
    isBankedSelected || isPositionedSelected

isPositionedSelected : Model -> Bool
isPositionedSelected model =
  let
    (isPositionedSelected, selectedPositionedIndex) = selectedPositioned model
  in
    isPositionedSelected

getSelectedPositionedIndex : Model -> Int
getSelectedPositionedIndex model =
  let
    (isPositionedSelected, selectedPositionedIndex) = selectedPositioned model
  in
    selectedPositionedIndex

isBankedSelected : Model -> Bool
isBankedSelected model =
  let
    (isBankedSelected, selectedBankedIndex) = selectedBanked model
  in
    isBankedSelected

getSelectedBankedIndex : Model -> Int
getSelectedBankedIndex model =
  let
    (isBankedSelected, selectedBankedIndex) = selectedBanked model
  in
    selectedBankedIndex

selectedBanked : Model -> (Bool, Int)
selectedBanked model =
  case model.selectedBankedTile of
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
