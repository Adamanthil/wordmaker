module App exposing (..)

import String
import List
import Dict exposing(Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App

import Utils exposing (..)

main =
  App.beginnerProgram { model = initialModel "test sentence", view = view, update = update }


-- MODEL

type alias Letter = Char

type alias Word = List Letter
type alias LetterTile =
  { letter: Letter }

type alias Model =
  { sentence: List Word
  , positionedTiles: Dict Int LetterTile
  , bankedTiles: Dict Int LetterTile
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
  }


-- UPDATE

type Msg = NoOp

update : Msg -> Model -> Model
update msg model = model

--type Msg = Increment | Decrement

--update : Msg -> Model -> Model
--update msg model =
--  case msg of
--    Increment ->
--      model + 1

--    Decrement ->
--      model - 1


-- VIEW

letterSlot : Dict Int LetterTile -> Int -> Letter -> Html Msg
letterSlot positionedTiles index letter =
  case Dict.get index positionedTiles of
    Just tile ->
      renderTile index tile
    Nothing ->
      div [class "empty-slot"] []

wordSlot : Dict Int LetterTile -> Int -> Word -> Html Msg
wordSlot positionedTiles index word =
  div [class "word"] (List.map (letterSlot positionedTiles index) word)

renderTile : Int -> LetterTile -> Html Msg
renderTile index tile =
  div [class "tile"] [text (String.fromChar (.letter tile))]

view : Model -> Html Msg
view model =
  div
    []
    [ div [] (List.indexedMap (wordSlot model.positionedTiles) model.sentence)
    , div [class "tile-bank"] (Dict.values (Dict.map renderTile model.bankedTiles))
    ]
