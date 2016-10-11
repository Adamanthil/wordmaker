module App exposing (..)

import String
import List
import Dict exposing(Dict)
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)


main =
  App.beginnerProgram { model = initialModel "Test Sentence", view = view, update = update }


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

letterSlot : Letter -> Html Msg
letterSlot letter =
  div [] [text (String.fromChar letter)]

wordSlot : Word -> Html Msg
wordSlot word =
  div [] (List.map letterSlot word)

renderTile : Int -> LetterTile -> Html Msg
renderTile index tile =
  div [] [text (String.fromChar (.letter tile))]

view : Model -> Html Msg
view model =
  div
    []
    [ div [] (List.map wordSlot model.sentence)
    , div [] (Dict.values (Dict.map renderTile model.bankedTiles))
    ]
