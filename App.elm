module App exposing (..)

import String
import List
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)


main =
  App.beginnerProgram { model = initialModel "Test Sentence", view = view, update = update }


-- MODEL

type alias Letter = Char
type alias Word = List Letter
type alias LetterTile =
  { letter: Letter
  , position: Int
  , inSentence: Bool
  }

type alias Model =
  { sentence: List Word
  , positionedLetters: List LetterTile
  }

makeLetterTile : Int -> Letter -> LetterTile
makeLetterTile index letter =
  { letter = letter
  , position = index
  , inSentence = False
  }

initialModel : String -> Model
initialModel sentence =
  { sentence = sentence |> String.words |> List.map String.toList
  , positionedLetters = sentence |> String.toList |> List.indexedMap makeLetterTile
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

view : Model -> Html Msg
view model =
  div [] (List.map wordSlot model.sentence)
