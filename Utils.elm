module Utils exposing (shuffleList)

import List
import Random

-- Shuffle implementation borrowed from Tyler Jennings:
-- http://tylerscode.com/2016/06/list-shuffle-elm/
shuffleList: List a -> List a
shuffleList inList =
    let
        randList = Random.step (Random.list (List.length inList) (Random.int 1 100)) (Random.initialSeed 1706104) |> fst
    in
        List.map2 (,) randList inList |> List.sortBy fst |> List.unzip |> snd
