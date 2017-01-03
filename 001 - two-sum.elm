import Html exposing (div, text)
import Dict

nums = [2, 3, 4]


target = 7


main =
    div []
        [ div [] [ text <| (toString nums) ++ " " ++ (toString target) ]
        , div [] [ text (toString (twoSum nums target)) ]
        ]


twoSum : List Int -> Int -> Maybe ( Int, Int )
twoSum nums target =
    (List.foldl
        (\num state ->
            case state.result of
                Just result ->
                    { state | i = state.i + 1 }
                Nothing ->
                    if Dict.member (target - num) state.toIndex then
                        let
                            firstIndex =
                                Maybe.withDefault -1
                                    (Dict.get (target - num) state.toIndex)
                        in
                            { state
                                | result = Just ( firstIndex, state.i )
                                , i = state.i + 1
                                }
                    else
                        { state
                            | toIndex = Dict.insert num state.i state.toIndex
                            , i = state.i + 1
                            }
        )
        { toIndex = Dict.empty
        , result = Nothing
        , i = 0
        }
        nums
    ).result
