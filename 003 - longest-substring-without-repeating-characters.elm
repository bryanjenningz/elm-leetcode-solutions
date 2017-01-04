import Html exposing (div, text)
import Dict


s = "abcabbdfbbaa"


main =
    div []
        [ div [] [ text (toString (lengthOfLongestSubstring s)) ]
        ]


lengthOfLongestSubstring : String -> Int
lengthOfLongestSubstring s =
    ( String.foldl
        (\char state ->
            let
                start =
                    if Dict.member char state.used &&
                        (Maybe.withDefault 0 <| Dict.get char state.used) >= state.start then
                        (Maybe.withDefault 0 <| Dict.get char state.used) + 1
                    else
                        state.start
                used =
                    Dict.insert char state.i state.used
            in
                { start = start
                , longest = max state.longest (state.i - start + 1)
                , i = state.i + 1
                , used = used
                }
        )
        { start = 0
        , longest = 0
        , i = 0
        , used = Dict.empty
        }
        s
    ).longest
