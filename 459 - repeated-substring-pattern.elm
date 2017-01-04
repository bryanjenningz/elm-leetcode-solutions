import Html exposing (div, text)


str = "abcabcabc"


main =
    div []
        [ div [] [ text (toString (repeatedSubstringPattern str)) ]
        ]


at : Int -> String -> String
at i str =
    String.slice i (i + 1) str


repeatedSubstringPattern : String -> Bool
repeatedSubstringPattern str =
    List.any
        (\div ->
            if String.length str % div /= 0 then
                False
            else
                let
                    blockSize =
                        String.length str // div
                in
                    List.all
                        (\offset ->
                            List.all
                                (\blockOffset ->
                                    (at (blockOffset * blockSize + offset) str)
                                        == (at offset str)
                                )
                                (List.range 1 (div - 1))
                        )
                        (List.range 0 (blockSize - 1))
        )
        (List.range 2 (String.length str))
