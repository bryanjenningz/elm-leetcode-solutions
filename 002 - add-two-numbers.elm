import Html exposing (div, text)


xs = [2, 3, 4, 9, 9]
ys = [3, 4, 1, 2]


main =
    div []
        [ div [] [ text (toString xs) ]
        , div [] [ text (toString ys) ]
        , div [] [ text (toString (addTwoNumbers xs ys)) ]
        ]


addTwoNumbers : List Int -> List Int -> List Int
addTwoNumbers xs ys =
    let
        maxLength =
            max (List.length xs) (List.length ys)
        xsEnd =
            List.repeat (maxLength - List.length xs) 0
        ysEnd =
            List.repeat (maxLength - List.length ys) 0
        xys =
            List.map2 (\x y -> x :: [ y ]) (xs ++ xsEnd) (ys ++ ysEnd)
        start =
            List.foldl
                (\xy state ->
                    let
                        sum =
                            (Maybe.withDefault 0 <| List.head xy)
                                + (Maybe.withDefault 0 <| List.head <| List.drop 1 xy)
                                + state.overflow
                        overflow =
                            sum // 10
                        onesDigit =
                            sum % 10
                    in
                        { overflow = overflow
                        , result = state.result ++ [ onesDigit ]
                        }
                )
                { overflow = 0
                , result = []
                }
                xys
        end =
            if start.overflow == 0 then
                []
            else
                [ start.overflow ]
    in
        start.result ++ end
