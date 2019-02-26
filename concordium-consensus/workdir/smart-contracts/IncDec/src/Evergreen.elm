module Evergreen exposing (atIndex, custom, d_char, d_dict, d_order, d_set, d_time, d_tuple, d_unit, e_char, e_dict, e_order, e_time, e_tuple, e_unit, union, union1, union2, unsafeRunDecoder, unsafeRunEncoder)
import Char
import Dict exposing (Dict)
import Json.Decode as D
import Json.Encode as E
import Set exposing (Set)
import Time
import Debug


e_char : Char -> E.Value
e_char evg_p0 =
    E.int (Char.toCode evg_p0)


d_char : D.Decoder Char
d_char =
    D.int |> D.map Char.fromCode


e_order : Order -> E.Value
e_order evg_p0 =
    case evg_p0 of
        LT ->
            E.string "LT"

        EQ ->
            E.string "EQ"

        GT ->
            E.string "GT"


d_order : D.Decoder Order
d_order =
    D.string
        |> D.andThen
            (\s ->
                case s of
                    "LT" ->
                        D.succeed LT

                    "EQ" ->
                        D.succeed EQ

                    "GT" ->
                        D.succeed GT

                    _ ->
                        D.fail <| "unexpected Order value: " ++ s
            )


d_set : D.Decoder comparable -> D.Decoder (Set comparable)
d_set decoder =
    D.list decoder |> D.map Set.fromList


e_time : Time.Posix -> E.Value
e_time t =
    E.int <| Time.posixToMillis t


d_time : D.Decoder Time.Posix
d_time =
    D.int |> D.map (\t -> Time.millisToPosix t)

e_result : (err -> E.Value) -> (a -> E.Value) -> Result err a -> E.Value
e_result err_encode a_encode result =
    case result of
        Ok a ->
            E.list identity [ E.string "Ok", a_encode a ]

        Err err ->
            E.list identity [ E.string "Err", err_encode err ]


d_result : D.Decoder err -> D.Decoder a -> D.Decoder (Result err a)
d_result err_decode a_decode =
    D.oneOf
        [ union1 "Ok" a_decode Ok
        , union1 "Err" err_decode Err
        ]


e_maybe : (a -> E.Value) -> Maybe a -> E.Value
e_maybe a_encode maybe =
    case maybe of
        Just a ->
            E.list identity [ E.string "Just", a_encode a ]

        Nothing ->
            E.list identity [ E.string "Nothing" ]


d_maybe : D.Decoder a -> D.Decoder (Maybe a)
d_maybe a_decode =
    D.oneOf
        [ union1 "Just" a_decode Just
        , union "Nothing" Nothing
        ]


e_dict : (comparable -> E.Value) -> (v -> E.Value) -> Dict comparable v -> E.Value
e_dict k_encode v_encode dict =
    dict
        |> Dict.toList
        |> List.map (\t -> e_tuple k_encode v_encode t)
        |> E.list identity


d_dict : D.Decoder comparable -> D.Decoder v -> D.Decoder (Dict comparable v)
d_dict k_decode v_decode =
    D.list (d_tuple k_decode v_decode)
        |> D.map Dict.fromList


e_tuple : (a -> E.Value) -> (b -> E.Value) -> ( a, b ) -> E.Value
e_tuple a_encode b_encode ( a, b ) =
    E.list identity [ a_encode a, b_encode b ]


d_tuple : D.Decoder a -> D.Decoder b -> D.Decoder ( a, b )
d_tuple a b =
    D.index 0 a
        |> D.andThen
            (\aVal ->
                D.index 1 b
                    |> D.andThen (\bVal -> D.succeed ( aVal, bVal ))
            )


e_triple : (a -> E.Value) -> (b -> E.Value) -> (c -> E.Value) -> ( a, b, c ) -> E.Value
e_triple a_encode b_encode c_encode ( a, b, c ) =
    E.list identity [ a_encode a, b_encode b, c_encode c ]


d_triple : D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder ( a, b, c )
d_triple a b c =
    D.index 0 a
        |> D.andThen
            (\aVal ->
                D.index 1 b
                    |> D.andThen
                        (\bVal ->
                            D.index 2 c
                                |> D.andThen (\cVal -> D.succeed ( aVal, bVal, cVal ))
                        )
            )


e_unit : () -> E.Value
e_unit a =
    E.null


d_unit : D.Decoder ()
d_unit =
    D.null ()


union : String -> a -> D.Decoder a
union str final =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.succeed final

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union1 : String -> D.Decoder a -> (a -> b) -> D.Decoder b
union1 str decoder constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.index 1 decoder |> D.map constructor

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union2 : String -> D.Decoder a -> D.Decoder b -> (a -> b -> c) -> D.Decoder c
union2 str decoder1 decoder2 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map2 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union3 : String -> D.Decoder a -> D.Decoder b -> D.Decoder c -> (a -> b -> c -> d) -> D.Decoder d
union3 str decoder1 decoder2 decoder3 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map3 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)
                        (D.index 3 decoder3)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union4 : String -> D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder d -> (a -> b -> c -> d -> e) -> D.Decoder e
union4 str decoder1 decoder2 decoder3 decoder4 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map4 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)
                        (D.index 3 decoder3)
                        (D.index 4 decoder4)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union5 : String -> D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder d -> D.Decoder e -> (a -> b -> c -> d -> e -> f) -> D.Decoder f
union5 str decoder1 decoder2 decoder3 decoder4 decoder5 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map5 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)
                        (D.index 3 decoder3)
                        (D.index 4 decoder4)
                        (D.index 5 decoder5)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union6 : String -> D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder d -> D.Decoder e -> D.Decoder f -> (a -> b -> c -> d -> e -> f -> g) -> D.Decoder g
union6 str decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map6 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)
                        (D.index 3 decoder3)
                        (D.index 4 decoder4)
                        (D.index 5 decoder5)
                        (D.index 6 decoder6)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union7 : String -> D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder d -> D.Decoder e -> D.Decoder f -> D.Decoder g -> (a -> b -> c -> d -> e -> f -> g -> h) -> D.Decoder h
union7 str decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map7 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)
                        (D.index 3 decoder3)
                        (D.index 4 decoder4)
                        (D.index 5 decoder5)
                        (D.index 6 decoder6)
                        (D.index 7 decoder7)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


union8 : String -> D.Decoder a -> D.Decoder b -> D.Decoder c -> D.Decoder d -> D.Decoder e -> D.Decoder f -> D.Decoder g -> D.Decoder h -> (a -> b -> c -> d -> e -> f -> g -> h -> i) -> D.Decoder i
union8 str decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8 constructor =
    D.index 0 D.string
        |> D.andThen
            (\s ->
                if s == str then
                    D.map8 constructor
                        (D.index 1 decoder1)
                        (D.index 2 decoder2)
                        (D.index 3 decoder3)
                        (D.index 4 decoder4)
                        (D.index 5 decoder5)
                        (D.index 6 decoder6)
                        (D.index 7 decoder7)
                        (D.index 8 decoder8)

                else
                    D.fail <| "expected '" ++ str ++ "' but saw '" ++ s
            )


atIndex : Int -> D.Decoder a -> (D.Decoder (a -> b) -> D.Decoder b)
atIndex idx decoder =
    custom (D.index idx decoder)



-- Trick from json-decode-pipeline


custom : D.Decoder a -> D.Decoder (a -> b) -> D.Decoder b
custom =
    D.map2 (|>)

-- Useful for internal / temporary (de)serialisation code

unsafeRunDecoder : D.Decoder a -> String -> a
unsafeRunDecoder decoder str =
  case D.decodeString decoder str of
    Ok value -> value
    Err err -> Debug.todo ("unsafe decoding of string failed: " ++ (Debug.toString err))

unsafeRunEncoder : (a -> E.Value) -> a -> String
unsafeRunEncoder encoder value =
  E.encode 0 (encoder value)
