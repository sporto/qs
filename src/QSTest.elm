module QSTest exposing (..)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Expect
import QS exposing (..)


-- parseQuery


parseTest ( testCase, config, input, expected ) =
    test testCase <|
        \() ->
            Expect.equal expected (parse config input)


parseTests : Test
parseTests =
    let
        inputs =
            [ -- Strings
              ( "one string"
              , config
              , "?a=x"
              , Dict.fromList [ ( "a", One <| Str "x" ) ]
              )
            , ( "two strings"
              , config
              , "?a=y&b=z"
              , Dict.fromList [ ( "a", One <| Str "y" ), ( "b", One <| Str "z" ) ]
              )
            , ( "a list of strings"
              , config
              , "?a%5B%5D=y&a%5B%5D=z"
              , Dict.fromList [ ( "a", Many [ Str "y", Str "z" ] ) ]
              )

            -- Booleans
            , ( "booleans"
              , config
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", One <| Boolean True )
                    , ( "b", One <| Boolean False )
                    ]
              )
            , ( "booleans as strings"
              , config |> parseBooleans False
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", One <| Str "true" )
                    , ( "b", One <| Str "false" )
                    ]
              )
            , ( "list of booleans"
              , config
              , "?a[]=true&a[]=false"
              , Dict.fromList
                    [ ( "a", Many [ Boolean True, Boolean False ] )
                    ]
              )
            , ( "list of booleans as strings"
              , config |> parseBooleans False
              , "?a[]=true&a[]=false"
              , Dict.fromList
                    [ ( "a", Many [ Str "true", Str "false" ] )
                    ]
              )

            -- Numbers
            , ( "parse a number"
              , config
              , "?a=1"
              , Dict.fromList
                    [ ( "a", One <| Number 1 )
                    ]
              )
            , ( "parse number as string"
              , config |> parseNumbers False
              , "?a=1"
              , Dict.fromList
                    [ ( "a", One <| Str "1" )
                    ]
              )

            -- Mixed
            , ( "mixed"
              , config
              , "?a%5B%5D=1&a%5B%5D=2&b=3"
              , Dict.fromList
                    [ ( "a", Many [ Number 1, Number 2 ] )
                    , ( "b", One <| Number 3 )
                    ]
              )
            , ( "list of mixed booleans and strings"
              , config
              , "?a[]=true&a[]=falso"
              , Dict.fromList
                    [ ( "a", Many [ Boolean True, Str "falso" ] )
                    ]
              )

            -- Incomplete
            , ( "missing ?"
              , config
              , "a=z&b=2"
              , Dict.fromList [ ( "a", One <| Str "z" ), ( "b", One <| Number 2 ) ]
              )
            , ( "rubish"
              , config
              , "33monkey*^222"
              , Dict.empty
              )
            , ( "incomplete"
              , config
              , "33monkey*^222&a=1"
              , Dict.fromList
                    [ ( "a", One <| Number 1 )
                    ]
              )
            ]
    in
        List.map parseTest inputs
            |> describe "parse"



-- queryToString


serializeTest ( testCase, config, input, expected ) =
    test testCase <|
        \() ->
            Expect.equal expected (serialize config input)


serializeTests : Test
serializeTests =
    let
        inputs =
            [ ( "one string"
              , config
              , Dict.fromList [ ( "a", One <| Str "x" ) ]
              , "?a=x"
              )
            , ( "two strings"
              , config
              , Dict.fromList [ ( "a", One <| Str "y" ), ( "b", One <| Str "z" ) ]
              , "?a=y&b=z"
              )
            , ( "list of strings"
              , config
              , Dict.fromList [ ( "a", Many [ Str "z", Str "y" ] ) ]
              , "?a%5B%5D=z&a%5B%5D=y"
              )
            , ( "do not encode brackets"
              , config |> encodeBrackets False
              , Dict.fromList [ ( "a", Many [ Str "1", Str "2" ] ) ]
              , "?a[]=1&a[]=2"
              )
            , ( "brackets in value are always encoded"
              , config |> encodeBrackets False
              , Dict.fromList [ ( "a", Many [ Str "1[]", Str "2" ] ) ]
              , "?a[]=1%5B%5D&a[]=2"
              )
            , ( "boolean"
              , config
              , Dict.fromList [ ( "a", One <| Boolean True ) ]
              , "?a=true"
              )
            , ( "list of booleans"
              , config
              , Dict.fromList [ ( "a", Many [ Boolean True, Boolean False ] ) ]
              , "?a%5B%5D=true&a%5B%5D=false"
              )
            , ( "number"
              , config
              , Dict.fromList [ ( "a", One <| Number 1 ) ]
              , "?a=1"
              )
            , ( "list of numbers"
              , config
              , Dict.fromList [ ( "a", Many [ Number 1, Number 2 ] ) ]
              , "?a%5B%5D=1&a%5B%5D=2"
              )
            ]
    in
        List.map serializeTest inputs
            |> describe "serialize"


decoderTest ( testCase, input, expected ) =
    test testCase <|
        \() ->
            Expect.equal expected (Decode.decodeString decoder input)


decoderTests =
    let
        inputs =
            [ ( "It decodes text"
              , """{"a":"x"}"""
              , Ok <| Dict.fromList [ ( "a", One <| Str "x" ) ]
              )
            , ( "It decodes number"
              , """{"a":1}"""
              , Ok <| Dict.fromList [ ( "a", One <| Number 1 ) ]
              )
            , ( "It decodes boolean"
              , """{"a":true}"""
              , Ok <| Dict.fromList [ ( "a", One <| Boolean True ) ]
              )
            , ( "It decodes a list"
              , """{"a":["x", 1, true]}"""
              , Ok <| Dict.fromList [ ( "a", Many [ Str "x", Number 1, Boolean True ] ) ]
              )
            ]
    in
        List.map decoderTest inputs
            |> describe "decoder"


encodeTest ( testCase, input, expected ) =
    test testCase <|
        \() ->
            Expect.equal expected (Encode.encode 0 <| encode input)


encodeTests =
    let
        inputs =
            [ ( "It encodes one"
              , Dict.fromList [ ( "a", One <| Str "x" ) ]
              , """{"a":"x"}"""
              )
            , ( "It encodes a list"
              , Dict.fromList [ ( "a", Many [ Str "x", Number 1, Boolean True ] ) ]
              , """{"a":["x",1,true]}"""
              )
            ]
    in
        List.map encodeTest inputs
            |> describe "encode"


all : Test
all =
    describe "QS"
        [ parseTests
        , serializeTests
        , decoderTests
        , encodeTests
        ]
