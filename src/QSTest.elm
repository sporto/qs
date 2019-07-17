module QSTest exposing (all, decoderTest, decoderTests, encodeTest, encodeTests, parseTest, parseTests, pushStrTest, pushStrTests, serializeTest, serializeTests)

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import QS exposing (..)
import Test exposing (..)



-- parseQuery


parseTest testCase config input expected =
    test testCase <|
        \() ->
            Expect.equal expected (parse config input)


parseTests : Test
parseTests =
    describe "parse"
        [ -- Strings
          parseTest "one string"
            config
            "?a=x"
            (Dict.fromList [ ( "a", One <| Str "x" ) ])
        , parseTest
            "two strings"
            config
            "?a=y&b=z"
            (Dict.fromList [ ( "a", One <| Str "y" ), ( "b", One <| Str "z" ) ])
        , parseTest
            "a list of strings"
            config
            "?a%5B%5D=y&a%5B%5D=z"
            (Dict.fromList [ ( "a", Many [ Str "y", Str "z" ] ) ])

        -- Booleans
        , parseTest
            "booleans"
            config
            "?a=true&b=false"
            (Dict.fromList
                [ ( "a", One <| Boolean True )
                , ( "b", One <| Boolean False )
                ]
            )
        , parseTest
            "booleans as strings"
            (config |> parseBooleans False)
            "?a=true&b=false"
            (Dict.fromList
                [ ( "a", One <| Str "true" )
                , ( "b", One <| Str "false" )
                ]
            )
        , parseTest
            "list of booleans"
            config
            "?a[]=true&a[]=false"
            (Dict.fromList
                [ ( "a", Many [ Boolean True, Boolean False ] )
                ]
            )
        , parseTest
            "list of booleans as strings"
            (config |> parseBooleans False)
            "?a[]=true&a[]=false"
            (Dict.fromList
                [ ( "a", Many [ Str "true", Str "false" ] )
                ]
            )

        -- Numbers
        , parseTest
            "parse a number"
            config
            "?a=1"
            (Dict.fromList
                [ ( "a", One <| Number 1 )
                ]
            )
        , parseTest
            "parse number as string"
            (config |> parseNumbers False)
            "?a=1"
            (Dict.fromList
                [ ( "a", One <| Str "1" )
                ]
            )

        -- Mixed
        , parseTest
            "mixed"
            config
            "?a%5B%5D=1&a%5B%5D=2&b=3"
            (Dict.fromList
                [ ( "a", Many [ Number 1, Number 2 ] )
                , ( "b", One <| Number 3 )
                ]
            )
        , parseTest
            "list of mixed booleans and strings"
            config
            "?a[]=true&a[]=falso"
            (Dict.fromList
                [ ( "a", Many [ Boolean True, Str "falso" ] )
                ]
            )

        -- Incomplete
        , parseTest
            "missing ?"
            config
            "a=z&b=2"
            (Dict.fromList [ ( "a", One <| Str "z" ), ( "b", One <| Number 2 ) ])
        , parseTest
            "rubish"
            config
            "33monkey*^222"
            Dict.empty
        , parseTest
            "incomplete"
            config
            "33monkey*^222&a=1"
            (Dict.fromList
                [ ( "a", One <| Number 1 )
                ]
            )
        ]



-- queryToString


serializeTest testCase config input expected =
    test testCase <|
        \() ->
            Expect.equal expected (serialize config input)


serializeTests : Test
serializeTests =
    describe "serialize"
        [ serializeTest
            "one string"
            config
            (Dict.fromList [ ( "a", One <| Str "x" ) ])
            "?a=x"
        , serializeTest
            "one string with spaces"
            config
            (Dict.fromList [ ( "a", One <| Str "a b" ) ])
            "?a=a%20b"
        , serializeTest
            "two strings"
            config
            (Dict.fromList [ ( "a", One <| Str "y" ), ( "b", One <| Str "z" ) ])
            "?a=y&b=z"
        , serializeTest
            "list of strings"
            config
            (Dict.fromList [ ( "a", Many [ Str "z", Str "y" ] ) ])
            "?a%5B%5D=z&a%5B%5D=y"
        , serializeTest
            "list of strings with spaces"
            config
            (Dict.fromList [ ( "a", Many [ Str "a b", Str "c d" ] ) ])
            "?a%5B%5D=a%20b&a%5B%5D=c%20d"
        , serializeTest
            "do not encode brackets"
            (config |> encodeBrackets False)
            (Dict.fromList [ ( "a", Many [ Str "1", Str "2" ] ) ])
            "?a[]=1&a[]=2"
        , serializeTest
            "brackets in value are always encoded"
            (config |> encodeBrackets False)
            (Dict.fromList [ ( "a", Many [ Str "1[]", Str "2" ] ) ])
            "?a[]=1%5B%5D&a[]=2"
        , serializeTest
            "boolean"
            config
            (Dict.fromList [ ( "a", One <| Boolean True ) ])
            "?a=true"
        , serializeTest
            "list of booleans"
            config
            (Dict.fromList [ ( "a", Many [ Boolean True, Boolean False ] ) ])
            "?a%5B%5D=true&a%5B%5D=false"
        , serializeTest
            "number"
            config
            (Dict.fromList [ ( "a", One <| Number 1 ) ])
            "?a=1"
        , serializeTest
            "list of numbers"
            config
            (Dict.fromList [ ( "a", Many [ Number 1, Number 2 ] ) ])
            "?a%5B%5D=1&a%5B%5D=2"
        ]


decoderTest testCase input expected =
    test testCase <|
        \() ->
            Expect.equal expected (Decode.decodeString decoder input)


decoderTests =
    describe "decoder"
        [ decoderTest
            "It decodes text"
            """{"a":"x"}"""
            (Ok <| Dict.fromList [ ( "a", One <| Str "x" ) ])
        , decoderTest
            "It decodes number"
            """{"a":1}"""
            (Ok <| Dict.fromList [ ( "a", One <| Number 1 ) ])
        , decoderTest
            "It decodes boolean"
            """{"a":true}"""
            (Ok <| Dict.fromList [ ( "a", One <| Boolean True ) ])
        , decoderTest
            "It decodes a list"
            """{"a":["x", 1, true]}"""
            (Ok <| Dict.fromList [ ( "a", Many [ Str "x", Number 1, Boolean True ] ) ])
        ]


encodeTest testCase input expected =
    test testCase <|
        \() ->
            Expect.equal expected (Encode.encode 0 <| encode input)


encodeTests =
    describe "encode"
        [ encodeTest
            "It encodes one"
            (Dict.fromList [ ( "a", One <| Str "x" ) ])
            """{"a":"x"}"""
        , encodeTest
            "It encodes a list"
            (Dict.fromList [ ( "a", Many [ Str "x", Number 1, Boolean True ] ) ])
            """{"a":["x",1,true]}"""
        ]


pushStrTest testCase initialQuery key input expected =
    test testCase <|
        \() ->
            Expect.equal
                expected
                (pushStr key input initialQuery)


pushStrTests =
    describe "pushStr"
        [ pushStrTest
            "It adds to an existing list"
            (Dict.fromList [ ( "a", Many [ Str "x" ] ) ])
            "a"
            "z"
            (Dict.fromList [ ( "a", Many [ Str "x", Str "z" ] ) ])
        , pushStrTest
            "Adds new value"
            Dict.empty
            "a"
            "z"
            (Dict.fromList [ ( "a", Many [ Str "z" ] ) ])
        , pushStrTest
            "Promotes a value"
            (Dict.fromList [ ( "a", One <| Str "x" ) ])
            "a"
            "z"
            (Dict.fromList [ ( "a", Many [ Str "x", Str "z" ] ) ])
        ]


all : Test
all =
    describe "QS"
        [ parseTests
        , serializeTests
        , decoderTests
        , encodeTests
        , pushStrTests
        ]
