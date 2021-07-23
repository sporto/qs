module QSTest exposing
    ( all
    , decoderTest
    , decoderTests
    , encodeTest
    , encodeTests
    , parseTest
    , parseTests
    , pushStrTest
    , pushStrTests
    , serializeTest
    , serializeTests
    )

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import QS exposing (..)
import Test exposing (..)



-- parseQuery


parseTest testCase config input expected =
    test testCase <|
        \_ ->
            Expect.equal expected (parse config input)


parseTests : Test
parseTests =
    describe "parse"
        [ -- Strings
          parseTest "one string"
            config
            "?a=x"
            (Dict.fromList [ ( "a", One "x" ) ])
        , parseTest
            "two strings"
            config
            "?a=y&b=z"
            (Dict.fromList [ ( "a", One "y" ), ( "b", One "z" ) ])
        , parseTest
            "a list of strings"
            config
            "?a%5B%5D=y&a%5B%5D=z"
            (Dict.fromList [ ( "a", Many [ "y", "z" ] ) ])

        -- Booleans
        , parseTest
            "booleans"
            config
            "?a=true&b=false"
            (Dict.fromList
                [ ( "a", One "true" )
                , ( "b", One "false" )
                ]
            )
        , parseTest
            "list of booleans"
            config
            "?a[]=true&a[]=false"
            (Dict.fromList
                [ ( "a", Many [ "true", "false" ] )
                ]
            )

        -- Numbers
        , parseTest
            "parse a number"
            config
            "?a=1"
            (Dict.fromList
                [ ( "a", One "1" )
                ]
            )

        -- Mixed
        , parseTest
            "mixed"
            config
            "?a%5B%5D=1&a%5B%5D=2&b=3"
            (Dict.fromList
                [ ( "a", Many [ "1", "2" ] )
                , ( "b", One "3" )
                ]
            )

        -- Incomplete
        , parseTest
            "missing ?"
            config
            "a=z&b=2"
            (Dict.fromList [ ( "a", One "z" ), ( "b", One "2" ) ])
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
                [ ( "a", One "1" )
                ]
            )
        ]



-- queryToString


serializeTest testCase config input expected =
    test testCase <|
        \_ ->
            Expect.equal expected (serialize config input)


serializeTests : Test
serializeTests =
    describe "serialize"
        [ serializeTest
            "one string"
            config
            (Dict.fromList [ ( "a", One "x" ) ])
            "?a=x"
        , serializeTest
            "one string with spaces"
            config
            (Dict.fromList [ ( "a", One "a b" ) ])
            "?a=a%20b"
        , serializeTest
            "two strings"
            config
            (Dict.fromList [ ( "a", One "y" ), ( "b", One "z" ) ])
            "?a=y&b=z"
        , serializeTest
            "list of strings"
            config
            (Dict.fromList [ ( "a", Many [ "z", "y" ] ) ])
            "?a%5B%5D=z&a%5B%5D=y"
        , serializeTest
            "list of strings with spaces"
            config
            (Dict.fromList [ ( "a", Many [ "a b", "c d" ] ) ])
            "?a%5B%5D=a%20b&a%5B%5D=c%20d"
        , serializeTest
            "do not encode brackets"
            (config |> encodeBrackets False)
            (Dict.fromList [ ( "a", Many [ "1", "2" ] ) ])
            "?a[]=1&a[]=2"
        , serializeTest
            "brackets in value are always encoded"
            (config |> encodeBrackets False)
            (Dict.fromList [ ( "a", Many [ "1[]", "2" ] ) ])
            "?a[]=1%5B%5D&a[]=2"
        , serializeTest
            "boolean"
            config
            (Dict.fromList [ ( "a", One "true" ) ])
            "?a=true"
        , serializeTest
            "list of booleans"
            config
            (Dict.fromList [ ( "a", Many [ "true", "false" ] ) ])
            "?a%5B%5D=true&a%5B%5D=false"
        , serializeTest
            "number"
            config
            (Dict.fromList [ ( "a", One "1" ) ])
            "?a=1"
        , serializeTest
            "list of numbers"
            config
            (Dict.fromList [ ( "a", Many [ "1", "2" ] ) ])
            "?a%5B%5D=1&a%5B%5D=2"
        , serializeTest
            "it can serialize without ?"
            (config |> addQuestionMark False)
            (Dict.fromList [ ( "a", One "1" ) ])
            "a=1"
        ]


decoderTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal expected (Decode.decodeString decoder input)


decoderTests =
    describe "decoder"
        [ decoderTest
            "It decodes text"
            """{"a":"x"}"""
            (Ok <| Dict.fromList [ ( "a", One "x" ) ])
        , decoderTest
            "It decodes number"
            """{"a":"1"}"""
            (Ok <| Dict.fromList [ ( "a", One "1" ) ])
        , decoderTest
            "It decodes boolean"
            """{"a":"true"}"""
            (Ok <| Dict.fromList [ ( "a", One "true" ) ])
        , decoderTest
            "It decodes a list"
            """{"a":["x", "1", "true"]}"""
            (Ok <| Dict.fromList [ ( "a", Many [ "x", "1", "true" ] ) ])
        ]


encodeTest testCase input expected =
    test testCase <|
        \_ ->
            Expect.equal expected (Encode.encode 0 <| encode input)


encodeTests =
    describe "encode"
        [ encodeTest
            "It encodes one"
            (Dict.fromList [ ( "a", One "x" ) ])
            """{"a":"x"}"""
        , encodeTest
            "It encodes a list"
            (Dict.fromList [ ( "a", Many [ "x", "1", "true" ] ) ])
            """{"a":["x","1","true"]}"""
        ]


pushStrTest testCase initialQuery key input expected =
    test testCase <|
        \_ ->
            Expect.equal
                expected
                (pushStr key input initialQuery)


pushStrTests =
    describe "pushStr"
        [ pushStrTest
            "It adds to an existing list"
            (Dict.fromList [ ( "a", Many [ "x" ] ) ])
            "a"
            "z"
            (Dict.fromList [ ( "a", Many [ "x", "z" ] ) ])
        , pushStrTest
            "Adds new value"
            Dict.empty
            "a"
            "z"
            (Dict.fromList [ ( "a", Many [ "z" ] ) ])
        , pushStrTest
            "Promotes a value"
            (Dict.fromList [ ( "a", One "x" ) ])
            "a"
            "z"
            (Dict.fromList [ ( "a", Many [ "x", "z" ] ) ])
        ]


getAsStringListTest testCase query key expected =
    test testCase <|
        \_ ->
            Expect.equal
                expected
                (getAsStringList key query)


getAsStringListTests =
    describe
        "getAsStringList"
        [ getAsStringListTest
            "It returns the list"
            (Dict.fromList [ ( "a", Many [ "x", "y" ] ) ])
            "a"
            [ "x", "y" ]
        , getAsStringListTest
            "It returns an empty list when there are not keys"
            Dict.empty
            "a"
            []
        ]


getAsMaybeStringListTest testCase query key expected =
    test testCase <|
        \_ ->
            Expect.equal
                expected
                (getAsMaybeStringList key query)


getAsMaybeStringListTests =
    describe
        "getAsMaybeStringList"
        [ getAsMaybeStringListTest
            "It returns the list"
            (Dict.fromList [ ( "a", Many [ "x", "y" ] ) ])
            "a"
            (Just [ "x", "y" ])
        , getAsMaybeStringListTest
            "It Nothing when there are not keys"
            Dict.empty
            "a"
            Nothing
        ]


all : Test
all =
    describe "QS"
        [ parseTests
        , serializeTests
        , decoderTests
        , encodeTests
        , pushStrTests
        , getAsStringListTests
        , getAsMaybeStringListTests
        ]
