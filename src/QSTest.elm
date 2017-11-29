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
            [ ( "one string"
              , parseConfig
              , "?a=1"
              , Dict.fromList [ ( "a", QueryString "1" ) ]
              )
            , ( "two strings"
              , parseConfig
              , "?a=1&b=2"
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              )
            , ( "missing ?"
              , parseConfig
              , "a=1&b=2"
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              )
            , ( "a list of strings"
              , parseConfig
              , "?a%5B%5D=1&a%5B%5D=2"
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              )
            , ( "mixed"
              , parseConfig
              , "?a%5B%5D=1&a%5B%5D=2&b=3"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "1", "2" ] )
                    , ( "b", QueryString "3" )
                    ]
              )
            , ( "booleans"
              , parseConfig
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", QueryBool True )
                    , ( "b", QueryBool False )
                    ]
              )
            , ( "booleans as strings"
              , parseConfig |> parseBooleans False
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", QueryString "true" )
                    , ( "b", QueryString "false" )
                    ]
              )
            , ( "list of booleans"
              , parseConfig
              , "?a[]=true&a[]=false"
              , Dict.fromList
                    [ ( "a", QueryBoolList [ True, False ] )
                    ]
              )
            , ( "list of booleans as strings"
              , parseConfig |> parseBooleans False
              , "?a[]=true&a[]=false"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "true", "false" ] )
                    ]
              )
            , ( "list of mixed booleans and strings"
              , parseConfig
              , "?a[]=true&a[]=falso"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "true", "falso" ] )
                    ]
              )
            , ( "rubish"
              , parseConfig
              , "33monkey*^222"
              , Dict.empty
              )
            , ( "incomplete"
              , parseConfig
              , "33monkey*^222&a=1"
              , Dict.fromList
                    [ ( "a", QueryString "1" )
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
              , serializeConfig
              , Dict.fromList [ ( "a", QueryString "1" ) ]
              , "?a=1"
              )
            , ( "two strings"
              , serializeConfig
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              , "?a=1&b=2"
              )
            , ( "list of strings"
              , serializeConfig
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              , "?a%5B%5D=1&a%5B%5D=2"
              )
            , ( "do not encode brackets"
              , serializeConfig |> encodeBrackets False
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              , "?a[]=1&a[]=2"
              )
            , ( "brackets in value are always encoded"
              , serializeConfig |> encodeBrackets False
              , Dict.fromList [ ( "a", QueryStringList [ "1[]", "2" ] ) ]
              , "?a[]=1%5B%5D&a[]=2"
              )
            , ( "boolean"
              , serializeConfig
              , Dict.fromList [ ( "a", QueryBool True ) ]
              , "?a=true"
              )
            , ( "list of booleans"
              , serializeConfig
              , Dict.fromList [ ( "a", QueryBoolList [ True, False ] ) ]
              , "?a%5B%5D=true&a%5B%5D=false"
              )
            , ( "number"
              , serializeConfig
              , Dict.fromList [ ( "a", QueryNumber 1 ) ]
              , "?a=1"
              )
            , ( "list of numbers"
              , serializeConfig
              , Dict.fromList [ ( "a", QueryNumberList [ 1, 2 ] ) ]
              , "?a%5B%5D=1&a%5B%5D=2"
              )
            ]
    in
        List.map serializeTest inputs
            |> describe "serialize"


all : Test
all =
    describe "QS"
        [ parseTests
        , serializeTests
        ]
