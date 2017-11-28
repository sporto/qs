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


queryToStringTest ( testCase, input, expected ) =
    test testCase <|
        \() ->
            Expect.equal expected (queryToString input)


queryToStringTests : Test
queryToStringTests =
    let
        inputs =
            [ ( "one string"
              , Dict.fromList [ ( "a", QueryString "1" ) ]
              , "?a=1"
              )
            , ( "two strings"
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              , "?a=1&b=2"
              )
            , ( "list of strings"
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              , "?a%5B%5D=1&a%5B%5D=2"
              )
            , ( "boolean"
              , Dict.fromList [ ( "a", QueryBool True ) ]
              , "?a=true"
              )
            , ( "list of booleans"
              , Dict.fromList [ ( "a", QueryBoolList [ True, False ] ) ]
              , "?a%5B%5D=true&a%5B%5D=false"
              )
            , ( "number"
              , Dict.fromList [ ( "a", QueryNumber 1 ) ]
              , "?a=1"
              )
            , ( "list of numbers"
              , Dict.fromList [ ( "a", QueryNumberList [ 1, 2 ] ) ]
              , "?a%5B%5D=1&a%5B%5D=2"
              )
            ]
    in
        List.map queryToStringTest inputs
            |> describe "queryToString"


all : Test
all =
    describe "QS"
        [ parseTests
        , queryToStringTests
        ]
