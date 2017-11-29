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
              , config
              , "?a=1"
              , Dict.fromList [ ( "a", QueryString "1" ) ]
              )
            , ( "two strings"
              , config
              , "?a=1&b=2"
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              )
            , ( "missing ?"
              , config
              , "a=1&b=2"
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              )
            , ( "a list of strings"
              , config
              , "?a%5B%5D=1&a%5B%5D=2"
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              )
            , ( "mixed"
              , config
              , "?a%5B%5D=1&a%5B%5D=2&b=3"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "1", "2" ] )
                    , ( "b", QueryString "3" )
                    ]
              )

            -- Booleans
            , ( "booleans"
              , config
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", QueryBool True )
                    , ( "b", QueryBool False )
                    ]
              )
            , ( "booleans as strings"
              , config |> parseBooleans False
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", QueryString "true" )
                    , ( "b", QueryString "false" )
                    ]
              )
            , ( "list of booleans"
              , config
              , "?a[]=true&a[]=false"
              , Dict.fromList
                    [ ( "a", QueryBoolList [ True, False ] )
                    ]
              )
            , ( "list of booleans as strings"
              , config |> parseBooleans False
              , "?a[]=true&a[]=false"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "true", "false" ] )
                    ]
              )
            , ( "list of mixed booleans and strings"
              , config
              , "?a[]=true&a[]=falso"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "true", "falso" ] )
                    ]
              )

            -- Numbers
            , ( "parse a number"
              , config
              , "?a=1"
              , Dict.fromList
                    [ ( "a", QueryNumber 1 )
                    ]
              )

            -- Incomplete
            , ( "rubish"
              , config
              , "33monkey*^222"
              , Dict.empty
              )
            , ( "incomplete"
              , config
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
              , config
              , Dict.fromList [ ( "a", QueryString "1" ) ]
              , "?a=1"
              )
            , ( "two strings"
              , config
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              , "?a=1&b=2"
              )
            , ( "list of strings"
              , config
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              , "?a%5B%5D=1&a%5B%5D=2"
              )
            , ( "do not encode brackets"
              , config |> encodeBrackets False
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              , "?a[]=1&a[]=2"
              )
            , ( "brackets in value are always encoded"
              , config |> encodeBrackets False
              , Dict.fromList [ ( "a", QueryStringList [ "1[]", "2" ] ) ]
              , "?a[]=1%5B%5D&a[]=2"
              )
            , ( "boolean"
              , config
              , Dict.fromList [ ( "a", QueryBool True ) ]
              , "?a=true"
              )
            , ( "list of booleans"
              , config
              , Dict.fromList [ ( "a", QueryBoolList [ True, False ] ) ]
              , "?a%5B%5D=true&a%5B%5D=false"
              )
            , ( "number"
              , config
              , Dict.fromList [ ( "a", QueryNumber 1 ) ]
              , "?a=1"
              )
            , ( "list of numbers"
              , config
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
