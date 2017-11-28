module QSTest exposing (..)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Expect
import QS exposing (..)


-- parseQuery


parseTests : Test
parseTests =
    let
        inputs =
            [ ( "one string"
              , "?a=1"
              , Dict.fromList [ ( "a", QueryString "1" ) ]
              )
            , ( "two strings"
              , "?a=1&b=2"
              , Dict.fromList [ ( "a", QueryString "1" ), ( "b", QueryString "2" ) ]
              )
            , ( "a list"
              , "?a%5B%5D=1&a%5B%5D=2"
              , Dict.fromList [ ( "a", QueryStringList [ "1", "2" ] ) ]
              )
            , ( "mixed"
              , "?a%5B%5D=1&a%5B%5D=2&b=3"
              , Dict.fromList
                    [ ( "a", QueryStringList [ "1", "2" ] )
                    , ( "b", QueryString "3" )
                    ]
              )
            , ( "booleans"
              , "?a=true&b=false"
              , Dict.fromList
                    [ ( "a", QueryBool True )
                    , ( "b", QueryBool False )
                    ]
              )
            , ( "rubish"
              , "33monkey*^222"
              , Dict.empty
              )
            , ( "incomplete"
              , "33monkey*^222&a=1"
              , Dict.fromList
                    [ ( "a", QueryString "1" )
                    ]
              )
            ]

        makeTest ( testCase, input, expected ) =
            test testCase <|
                \() ->
                    Expect.equal expected (parse input)
    in
        List.map makeTest inputs
            |> describe "parse"



-- queryToString


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

        makeTest ( testCase, input, expected ) =
            test testCase <|
                \() ->
                    Expect.equal expected (queryToString input)
    in
        List.map makeTest inputs
            |> describe "queryToString"


all : Test
all =
    describe "QS"
        [ parseTests
        , queryToStringTests
        ]
