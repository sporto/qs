module Main exposing (..)

import QSTest
import Test exposing (..)


suite : Test
suite =
    describe "suite"
        [ QSTest.all
        ]
