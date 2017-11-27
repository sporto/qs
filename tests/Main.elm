module Main exposing (..)

import Test exposing (..)
import QSTest


suite : Test
suite =
    describe "suite"
        [ QSTest.all
        ]
