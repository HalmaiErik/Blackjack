module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Main exposing (..)
import Card exposing (..)

genTest name fn input expected =
  test name <|
    \_ -> Expect.equal expected (fn input)

suite : Test
suite = describe "Test" [
   describe "calculateScore"
     [  genTest "calculateScore 1" calculateScore [(King, Hearts)] 10
     ,  genTest "calculateScore 2" calculateScore [(Two, Hearts)] 2
     ,  genTest "calculateScore 3" calculateScore [(Two, Hearts), (King, Spades)] 12
     ,  genTest "calculateScore 4" calculateScore [(Ace, Hearts), (King, Spades)] 21
     ,  genTest "calculateScore 5" calculateScore [(Ace, Hearts), (Five, Hearts), (Seven, Spades)] 13
     ,  genTest "calculateScore 6" calculateScore [(King, Hearts), (Five, Hearts), (Seven, Spades)] 22
     ,  genTest "calculateScore 7" calculateScore [(King, Hearts), (Ten, Clubs), (Ace, Spades)] 21
     ,  genTest "calculateScore 8" calculateScore [(Ace, Spades), (Ace, Clubs), (Ten, Clubs), (King, Clubs)] 22
     ]
  ]
