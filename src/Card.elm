-----------------------
-- Erik Halmai
-- 02.11.2020
-----------------------

module Card exposing (Card, Face(..), Suit(..), cardValue, viewCard, cardToString, deck, cardToUnicode)

import Html exposing (..)
import Html.Attributes exposing (style)

{-
  Replace with your definitions from assignment 1
-}
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
type Suit = Clubs | Diamonds | Hearts | Spades
type alias Card = (Face, Suit)

faceList = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
suitList = [Clubs, Diamonds, Hearts, Spades]

faceToString : Face -> String
faceToString face =
    case face of
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nine"
        Ten -> "Ten"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"

suitToString : Suit -> String
suitToString suit = 
    case suit of
        Clubs -> "Clubs"
        Diamonds -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"

cardToString : Card -> String
cardToString (face, suit) = 
        faceToString face ++ " of " ++ suitToString suit

cardValue : Card -> List Int
cardValue card = 
    case card of
        (Ace, _) -> [1, 11]
        (Two, _) -> [2]
        (Three, _) -> [3]
        (Four, _) -> [4]
        (Five, _) -> [5]
        (Six, _) -> [6]
        (Seven, _) -> [7]
        (Eight, _) -> [8]
        (Nine, _) -> [9]
        (_, _) -> [10]

deck : List Card
deck =
    List.concat ( 
        List.map (\face ->
            List.map (\suit -> (face, suit)) suitList
        ) faceList
    )

{-
  Modify this function (if needed) to work with your `Card` definition
-}
cardToUnicode : Card -> String
cardToUnicode (face, suit) = 
    case face of
        Ace -> case suit of 
            Spades ->"🂡"
            Hearts -> "🂱"
            Clubs ->  "🃑"
            Diamonds -> "🃁"
        Two -> case suit of 
            Spades ->"🂢"
            Hearts -> "🂲"
            Clubs ->  "🃒"
            Diamonds -> "🃂"
        Three -> case suit of 
            Spades ->"🂣"
            Hearts -> "🂳"
            Clubs ->  "🃓"
            Diamonds ->"🃃" 
        Four -> case suit of 
            Spades ->"🂤"
            Hearts -> "🂴"
            Clubs ->  "🃔"
            Diamonds -> "🃄"
        Five -> case suit of 
            Spades ->"🂥"
            Hearts -> "🂵"
            Clubs ->  "🃕"
            Diamonds -> "🃅"
        Six -> case suit of 
            Spades ->"🂦"
            Hearts -> "🂶"
            Clubs ->  "🃖"
            Diamonds -> "🃆"
        Seven -> case suit of 
            Spades ->"🂩"
            Hearts -> "🂹"
            Clubs ->  "🃙"
            Diamonds -> "🃉"
        Eight -> case suit of 
            Spades -> "🂨"
            Hearts ->  "🂸"
            Clubs ->   "🃘"
            Diamonds ->  "🃈"
        Nine -> case suit of 
            Spades -> "🂩"
            Hearts ->  "🂹"
            Clubs ->   "🃙"
            Diamonds ->  "🃉"
        Ten -> case suit of 
            Spades ->"🂪"
            Hearts -> "🂺"
            Clubs ->  "🃚"
            Diamonds -> "🃊"
        Jack -> case suit of 
            Spades ->"🂫"
            Hearts -> "🂻"
            Clubs ->  "🃛"
            Diamonds -> "🃋"
        Queen -> case suit of 
            Spades ->"🂭"
            Hearts -> "🂽"
            Clubs ->  "🃝"
            Diamonds -> "🃍"
        King -> case suit of 
            Spades -> "🂮"
            Hearts -> "🂾"
            Clubs ->  "🃞"
            Diamonds -> "🃎"

{-
  Modify this function (if needed) to work with your `Card` definition
-}
viewCard : Card -> Html msg
viewCard (face, suit) =
    let
        faceName = faceToString face
        suitName = suitToString suit
        suitColor s = 
            case s of
                Diamonds -> "red"
                Spades -> "black"
                Hearts -> "red"
                Clubs -> "black"
        unicode = cardToUnicode (face, suit)
    in
        div [style "display" "inline-block"] [
            div [style "font-size" "12em", style "color" (suitColor suit)] [text unicode],
            div [style "font-size" "0.8em"]  [text (cardToString (face, suit))]
        ]