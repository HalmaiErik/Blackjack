-----------------------
-- Erik Halmai
-- 02.11.2020
-----------------------

module Main exposing (main, calculateScore)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Random
import Debug


import Card exposing (Card)
import Card exposing (cardValue)
import Card exposing (viewCard)
import Card exposing (cardToUnicode)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


type alias Model =
  { hand: List Card,
    deck: List Card,
    showDeck: Bool
  }

startingModel : Model
startingModel =
    Model [] Card.deck True

init : () -> (Model, Cmd Msg)
init _ =
  ( startingModel
  , Cmd.none
  )


type Msg
  = Draw
  | NewCard Card
  | ToogleDeck

isOver : Model -> Bool
isOver model =
  if calculateScore model.hand >= 21 then
    True
  else
    False

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Draw ->
      if not (isOver model) then
        ( model
        , drawCard model
        )
      else
        init ()
    
    -- Add the new card to the player's hand (`hand`) and remove it from the `deck`
    NewCard newCard ->
      ( {model | hand = newCard :: model.hand, deck = (List.filter (\x -> if x == newCard then False else True) model.deck)}
      , Cmd.none
      )

    -- Toggle (if it's `True` set it `False`, if it's `False` set it to `True`) the `showDeck` member of the `Model`
    ToogleDeck ->
      (
        {model | showDeck = not model.showDeck}
      , Cmd.none
      )

drawCard : Model -> Cmd Msg
drawCard model =
  case model.deck of
    (first::rest) -> Random.generate NewCard (Random.uniform first rest)
    _ -> Cmd.none

{-
  1. Get the value of each card (use `cardValue`)
  2. Generate a list of all possible scores
  3. Return the score closest to 21 (less than 21!), if one exists, else the smallest score over 21
  ```elm
  calculateScore [(King, Hearts)] == 10
  calculateScore [(Two, Hearts)] == 2
  calculateScore [(Two, Hearts), (King, Spades)] == 12
  calculateScore [(Ace, Hearts), (King, Spades)] == 21
  calculateScore [(Ace, Hearts), (Five, Hearts), (Seven, Spades)] == 13
  calculateScore [(King, Hearts), (Five, Hearts), (Seven, Spades)] == 22
  calculateScore [(King, Hearts), (Ten, Clubs), (Ace, Spades)] == 21
  calculateScore [(Ace, Spades), (Ace, Clubs), (Ten, Clubs), (King, Clubs)] == 22
  ```
-}
calculateScore : List Card -> Int
calculateScore cards =
  let
    scoreList scores lcards =
      case lcards of
        [] -> scores
        x :: xs ->
            scoreList (
              List.concat (
                List.map (\cardVal ->
                  List.map (\score -> score + cardVal) scores
                 ) (cardValue x)
              )
            ) xs

    closestToVal choosenVal val scores =
        case scores of
            [] -> choosenVal
            x :: xs ->
                if x > val then
                    if choosenVal /= 0 then
                        choosenVal
                    else x
                else closestToVal x val xs
  in
    closestToVal 0 21 (scoreList [0] cards)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

{-
  Use the `viewCard` function for showing the player's hand and the `cardToUnicode` function for the remaining deck.
-}
view : Model -> Html Msg
view model =
  let
    appName = "Blackjack"
    score = calculateScore model.hand
    deckText =
      model.deck
        |> List.map cardToUnicode
        |> List.map (\x -> text x) 
  in
    div []
      [ div [] [ h1 [] [text appName] ]
      , h2 [] [text ("Score: " ++ String.fromInt score)]
      , button [onClick Draw] [text "Draw card"]
      , button [onClick ToogleDeck] [text "Show deck"]
      , div [Html.Attributes.hidden (not model.showDeck), style "font-size" "2.8em"] deckText
      , div [] (List.map viewCard model.hand)
      , div [Html.Attributes.hidden (not (score == 21)), style "font-size" "4em"] [text "You won!"]
      , div [Html.Attributes.hidden (not (score > 21)), style "font-size" "4em"] [text "You lost!"]
      ]