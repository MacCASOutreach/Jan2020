{- 
  Choice of Four Game
-}
module Choose4 exposing (..)

import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import Random
import Array
import Dict exposing (Dict)
import MultilingualFromElmJr2019May15

import Translate exposing (Language(..), dictFor)

words = MultilingualFromElmJr2019May15.words

{-headerMake your own WordGame.-}

{-editable-}
myTitle = "Choose4"
timeToDrop = 1
halfToAppear = 5
timeToAppear = 2 * halfToAppear
letterWidth = 7
dScale = 0.4375 -- 7/16 < 1/2

type alias Model = { time : Float
                   , animation : GuessState
                   , score : Int
                   , pics : List ( { time : Float } -> List (Shape Msg) )
                   , correct : Pick
                   , letters : List (Float,String)
                   , school : String
                   , username : String
                   , nextRand : (List Int, Int)
                   , badTime : Float
                   , debug : String
                   , dict : Dict String (List (Float,String))
                   }
init : Model
init = { time = 0
       , animation = Startup
       , score = 0
       , pics = [ \ _ -> [],  \ _ -> [],  \ _ -> [],  \ _ -> []]
       , correct = Pick1
       , letters = List.indexedMap (\ idx c -> (toFloat idx - 1, String.fromChar c)) ['n','o','t',' ','r','e','a','d','y']
       , school = ""
       , username = ""
       , nextRand = ([-1,-1,-1,-1],0)
       , badTime = pi -- time for the bad-pick animation
       , debug = ""
       , dict = dictFor Tamil
       }
-- phase of the game
type GuessState = Waiting              -- waiting for click
                | BadPick Pick Float   -- animating bad pick, with time in seconds left
                | GoodPick Float       -- animating good pick, with time in seconds left
                | Startup              -- waiting for the first random numbers
type Msg = Tick Float GetKeyState
         | RandIdx (List Int, Int)  -- four pics to display and 0..3 for the one which will be correct
         | Choice Pick              -- message for button
-- type allowing 4 "picks"
type Pick = Pick0 | Pick1 | Pick2 | Pick3
-- function to show the message to help us understand our program
toString msg = case msg of
                  Tick t _ -> "Tick "++String.fromFloat t
                  RandIdx (idxs,idx) -> "RandIdx["++String.fromInt idx++"] "++(String.concat <| List.intersperse "," <| List.map String.fromInt idxs)
                  Choice Pick0 -> "Choice 0"
                  Choice Pick1 -> "Choice 1"
                  Choice Pick2 -> "Choice 2"
                  Choice Pick3 -> "Choice 3"
-- convert a (random) Int to a Pick
int2Pick i = case i of
               1 -> Pick1
               2 -> Pick2
               3 -> Pick3
               _ -> Pick0
-- get one of the first 4 things from the word list, otherwise make it blank
listPick idx lst = case (idx,lst) of
                     (0, x :: _ ) -> x
                     (1, _ :: x :: _ ) -> x
                     (2, _ :: _ :: x :: _ ) -> x
                     (3, _ :: _ :: _ :: x :: _ ) -> x
                     (_, _) -> (\ _ -> [], ("","",""))
-- where the different picks will show up on the screen
pickPositions = [ (Pick0, (-42, 36))
                , (Pick1, ( 42, 36))
                , (Pick2, (-42,-36))
                , (Pick3, ( 42,-36))
                ]
update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
 let model = case msg of
               Tick _ _ -> m
               _ -> { m | debug = toString msg }
 in
  case msg of
    RandIdx (idxsBad,correct) ->
      let
          idxs = removeDups [] idxsBad

          -- WARNING will recurse infinitely if arrayLength < 4
          removeDups uniques next =
            case next of
              [] -> uniques
              n :: ns -> removeDups ((fix uniques n)::uniques) ns

          fix uniques n = if List.member n uniques
                          then if n+1 == arrayLength then fix uniques 0 else fix uniques (n+1)
                          else n

      in
          if model.animation == Startup
                                then let
                                        fourWords = List.map getWord idxs
                                        (un,w,s) = Tuple.second <| listPick correct fourWords
                                        -- if we have a translation, use that
                                        ww = case Dict.get w init.dict of
                                              Just translated -> translated 
                                              Nothing -> Tuple.second <| stringToChars w 
                                     in
                                       ( { model | animation = Waiting
                                                 , correct = int2Pick correct
                                                 , pics = List.map Tuple.first fourWords
                                                 , username = un
                                                 , letters = ww
                                                 , school = s
                                         }
                                       , getRandChoices )

                                else ( { model | nextRand = (idxs,correct) }, Cmd.none )

    Tick t _ ->
      let lastTime = if model.time > 0 then model.time else t
      in case model.animation of
           Startup ->  ( { model | time = t
                                 , animation = Startup
                         }
                       , Cmd.none )
           Waiting ->  ( { model | time = t
                               , animation = Waiting
                         }
                       , Cmd.none )
           BadPick pick tLeft
             -> if tLeft < t - lastTime
                  then ( { model | time = t
                               , animation = Waiting
                         }
                       , Cmd.none )
                  else ( { model | time = t
                               , animation = BadPick pick (tLeft - (t - lastTime))
                         }
                       , Cmd.none )
           GoodPick tLeft
             -> if tLeft < t - lastTime
                  then let
                          (idxs,correct) = model.nextRand
                          fourWords = List.map getWord idxs
                          (un,w,s) = Tuple.second <| listPick correct fourWords
                          -- if we have a translation, use that
                          ww = case Dict.get w init.dict of
                                Just translated -> translated 
                                Nothing -> Tuple.second <| stringToChars w
                       in
                         ( { model | animation = Waiting
                                   , correct = int2Pick correct
                                   , pics = List.map Tuple.first fourWords
                                   , username = un
                                   , letters = ww
                                   , badTime = model.badTime * 0.9
                                   , school = s
                                   , time = t
                           }
                         , getRandChoices )
                  else ( { model | time = t
                               , animation = GoodPick (tLeft - (t - lastTime))
                         }
                       , Cmd.none )

    Choice pick
      -> case model.animation of
           Waiting
             -> if Debug.log (toString <| Choice pick) pick == model.correct
                  then
                    ( { model | animation = GoodPick <| model.badTime, score = model.score + 1 }, getRandChoices )
                  else
                    ( { model | animation = BadPick pick model.badTime }, Cmd.none )
           other -> ( model , Cmd.none )
view : Model -> Collage Msg
view model = collage 192 128
  <| ( case model.animation of
           Startup ->  [ text "Read the word and tap on the picture!" |> size 20 |> filled (rgb 255 0 255) ]
           Waiting ->  ( List.map2 (drawPic model.time) model.pics pickPositions )
                       ++
                       ( List.map (oneLetter 0 1) model.letters)
                       ++ ( List.map2 (drawMsg model.time) model.pics pickPositions )

           BadPick pick tLeft
                   ->  ( List.map2 (drawBad pick tLeft) model.pics pickPositions )
                       ++
                       ( List.map (oneLetter 0 1) model.letters)
           GoodPick tLeft
                   ->  ( List.map2 (drawGood model.correct (max 0 (2 * (model.badTime - tLeft) / model.badTime)) model.time)
                                   model.pics
                                   pickPositions )
                       ++
                       ( List.map (oneLetter tLeft model.badTime) model.letters)
                       ++
                       [ "Score: " ++ String.fromInt model.score |> text |> bold |> fixedwidth |> filled (rgb (125 + 125 * sin (20 * model.time)) 0 0) |> move (10,-4) ]
     ) ++[ "Score: " ++ String.fromInt model.score |> text |> fixedwidth |> filled black |> move (10,-4)
         -- when you make your own game, you can use this to display debug information
         --, model.debug |> text |> filled blue |> move (-86,-20)
         ]
drawMsg t pic (msg, pos) = group [ rect 80 55 |> filled (rgba 0 0 0 0) |> move pos |> notifyTap (Choice msg)                                    ]
drawPic t pic (msg, pos) = (group <| pic { time = t }) |> clip (rect 192 128 |> ghost) |> scale dScale |> move pos
drawBad pick t pic (msg, pos) = if pick == msg
                                  then ((group <| pic { time = t }) |> move (10 * cos (30*t),0)) |> clip (rect 192 128 |> ghost) |> scale dScale |> move pos
                                  else (group <| pic { time = t }) |> clip (rect 192 128 |> ghost) |> scale dScale |> move pos
drawGood pick frac t pic (msg, pos) = if pick == msg
                                  then ((group <| pic { time = t })) |> clip (rect 192 128 |> ghost) |> scale dScale |> move pos
                                  else (group <| pic { time = t } ++ [rect 192 128 |> filled (rgba 255 255 255 frac)] ) |> clip (rect 192 128 |> ghost) |> scale dScale |> move pos
getRandChoices = Random.generate RandIdx (Random.map5 ( \ i j k l c -> ([i,j,k,l],c) ) oneRandIdx oneRandIdx oneRandIdx oneRandIdx rand0to3)
oneRandIdx = Random.int 0 (arrayLength - 1)
rand0to3   = Random.int 0 3
{-endeditable-}

{-viewable-}
-- this is an array of all the words tuples, so you can access them in any order
wordArray = Array.fromList words
arrayLength = List.length words

getWord idx = case Array.get idx wordArray of
                Just ((username,word,_),(_,school,pic)) -> (pic,(username,word,school))
                Nothing                             -> (\ _ -> [],("","no word",""))

main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init  -- this is the initial state, like you are used to
                        , getRandChoices)-- this requests the first random number
        , update = update
        , view = \ model -> { title = myTitle, body = view model }
        , subscriptions = \_ -> Sub.none
        }

-- turn a string into a list of characters to make it possible to drop letters not the whole word
stringToChars : String -> (Float,List (Float,String))
stringToChars s = String.foldr (\ c (soFar,cs) -> (soFar + 1, (soFar,String.fromChar c) :: cs )) (0,[]) s
-- this function draws one letter, shifted over by the idx number of letters
-- use it with List.indexedMap to display a list of letters
oneLetter : Float -> Float -> (Float,String) -> Shape a
oneLetter tLeft bt (pos,letterChunk) = text letterChunk |> fixedwidth |> filled black
                         |> rotate (9 * tLeft / bt)
                         |> move (-pos * letterWidth - 50, -4)

{-endviewable-}
