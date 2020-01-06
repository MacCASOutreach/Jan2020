module Translate exposing (..)

import List
import Dict exposing (Dict)

type Language
    = English
    | French
    | Tamil
    | Hindi

{-
If you want to help with translations, we need to tranlate the words which appear in the 
Wordathon submissions into letter groupings.  This was asked for by English teachers,
to group letters by phonemes, and it is necessary for abugida or alphasyllabary languages.

Phonemes are broken by ` characters, and lengths are calculated using String.count,
but this doesn't seem to be an adequate solution.

FIXME: how do we better handle different lengths?  Use ` with an optional digit for length?
-}
words =
    [("dog",[(Tamil,"நா`ய்"),(Hindi,"कु`त्ता")])
    ,("cat",[(Tamil,"பூ`னை"),(Hindi,"बि`ल्ली")])
    ,   ("grandfather"
        ,   [(English,"gr`a`nd`f`a`th`er")
            ,(Tamil,"தா`த்`தா")
            ,(Hindi,"दा`दा")])
    ]

dictFor : Language -> Dict String (List (Float,String))
dictFor language =
    List.concatMap (select language) words
        |> Dict.fromList

select language (english,languages) =
    case List.filter ( \ (l,_) -> language == l ) languages of
        (_,target)::_ -> [(english, splitWithLengths target)]
        otherwise -> []

splitWithLengths : String -> List (Float,String)
splitWithLengths s 
    = Tuple.second 
        <| List.foldr (\ c (soFar,cs) -> (soFar + (String.length c |> toFloat), (soFar, c) :: cs )) (0,[])
        <| String.split "`" s
