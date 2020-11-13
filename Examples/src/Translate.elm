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
-- keep words on a single line so we can easily sort them
words =
    [("dog",[(Tamil,"நா`ய்"),(Hindi,"कु`त्ता")])
    ,("cat",[(Tamil,"பூ`னை"),(Hindi,"बि`ल्ली")])
    , ("woman",[(Tamil,"பெ`ண்"),(Hindi,"म`हि`ला")])
    , ("eating",[(Tamil,"சா`ப்`பி`டு`வ`து"),(Hindi,"खा`ना")])
    , ("sunset",[(Tamil,"சூ`ரி`ய`ஸ்`த`ம`ம்"),(Hindi,"सू`र्या`स्त")])
    ,   ("grandfather"
        ,   [(English,"gr`a`nd`f`a`th`er")
            ,(Tamil,"தா`த்`தா")
            ,(Hindi,"दा`दा")])
    ,   ("bunny",   [(English,"b`u`nn`y")    ,(Tamil,"மு`ய`ல்")    ,(Hindi,"क`र`गो`श")])
    {- TODO
    ,   ("apple tree",   [(English,"apple tree")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("ball",   [(English,"ball")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("ball",   [(English,"ball")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("baloon",   [(English,"baloon")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("beach",   [(English,"beach")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("bear",   [(English,"bear")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("boat",   [(English,"boat")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("bow",   [(English,"bow")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("bunny",   [(English,"b`u`nn`y")    ,(Tamil,"மு`ய`ல்")    ,(Hindi,"क`र`गो`श")])
    ,   ("bus",   [(English,"bus")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("butterfly",   [(English,"butterfly")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("cake",   [(English,"cake")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("cat",     [(English,"c`a`t")    (Tamil,"பூ`னை"),(Hindi,"बि`ल्ली")])
    ,   ("clock",   [(English,"clock")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("clown",   [(English,"clown")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("clown",   [(English,"clown")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("dog",     [(English,"d`o`g")    ,(Tamil,"நா`ய்")     ,(Hindi,"कु`त्ता")])
    ,   ("dress",   [(English,"dress")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("duck",   [(English,"duck")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("eating",   [(English,"eating")    ,(Tamil,"சா`ப்`பி`டு`வ`து")     ,(Hindi,"खा`ना")])
    ,   ("face",   [(English,"face")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("ghost",   [(English,"ghost")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("hand",   [(English,"hand")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("happy",   [(English,"happy")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("house",   [(English,"house")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("houses",   [(English,"houses")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("koala",   [(English,"koala")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("koala",   [(English,"koala")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("pencil",   [(English,"pencil")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("penguin",   [(English,"penguin")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("phone",   [(English,"phone")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("picklemobile",   [(English,"p`i`ck`le`m`o`b`i`le")    ,(Tamil,"")    ,(Hindi,"क`क`ड़ी`वा`ह`न")])
    ,   ("popcorn",   [(English,"popcorn")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("rainbow",   [(English,"rainbow")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("rainbow",   [(English,"rainbow")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("snail",   [(English,"snail")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("snowman",   [(English,"snowman")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("sunset",   [(English,"sunset")    ,(Tamil,"சூ`ரி`ய`ஸ்`த`ம`ம்")      ,(Hindi,"सू`र्या`स्त")])
    ,   ("wave",   [(English,"wave")    ,(Tamil,"")    ,(Hindi,"")])
    ,   ("woman",   [(English,"woman")    ,(Tamil,"பெ`ண்"),    (Hindi,"औ`र`त")])
    -}
    ]
    {- template
    ,   ("",   [(English,"")    ,(Tamil,"")    ,(Hindi,"")])
    -}

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
