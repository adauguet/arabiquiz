module Digit exposing (Digit, fromChar, generate, toArabic)

import Random exposing (uniform)


type Digit
    = One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Zero



-- helpers


generate : (Digit -> msg) -> Cmd msg
generate m =
    Random.generate m (uniform Zero [ One, Two, Three, Four, Five, Six, Seven, Eight, Nine ])


fromChar : Char -> Maybe Digit
fromChar c =
    case c of
        '1' ->
            Just One

        '2' ->
            Just Two

        '3' ->
            Just Three

        '4' ->
            Just Four

        '5' ->
            Just Five

        '6' ->
            Just Six

        '7' ->
            Just Seven

        '8' ->
            Just Eight

        '9' ->
            Just Nine

        '0' ->
            Just Zero

        _ ->
            Nothing


toArabic : Digit -> String
toArabic d =
    case d of
        Zero ->
            "٠"

        One ->
            "١"

        Two ->
            "٢"

        Three ->
            "٣"

        Four ->
            "٤"

        Five ->
            "٥"

        Six ->
            "٦"

        Seven ->
            "٧"

        Eight ->
            "٨"

        Nine ->
            "٩"
