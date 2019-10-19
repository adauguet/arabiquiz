module Main exposing (main)

import Browser exposing (Document)
import Browser.Events
import Css
    exposing
        ( alignItems
        , backgroundColor
        , center
        , column
        , displayFlex
        , flexDirection
        , flexGrow
        , fontSize
        , int
        , justifyContent
        , rem
        , rgb
        )
import Digit exposing (Digit)
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Json.Decode as D exposing (Decoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, Digit.generate GotRandomDigit )



-- model


type Model
    = Loading
    | Question Digit



-- update


type Msg
    = GotRandomDigit Digit
    | OnDigitPressed Digit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
    case msg of
        GotRandomDigit digit ->
            ( Question digit, Cmd.none )

        OnDigitPressed digit ->
            case model of
                Loading ->
                    ( model, Cmd.none )

                Question d ->
                    if d == digit then
                        ( Loading, Digit.generate GotRandomDigit )

                    else
                        ( model, Cmd.none )



-- view


content : Model -> List (Html Msg)
content model =
    case model of
        Loading ->
            [ text "Loading" ]

        Question digit ->
            [ div
                [ css
                    [ flexGrow (int 1)
                    , displayFlex
                    , flexDirection column
                    , justifyContent center
                    , alignItems center
                    ]
                ]
                [ div
                    [ css [ fontSize (rem 10) ] ]
                    [ Digit.toArabic digit |> text ]
                ]
            ]


view : Model -> Document Msg
view model =
    { title = ""
    , body = content model |> List.map toUnstyled
    }



-- main


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Browser.Events.onKeyPress keyDecoder
        }


keyDecoder : Decoder Msg
keyDecoder =
    D.field "key" D.string
        |> D.andThen
            (\string ->
                case String.uncons string of
                    Just ( char, "" ) ->
                        case Digit.fromChar char of
                            Just digit ->
                                D.succeed (OnDigitPressed digit)

                            Nothing ->
                                D.fail "failed to decode digit"

                    _ ->
                        D.fail "failed to decode char"
            )
