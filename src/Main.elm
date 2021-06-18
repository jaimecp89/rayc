module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Browser.Dom
import Url exposing (Url)
import Element exposing (Element, text, row, column, fill, width, height, rgb255, centerX, centerY, explain)
import Array exposing (fromList, Array, get)
import String exposing (words)
import Time
import Browser.Events
import Json.Decode as Decode
import Html exposing (Html)
import Element exposing (paragraph)
import Element.Font
import Element
import Element exposing (fillPortion)
import Element.Background
import Element


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


init : flags -> Url -> Key -> (Model, Cmd msg)
init flags url key = ({ words = fromList <| words sampleText
                      , wpm = 300
                      , currentIndex = 0
                      , mode = Pause
                      , wordElems = { left = Nothing
                                    , center = Nothing
                                    , right = Nothing
                                    }
                      } , Cmd.none )


type alias Model = { words: Array String
                   , wpm: Int 
                   , currentIndex: Int
                   , mode: PlayMode
                   , wordElems: { left: Maybe Browser.Dom.Element
                                , center: Maybe Browser.Dom.Element
                                , right: Maybe Browser.Dom.Element}
                   }


type Msg = Noop
         | NextWord
         | KeyPressed Keys


type PlayMode = Pause |
                Play |
                Backwards


view : Model -> Browser.Document Msg
view model = { title = "RAYC"
             , body = [body model]
             }


body : Model -> Html msg
body model =
    Element.layout [] <| mainLayout model


ex : Element.Attribute msg
ex = explain Debug.todo


mainLayout : Model -> Element msg
mainLayout model =
    column [ width fill, height fill ]
        [ menuLayout model
        , row [ width fill, height (fillPortion 1) ] []
        , row [ width fill ] [  wordLayout <| currentWord model ]
        , row [ width fill, height (fillPortion 3) ] []
        , footerLayout model
        ]


menuLayout : Model -> Element msg
menuLayout model = row [ width fill
                       , Element.paddingXY 10 10
                       , Element.Background.color (rgb255 57 69 81)
                       ]
                       [ Element.text "Hi"]


footerLayout : Model -> Element msg
footerLayout model = row [ width fill
                         , Element.paddingXY 10 10
                         , Element.Background.color (rgb255 57 69 81)
                         ]
                         [ Element.text "Copyright"]


currentWord : Model -> String
currentWord model = Maybe.withDefault "" <| (get model.currentIndex model.words)


wordLayout : String -> Element msg
wordLayout word =
    let
        centerIdx = computeCenter word
        leftPart = String.slice 0 centerIdx word
        centerChar = String.slice centerIdx (centerIdx + 1) word
        rightPart = String.slice (centerIdx + 1) (String.length word)  word
    in
    row [ centerX
        , centerY
        , width fill
        ]
        [ column [ width fill ]
                 [ paragraph [Element.Font.alignRight] [text leftPart] ]
        , column [ Element.Font.color (rgb255 255 0 0) ]
                 [ paragraph [Element.Font.center] [text centerChar] ]
        , column [ width fill ]
                 [ paragraph [Element.Font.alignLeft] [text rightPart] ]
        ]


computeCenter : String -> Int
computeCenter word =
    let
        wordLen = String.length word
    in
    if modBy wordLen 2 == 0
        then round <| (toFloat wordLen) / 2 - 1
        else round <| (toFloat wordLen) / 2 - 1


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> (model, Cmd.none)
    NextWord -> ( if model.mode == Play then
                    { model | currentIndex =
                        if model.currentIndex < (Array.length model.words)
                        then model.currentIndex + 1 else
                        0 }
                  else model
                , Cmd.none
                )
    KeyPressed key ->
        case key of
            Space ->
                case model.mode of
                    Play -> ({ model | mode = Pause }, Cmd.none)
                    Pause -> ({ model | mode = Play }, Cmd.none)
                    Backwards -> (model, Cmd.none)
            _ -> (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every (toFloat <| wpmToMilis model.wpm) (\_ -> NextWord)
              , Sub.map KeyPressed <| Browser.Events.onKeyDown keyDecoder 
              ]


keyDecoder : Decode.Decoder Keys
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


type Keys = Space
          | Other


toKey : String -> Keys
toKey string =
  case string of
    " " ->
      Space
    _ ->
      Other


wpmToMilis : Int -> Int
wpmToMilis wpm = round <| (60 / toFloat wpm) * 1000


onUrlChange : Url -> Msg
onUrlChange url = Noop


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest url = Noop


sampleText : String
sampleText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
