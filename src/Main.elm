module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Process
import Task
import Browser.Events
import Json.Decode as Decode
import Html exposing (Html)
import Html.Attributes
import Element as Ui
import Element exposing ( row, column, fill, fillPortion, width, height
                        , centerX, centerY, paddingXY, spacingXY, spacing
                        )
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Array

import Text
import IO


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


type alias Model =
    { mode: EditMode
    , readingStatus: ReadingStatus
    , wpm : Int
    , readUntil : Maybe Int
    , currentWord : Maybe String
    , buffer : Text.Text
    , command : Maybe Text.Text
    }


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    (
    { mode = Normal
    , readingStatus = Stop
    , wpm = 500
    , readUntil = Nothing
    , currentWord = Nothing
    , buffer = Text.fromString sampleText
    , command = Nothing
    }
    ,
    Cmd.none
    )


type Msg
    = Noop
    | ReadTick Int Int
    | KeyPressed KeybActions
    | FileRead ( Result String String )


type EditMode
    = Normal
    | Insert
    | Command


type ReadingStatus
    = GoOn
    | Stop


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> ( model, Cmd.none )
    ReadTick from until -> case model.readingStatus of
        Stop ->
            ( { model | currentWord = Nothing }, Cmd.none )
        GoOn ->
            let
                stopReading = ( { model | currentWord = Nothing
                                        , readingStatus = Stop }
                              , Cmd.none
                              )
            in
            case nextWordAndIndex ( from, until ) model.buffer of
                Just ( nextIdx, nextWord) ->
                    if nextIdx < until then
                        ( { model | currentWord = Just nextWord }
                        , delay (wpmToMilis model.wpm) <|
                            ReadTick nextIdx until
                        )
                    else
                        stopReading
                _ -> stopReading
    KeyPressed action -> case action of
        ReadLine ->
            readUntil
                model.buffer.cursorIdx
                ( getUntilIdx model.buffer.cursorIdx model.buffer ['\n'] )
                model
        NextWord ->
            readAndJumpUntil model Text.softWordDelimiters
        _ -> ( model, IO.requestRead "hi" )
    FileRead response ->
        case response of
            Ok content -> Debug.log content (model, Cmd.none)
            Err error -> Debug.log error (model, Cmd.none)


getUntilIdx : Int -> Text.Text -> List Char -> Int
getUntilIdx from text delims = Maybe.withDefault
    ( Array.length text.rawText )
    ( Text.findCharsFromIn ( from + 1) text.rawText delims )


readUntil : Int -> Int -> Model -> (Model, Cmd Msg)
readUntil from until model =
        update ( ReadTick from until )
            { model | readingStatus =
                if model.readingStatus == GoOn then Stop
                else GoOn
            }

moveCursor : Int -> Text.Text -> Text.Text
moveCursor to text = { text | cursorIdx = to }


readAndJumpUntil : Model -> List Char -> ( Model, Cmd Msg )
readAndJumpUntil model delims =
    let
        until =
            getUntilIdx model.buffer.cursorIdx model.buffer delims
        ( newModel, command ) = readUntil
                                    model.buffer.cursorIdx
                                    until
                                    model
    in
        ( { newModel | buffer = moveCursor until model.buffer }
        , command
        )


nextWordAndIndex : ( Int, Int ) -> Text.Text -> Maybe ( Int, String )
nextWordAndIndex ( from, until ) text =
    let
        nextIdx = Text.nextWordStart from text.rawText Text.softWordDelimiters
        nextWord = nextIdx |> Maybe.andThen ( \idx ->
            Text.getWordStrAt idx text.rawText Text.softWordDelimiters )
    in
        case (nextIdx, nextWord) of
            (Just idx, Just word) -> Just ( idx, word )
            _ -> Nothing


delay : Float -> msg -> Cmd msg
delay ms msg = Process.sleep ms |> Task.perform (\_ -> msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyPressed
                <| Browser.Events.onKeyDown
                <| keyDecoder model.mode,
                IO.receiveRead <| FileRead << IO.decodeRead
              ]


keyDecoder : EditMode -> Decode.Decoder KeybActions
keyDecoder mode = Decode.map (processKeyb mode) (Decode.field "key" Decode.string)


processKeyb : EditMode -> String -> KeybActions
processKeyb mode rawKey =
    case mode of
        Normal -> case rawKey of
            " " ->
              ReadLine
            "l" ->
              NextWord
            "h" ->
              PrevWord
            ":" ->
              EnterCommandMode
            _ ->
              DoNothing
        _ -> case rawKey of
            "Escape" -> ExitTypingMode
            _ -> RawKey rawKey


type KeybActions = ReadLine
                 | NextWord
                 | PrevWord
                 | EnterCommandMode
                 | EnterEditMode
                 | ExitTypingMode
                 | DoNothing
                 | RawKey String


wpmToMilis : Int -> Float
wpmToMilis wpm = (60 / toFloat wpm) * 1000


onUrlChange : Url -> Msg
onUrlChange url = Noop


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest url = Noop


view : Model -> Browser.Document Msg
view model = { title = "RAYC"
             , body = [body model]
             }


body : Model -> Html Msg
body model =
    Ui.layout [] <| mainLayout model


ex : Ui.Attribute Msg
ex = Ui.explain Debug.todo


mainLayout : Model -> Ui.Element Msg
mainLayout model =
    column [ width fill
           , height fill
           , paddingXY 5 5
           , spacingXY 0 3
           , Background.color (Ui.rgb255 61 61 61)
           , Font.family
                [ Font.external
                    { name = "Montserrat"
                    , url = "https://fonts.googleapis.com/css?family=Montserrat"
                    }
                , Font.sansSerif
                ]
           , Font.color (Ui.rgb255 230 230 230)
           ]
           [ menuLayout model
           , row [ width fill, height (fillPortion 1) ] []
           , row [ width fill ] [  wordLayout model ]
           , row [ width fill, height (fillPortion 3) ] []
           , footerLayout model
           ]


centeredElem : Ui.Element Msg -> List (Ui.Element Msg)
centeredElem element = [ column [ width fill ] []
                       , column [ centerY ] [ element ]
                       , column [ width fill ] []
                       ]


menuLayout : Model -> Ui.Element Msg
menuLayout model = row [ width fill
                       , paddingXY 10 10
                       , Background.color (Ui.rgb255 61 61 61)
                       , spacing 10
                       , Border.rounded 8
                       , Ui.htmlAttribute <| Html.Attributes.style
                            "box-shadow"
                            "5px 5px 10px #323232, -5px -5px 10px #484848"
                       ]
                       [ Ui.text "Menu TODO" ]


footerLayout : Model -> Ui.Element Msg
footerLayout model = row [ width fill
                         , paddingXY 10 10
                         , Background.color (Ui.rgb255 61 61 61)
                         , spacing 10
                         , Border.rounded 8
                         , Ui.htmlAttribute <| Html.Attributes.style
                            "box-shadow"
                            "5px 5px 10px #323232, -5px -5px 10px #484848"
                         ]
                         [ Ui.text "Copyright"]


wordLayout : Model -> Ui.Element Msg
wordLayout model =
    let
        wordParts =
            Maybe.withDefault
                { left = "", center = "🕮", right = "" }
                <| divideWord model.currentWord
    in
    column [ width fill, height fill ]
           [ row [ width fill ] <| centeredElem
                                <| Ui.el [ Font.color <| Ui.rgb255 255 0 0]
                                <| Element.text <| topMarkerSymbol
           , row [ centerX
                 , centerY
                 , width fill
                 ]
                 [ column [ width fill ]
                          [ Ui.paragraph [Font.alignRight] [Ui.text wordParts.left] ]
                 , column [ Font.color (Ui.rgb255 255 0 0) ]
                          [ Ui.paragraph [Font.center] [Ui.text wordParts.center] ]
                 , column [ width fill ]
                          [ Ui.paragraph [Font.alignLeft] [Ui.text wordParts.right] ]
                 ]
           , row [ width fill ] <| centeredElem
                                <| Ui.el [ Font.color <| Ui.rgb255 255 0 0]
                                <| Element.text <| bottomMarkerSymbol
           ]


type alias WordParts =
    { left : String
    , center : String
    , right : String
    }


divideWord : Maybe String -> Maybe WordParts
divideWord maybeWord = maybeWord |> Maybe.map
    ( \word ->
        let
            centerIdx = computeCenter word
        in
            { left = String.slice 0 centerIdx word
            , center = String.slice centerIdx (centerIdx + 1) word
            , right = String.slice (centerIdx + 1) (String.length word)  word
            }

    )


topMarkerSymbol : String
topMarkerSymbol = "⥾"


bottomMarkerSymbol : String
bottomMarkerSymbol = "⥿"


computeCenter : String -> Int
computeCenter word =
    let
        wordLen = String.length word
    in
    if wordLen <= 2 then
        0
    else if modBy wordLen 2 == 0 then
        round <| (toFloat wordLen) / 2 - 2
    else
        round <| (toFloat wordLen) / 2 - 2


sampleText : String
sampleText = """def name():
    n1 = "John"
    n2 = "Armin"

    return {1:n1, 2:n2}

names = name()
print(names)
"""
