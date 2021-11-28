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
    , text : String
    , cursorPos : Int
    , displayWord : Maybe String
    , command : Maybe Text.Text
    }


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    (
    { mode = Normal
    , readingStatus = Stop
    , wpm = 500
    , text = sampleText
    , cursorPos = 0
    , displayWord = Nothing
    , command = Nothing
    }
    ,
    Cmd.none
    )


type Msg
    = Noop
    | ReadTick
    | KeyPressed KeybActions
    -- | FileRead ( Result String String )


type EditMode
    = Normal
    | Insert
    | Command


type ReadingStatus
    = GoOn (Array.Array String) Int
    | Stop


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of

    Noop -> ( model, Cmd.none )

    ReadTick -> case model.readingStatus of
        Stop ->
            ( { model | displayWord = Nothing }, Cmd.none )
        GoOn phrase index ->
            let
                mDisplayWord = Array.get index phrase
            in
                case mDisplayWord of
                    Nothing ->
                        ( { model | readingStatus = Stop }
                        , delay (wpmToMilis model.wpm) ReadTick
                        )
                    Just displayWord ->
                        ( { model
                          | readingStatus = GoOn phrase ( index + 1 )
                          , displayWord = Just displayWord
                          }
                        , delay (wpmToMilis model.wpm) ReadTick
                        )

    KeyPressed action -> case action of
        ReadLine ->
            case model.readingStatus of
                Stop ->
                    update ReadTick
                        { model
                        | readingStatus =
                                GoOn (nextPhrase model.cursorPos ['\n'] model.text) 0
                        }
                GoOn _ _ ->
                    ( { model | readingStatus = Stop }, Cmd.none )
        NextWord ->
            update ReadTick <|
                moveCursorAndReadLine model Text.softWordDelimiters

        NextLine ->
            update ReadTick <|
                moveCursorAndReadLine model ['\n']

        _ -> ( model, Cmd.none )

    -- FileRead response ->
    --     case response of
    --         Ok content -> Debug.log content (model, Cmd.none)
    --         Err error -> Debug.log error (model, Cmd.none)


moveCursorAndReadLine : Model -> List Char -> Model
moveCursorAndReadLine oldModel delims =
    let
        newModel = moveCursorTo oldModel delims
    in
        { newModel
        | readingStatus =
            GoOn (nextPhrase newModel.cursorPos ['\n'] newModel.text) 0
        }


nextPhrase : Int -> List Char -> String -> Array.Array String
nextPhrase from untilDelims text = 
    Array.fromList
    <| String.words
    <| takeUntil (\char -> List.member char untilDelims)
    <| String.dropLeft from text


moveCursorTo : Model -> List Char -> Model
moveCursorTo model delims =
    { model
    | cursorPos =
        Maybe.withDefault ( String.length model.text - 1)
        <| indexWhere model.cursorPos ( \char -> List.member char delims) model.text
    }


takeUntil : (Char -> Bool) -> String -> String
takeUntil cond string = takeUntilHelper cond "" string


takeUntilHelper : (Char -> Bool) -> String -> String -> String
takeUntilHelper cond head tail = 
    let
        maybeUncons = String.uncons tail
    in
        Maybe.withDefault head <| Maybe.map
        ( \uncons ->
            let
                ( nextChar, nextTail) = uncons
                nextHead = head ++ ( String.fromChar nextChar )
            in
                if cond nextChar then head
                else takeUntilHelper cond nextHead nextTail
        ) maybeUncons


indexWhere : Int -> (Char -> Bool) -> String -> Maybe Int
indexWhere from cond string =
    indexWhereHelper cond ( String.dropLeft from string ) 0 False|>
    Maybe.map ( \res -> from + res )


indexWhereHelper : (Char -> Bool) -> String -> Int -> Bool -> Maybe Int
indexWhereHelper cond string counter found =
    String.uncons string |> Maybe.andThen ( \uncons ->
        let
            (head, tail) = uncons
            nextStep = indexWhereHelper cond tail ( counter + 1 )
        in
            if not found then
                if cond head then nextStep True
                else nextStep False
            else
                if cond head then nextStep True
                else Just counter
    )


delay : Float -> msg -> Cmd msg
delay ms msg = Process.sleep ms |> Task.perform (\_ -> msg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyPressed
                <| Browser.Events.onKeyDown
                <| keyDecoder model.mode
                -- IO.receiveRead <| FileRead << IO.decodeRead
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
            "j" ->
              NextLine
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
                 | NextLine
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
                <| divideWord model.displayWord
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
