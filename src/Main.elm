module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Browser.Dom
import Url exposing (Url)
import Element exposing ( Element, text, row, column, fill, width, height
                        , rgb255, centerX, centerY, explain )
import Array exposing (fromList, Array, get)
import String exposing (words)
import Time
import Browser.Events
import Json.Decode as Decode
import Html exposing (Html)
import Html.Attributes
import Element exposing (paragraph)
import Element.Font
import Element
import Element exposing (fillPortion)
import Element.Background
import Element
import Element.Input
import Element
import Element.Border
import Element exposing (spacingXY)
import Element exposing (paddingXY)
import Element exposing (el)


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


type alias Model = { words: Array String
                   , wpm: Int 
                   , currentIndex: Int
                   , mode: Mode
                   , playMode: PlayMode
                   }


init : flags -> Url -> Key -> (Model, Cmd Msg)
init flags url key = ({ words = fromList <| words sampleText
                      , wpm = 600
                      , currentIndex = 0
                      , mode = ModeRead
                      , playMode = Pause
                      } , Cmd.none )


type Msg = Noop
         | TimerTick
         | KeyPressed KeybActions


type PlayMode = Pause |
                Play |
                Backwards


view : Model -> Browser.Document Msg
view model = { title = "RAYC"
             , body = [body model]
             }


body : Model -> Html Msg
body model =
    Element.layout [] <| mainLayout model


ex : Element.Attribute Msg
ex = explain Debug.todo


mainLayout : Model -> Element Msg
mainLayout model =
    column [ width fill
           , height fill
           , paddingXY 5 5
           , spacingXY 0 3
           , Element.Background.color (rgb255 61 61 61)
           , Element.Font.family
                [ Element.Font.external
                    { name = "Montserrat"
                    , url = "https://fonts.googleapis.com/css?family=Montserrat"
                    }
                , Element.Font.sansSerif
                ]
           , Element.Font.color (rgb255 230 230 230)
           ]
        [ menuLayout model
        , row [ width fill, height (fillPortion 1) ] []
        , row [ width fill ] [  wordLayout <| currentWord model ]
        , row [ width fill, height (fillPortion 3) ] []
        , footerLayout model
        ]


centeredElem : Element Msg -> List (Element Msg)
centeredElem element = [ column [ width fill ] []
                       , column [ centerY ] [ element ]
                       , column [ width fill ] []
                       ]


menuLayout : Model -> Element Msg
menuLayout model = row [ width fill
                       , Element.paddingXY 10 10
                       , Element.Background.color (rgb255 61 61 61)
                       , Element.spacing 10
                       , Element.Border.rounded 8
                       , Element.htmlAttribute <| Html.Attributes.style
                            "box-shadow"
                            "5px 5px 10px #323232, -5px -5px 10px #484848"
                       ]
                       [ Element.text "File"
                       , Element.Input.button
                            [ Element.focused [] ]
                            { onPress = onPlayButton
                            , label =  Element.text
                                    <| case model.playMode of
                                            Play -> "Pause"
                                            Pause -> "Play"
                                            Backwards -> "Pause"
                            }
                       ]


onPlayButton : Maybe Msg
onPlayButton = Just <| KeyPressed NextWord


footerLayout : Model -> Element Msg
footerLayout model = row [ width fill
                         , Element.paddingXY 10 10
                         , Element.Background.color (rgb255 61 61 61)
                         , Element.spacing 10
                         , Element.Border.rounded 8
                         , Element.htmlAttribute <| Html.Attributes.style
                            "box-shadow"
                            "5px 5px 10px #323232, -5px -5px 10px #484848"
                         ]
                         [ Element.text "Copyright"]


currentWord : Model -> String
currentWord model = Maybe.withDefault "" <| (get model.currentIndex model.words)


wordLayout : String -> Element Msg
wordLayout word =
    let
        centerIdx = computeCenter word
        leftPart = String.slice 0 centerIdx word
        centerChar = String.slice centerIdx (centerIdx + 1) word
        rightPart = String.slice (centerIdx + 1) (String.length word)  word
    in
    column [ width fill, height fill ]
        [ row [ width fill ] <| centeredElem
                             <| el [ Element.Font.color <| rgb255 255 0 0]
                             <| Element.text "⥾"
        , row [ centerX
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
        , row [ width fill ] <| centeredElem
                             <| el [ Element.Font.color <| rgb255 255 0 0]
                             <| Element.text "⥿"
        ]


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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (updateModel msg model, Cmd.none)


updateModel : Msg -> Model -> Model
updateModel msg model =
    case model.mode of
        ModeRead -> updateModelRead msg model
        ModeCommand -> updateModelCommand msg model


updateModelRead : Msg -> Model -> Model
updateModelRead msg model = case msg of
    Noop -> model
    TimerTick ->
        if model.playMode == Play then
            { model
            | currentIndex =
                shiftWordIdx model.currentIndex ((+) 1) model.words
            }
        else model
    KeyPressed action ->
        case action of
            TooglePlay ->
                case model.playMode of
                    Play -> { model | playMode = Pause }
                    Pause -> { model | playMode = Play }
                    Backwards -> model
            NextWord -> { model
                        | currentIndex =
                             shiftWordIdx
                                model.currentIndex ((+) 1) model.words
                        }
            PrevWord -> { model
                        | currentIndex =
                             shiftWordIdx
                                model.currentIndex ((+) -1) model.words
                        }
            EnterCommandMode -> Debug.todo "Implement command mode."
            _ -> model


updateModelCommand : Msg -> Model -> Model
updateModelCommand msg model = case msg of
    KeyPressed action -> case action of
        ExitCommandMode -> { model | mode = ModeRead }
        RawKey key -> Debug.todo ""
        _ -> model
    _ -> model


shiftWordIdx : Int -> (Int -> Int) -> Array String -> Int
shiftWordIdx currentIdx funct words =
    let
        wordsLen = (Array.length words)
        nextIdx = funct currentIdx
    in
        if nextIdx >= wordsLen then
            0
        else if nextIdx < 0 then
            wordsLen - 1
        else
            nextIdx

        

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every (toFloat <| wpmToMilis model.wpm) (\_ -> TimerTick)
              , Sub.map KeyPressed
                <| Browser.Events.onKeyDown
                <| keyDecoder model.mode 
              ]


keyDecoder : Mode -> Decode.Decoder KeybActions
keyDecoder mode = Decode.map (processKeyb mode) (Decode.field "key" Decode.string)


processKeyb : Mode -> String -> KeybActions
processKeyb mode rawKey =
    case mode of
        ModeRead -> case rawKey of
            " " ->
              TooglePlay
            "l" ->
              NextWord
            "h" ->
              PrevWord
            ":" ->
              EnterCommandMode
            _ ->
              DoNothing
        ModeCommand -> case rawKey of
            "esc" -> ExitCommandMode
            _ -> RawKey rawKey


type Mode = ModeRead
          | ModeCommand


type KeybActions = TooglePlay
                 | NextWord
                 | PrevWord
                 | EnterCommandMode
                 | ExitCommandMode
                 | DoNothing
                 | RawKey String


wpmToMilis : Int -> Int
wpmToMilis wpm = round <| (60 / toFloat wpm) * 1000


onUrlChange : Url -> Msg
onUrlChange url = Noop


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest url = Noop


sampleText : String
sampleText = "En un lugar de la Mancha, de cuyo nombre no quiero acordarme, no ha mucho tiempo que vivía un hidalgo de los de lanza en astillero, adarga antigua, rocín flaco y galgo corredor. Una olla de algo más vaca que carnero, salpicón las más noches, duelos y quebrantos los sábados, lantejas los viernes, algún palomino de añadidura los domingos, consumían las tres partes de su hacienda. El resto della concluían sayo de velarte, calzas de velludo para las fiestas, con sus pantuflos de lo mesmo, y los días de entresemana se honraba con su vellorí de lo más fino. Tenía en su casa una ama que pasaba de los cuarenta, y una sobrina que no llegaba a los veinte, y un mozo de campo y plaza, que así ensillaba el rocín como tomaba la podadera. Frisaba la edad de nuestro hidalgo con los cincuenta años; era de complexión recia, seco de carnes, enjuto de rostro, gran madrugador y amigo de la caza. Quieren decir que tenía el sobrenombre de Quijada, o Quesada, que en esto hay alguna diferencia en los autores que deste caso escriben; aunque por conjeturas verosímiles se deja entender que se llamaba Quijana. Pero esto importa poco a nuestro cuento: basta que en la narración dél no se salga un punto de la verdad."
