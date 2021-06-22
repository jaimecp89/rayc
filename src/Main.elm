module Main exposing (..)

import Browser
import Browser.Navigation exposing (Key)
import Url exposing (Url)
import Array exposing (fromList, Array, get)
import String exposing (words)
import Delay
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
import Element.Input as Input
import Element.Border as Border
import List.Extra exposing ( getAt )

import Text exposing ( CharLoc )


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
    , currentLoc : CharLoc
    , readRange : Maybe ( CharLoc, CharLoc )
    , buffers : Maybe ( List String, Int )
    }


init : flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    (
    { mode = Normal
    , readingStatus = Waiting
    , wpm = 500
    , currentLoc = ( 0, 0 )
    , readRange = Nothing
    , buffers = Just ( [ sampleText ], 0 )
    }
    ,
    Cmd.none
    )


type Msg
    = Noop
    | ReadTick
    | KeyPressed KeybActions


type EditMode
    = Normal
    | Insert
    | Command


type ReadingStatus
    = Reading
    | Waiting


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Noop -> ( model, Cmd.none )
    ReadTick -> case model.readingStatus of
        Waiting ->
            ( model, Cmd.none )
        Reading ->
            let
                nextLoc = jumpWordLoc model
            in
            ( { model
              | currentLoc = jumpWordLoc model
              }
            , Delay.after (wpmToMilis model.wpm) ReadTick
            )
    KeyPressed action -> Debug.todo ""


jumpWordLoc : Model -> Maybe CharLoc
jumpWordLoc model =
    let
        ( row, col ) = model.currentLoc
        line =  Maybe.andThen (getAt row)
             <| Maybe.map String.lines
             <| Maybe.andThen
                    ( \idx -> getAt idx model.buffers )
                    model.currentBuffer
    in
    Maybe.map
    (
        \line_ ->
            case String.indices " " <| String.dropLeft col line_ of 
                [] -> (0, col + 1)
                x::_ -> (x + 1, col)
    )
    line


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyPressed
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
        ModeCommand -> case Debug.log "" rawKey of
            "Escape" -> ExitCommandMode
            _ -> RawKey rawKey


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
           , row [ width fill ] [  wordLayout model.mode <| currentWord model ]
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
                       [ Ui.text "File"
                       , Input.button
                            [ Ui.focused [] ]
                            { onPress = onPlayButton
                            , label =  Ui.text
                                    <| case model.readMode of
                                            Play -> "Pause"
                                            Pause -> "Play"
                                            Backwards -> "Pause"
                            }
                       ]


onPlayButton : Maybe Msg
onPlayButton = Just <| KeyPressed TooglePlay


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


currentWord : Model -> String
currentWord model = case model.mode of
    ModeRead ->
        Maybe.withDefault "" <| (get model.readIndex model.readWords)
    ModeCommand ->
        Maybe.withDefault "" <| (get model.commandIndex model.commandWords)


wordLayout : Mode -> String -> Ui.Element Msg
wordLayout mode word =
    let
        centerIdx = computeCenter word
        leftPart = String.slice 0 centerIdx word
        centerChar = String.slice centerIdx (centerIdx + 1) word
        rightPart = String.slice (centerIdx + 1) (String.length word)  word
    in
    column [ width fill, height fill ]
           [ row [ width fill ] <| centeredElem
                                <| Ui.el [ Font.color <| Ui.rgb255 255 0 0]
                                <| Element.text <| topMarkerSymbol mode
           , row [ centerX
                 , centerY
                 , width fill
                 ]
                 [ column [ width fill ]
                          [ Ui.paragraph [Font.alignRight] [Ui.text leftPart] ]
                 , column [ Font.color (Ui.rgb255 255 0 0) ]
                          [ Ui.paragraph [Font.center] [Ui.text centerChar] ]
                 , column [ width fill ]
                          [ Ui.paragraph [Font.alignLeft] [Ui.text rightPart] ]
                 ]
           , row [ width fill ] <| centeredElem
                                <| Ui.el [ Font.color <| Ui.rgb255 255 0 0]
                                <| Element.text <| bottomMarkerSymbol mode
           ]


topMarkerSymbol : Mode -> String
topMarkerSymbol mode =
    case mode of
        ModeRead -> "⥾"
        ModeCommand -> "c"


bottomMarkerSymbol : Mode -> String
bottomMarkerSymbol mode =
    case mode of
        ModeRead -> "⥿"
        ModeCommand -> "c"


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
