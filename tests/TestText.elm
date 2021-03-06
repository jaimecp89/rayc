module TestText exposing (..)

import Test exposing (..)
import Text
import Array
import Expect


suite : Test
suite = describe "Testing the Text module"
      [ test "Text creation" <|
          \_ -> sampleString
                    |> Text.fromString
                    |> Expect.equal
                        {  rawText = Array.fromList <| String.toList sampleString
                        , cursorIdx = 0
                        }

      , test "Get word at simple" <|
          \_ -> Text.getWordStrAt 9 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Just "tal" )
      , test "Get word at start at gap" <|
          \_ -> Text.getWordStrAt 8 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Nothing )
      , test "Get word at start middle of word" <|
          \_ -> Text.getWordStrAt 10 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Just "al" )

      , test "Find chars simple 1" <|
          \_ -> Text.findCharsFromIn 0 sampleCharArr ['H']
                |> Expect.equal ( Just 0)
      , test "Find chars simple 2" <|
          \_ -> Text.findCharsFromIn 0 sampleCharArr [' ']
                |> Expect.equal ( Just 4)
      , test "Find chars simple 3" <|
          \_ -> Text.findCharsFromIn 4 sampleCharArr [' ']
                |> Expect.equal ( Just 4)
      , test "Find chars simple 4" <|
          \_ -> Text.findCharsFromIn 20 sampleCharArr [' ']
                |> Expect.equal ( Just 22)

      , test "Get next word start simple" <|
          \_ -> Text.nextWordStart 15 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Just 17)

      , test "Get next word simple" <|
          \_ -> Text.getNextWordFrom 16 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Just <| Text.stringToCharArr "llamo")
      , test "Get next word two gaps" <|
          \_ -> Text.getNextWordFrom 20 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Just <| Text.stringToCharArr "Jaime")
      , test "Get next word at end" <|
          \_ -> Text.getNextWordFrom 28 sampleCharArr Text.softWordDelimiters
                |> Expect.equal ( Nothing )
      ]


sampleString : String
--               00000000001111111111222222222233
--               01234567890123456789012345678901
sampleString =  "Hola que tal. Me llamo    Jaime."


sampleCharArr : Text.CharArr
sampleCharArr = Text.stringToCharArr sampleString


sampleText : Text.Text
sampleText = Text.fromString sampleString
