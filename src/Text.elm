module Text exposing (..)
import Array


------------------------------------------------------------------------------
-- Types ---------------------------------------------------------------------
------------------------------------------------------------------------------


type alias Text =
    { rawText : CharArr
    , cursorIdx: Int
    }


type alias CharArr = Array.Array Char


-------------------------------------------------------------------------------
-- Creation -------------------------------------------------------------------
-------------------------------------------------------------------------------


fromString : String -> Text
fromString str =
    { rawText = Array.fromList <| String.toList str
    , cursorIdx = 0
    }


-------------------------------------------------------------------------------
-- Search ---------------------------------------------------------------------
-------------------------------------------------------------------------------


getWordStrAt : Int -> CharArr -> List Char -> Maybe String
getWordStrAt at chars delims =
    getWordAt at chars delims |> Maybe.andThen ( Just << charArrToString )


getWordAt : Int -> CharArr -> List Char -> Maybe CharArr
getWordAt at chars delims =
    Array.get at chars |> Maybe.andThen
        ( \charAt ->
            if List.member charAt delims then
                Nothing
            else
                findCharsFromIn at chars delims |> Maybe.andThen
                    ( \gapIdx -> Just <| Array.slice at gapIdx chars )
        )


getNextWordFrom : Int -> CharArr -> List Char -> Maybe CharArr
getNextWordFrom from chars delims =
    nextWordStart from chars delims
        |> Maybe.andThen ( \at -> getWordAt at chars delims )


nextWordStart : Int -> CharArr -> List Char -> Maybe Int
nextWordStart from chars delims =
    Array.get from chars |> Maybe.andThen
        ( \char -> if List.member char delims then
                        findCharsFromNotIn from chars delims
                   else
                       nextWordStart ( from + 1 ) chars delims
        )


findCharsFromIn : Int -> CharArr -> List Char -> Maybe Int
findCharsFromIn from chars delims =
    findCharsFromCond from chars ( \char -> List.member char delims )


findCharsFromNotIn : Int -> CharArr -> List Char -> Maybe Int
findCharsFromNotIn from chars delims =
    findCharsFromCond from chars ( \char -> not <| List.member char delims )


findCharsFromCond : Int -> CharArr -> ( Char -> Bool ) -> Maybe Int
findCharsFromCond from chars cond =
    Array.get from chars |> Maybe.andThen
        ( \char -> if cond char then
                        Just from
                    else
                        findCharsFromCond (from + 1) chars cond
        )
        

-------------------------------------------------------------------------------
-- Utils ----------------------------------------------------------------------
-------------------------------------------------------------------------------


charArrToString : CharArr -> String
charArrToString = String.fromList << Array.toList


stringToCharArr : String -> CharArr
stringToCharArr = Array.fromList << String.toList


softWordDelimiters : List Char
softWordDelimiters =
    [ ' ',
      '.',
      ',',
      '-',
      '@',
      '\n'
    ]
