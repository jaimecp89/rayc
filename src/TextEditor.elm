module TextEditor exposing (..)
import Array exposing (Array)


type alias TextEditor =
    { cursorPos: Int
    , selectedPos: Int
    , text: Array Char
    }


saveToFile : TextEditor -> Cmd msg
saveToFile editor = Debug.todo ""


loadFromFile : String -> Cmd TextEditor
loadFromFile path = Debug.todo ""


moveForward : TextEditor -> Int -> TextEditor
moveForward editor amount =
    if editor.cursorPos + amount < ( textLen editor ) then
        { editor | cursorPos = editor.cursorPos + amount }
    else
        { editor | cursorPos = ( textLen editor ) }


moveBack : TextEditor -> Int -> TextEditor
moveBack editor amount =
    if editor.cursorPos - amount >= 0 then
        { editor | cursorPos = editor.cursorPos - amount }
    else
        { editor | cursorPos = 0 }


textLen : TextEditor -> Int
textLen = Array.length << .text
