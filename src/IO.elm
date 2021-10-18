port module IO exposing ( requestRead, receiveRead, decodeRead )

import Json.Decode exposing ( Decoder, field, string, bool, map2, decodeString
                            , errorToString
                            )


port requestRead : String -> Cmd msg
port receiveRead : (String -> msg) -> Sub msg 


decodeRead : String -> Result String String
decodeRead jsonStr =
    case decodeString readDecoder jsonStr of
        Ok response ->
            if response.ok then
                Ok response.data
            else
                Err response.data
        Err parseError ->
            Err <| errorToString parseError


readDecoder : Decoder ReadFileResponse
readDecoder =
    map2 ReadFileResponse (field "ok" bool) (field "data" string)


type alias ReadFileResponse =
    { ok: Bool
    , data: String
    }
