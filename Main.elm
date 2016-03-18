import Html exposing (div, text)
import StartApp
import List exposing (map, filterMap)
import Task
import Effects exposing (Never)
import Json.Decode as Json exposing ((:=))

app : StartApp.App (List Message)
app = StartApp.start {
        init = ([], Effects.none),
        view = view,
        update = \a m -> (update a m, Effects.none),
        inputs = [Signal.map SetList messages]
      }

main : Signal Html.Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

view : a -> List Message -> Html.Html
view address model =
  div [] (map (messageInner >> .message >> (\s -> div [] [text s])) model)

update : Action -> List Message -> List Message
update action model =
  case action of
    SetList list -> list

type Action = SetList (List Message)

port clippyLog : Signal (List String)

messages : Signal (List Message)
messages = Signal.map (filterMap <| Result.toMaybe << Json.decodeString messageDecoder) clippyLog

(:+:) : Json.Decoder (a -> b) -> Json.Decoder a -> Json.Decoder b
(:+:) func value = Json.object2 (<|) func value

nullOr : Json.Decoder a -> Json.Decoder (Maybe a)
nullOr decoder =
  Json.oneOf
    [ Json.null Nothing
    , Json.map Just decoder
    ]

type alias Span = {
    file_name: String,
    byte_start: Int,
    byte_end: Int,
    line_start: Int,
    line_end: Int,
    column_start: Int,
    column_end: Int
  }

spanDecoder : Json.Decoder Span
spanDecoder =
  Json.map Span
  ("file_name" := Json.string) :+:
  ("byte_start" := Json.int) :+:
  ("byte_end" := Json.int) :+:
  ("line_start" := Json.int) :+:
  ("line_end" := Json.int) :+:
  ("column_start" := Json.int) :+:
  ("column_end" := Json.int)

type Message = Message MessageInner

messageInner : Message -> MessageInner
messageInner message =
  case message of
    Message m -> m

type alias MessageInner = {
    message: String,
    code: Maybe String,
    level: String,
    spans: List Span,
    children: List Message
  }

-- `andThen` shenanigans due to Elm bug #873
messageDecoder : Json.Decoder Message
messageDecoder =
  Json.succeed () `Json.andThen`
  (\_ -> Json.map Message <|
     Json.map MessageInner
     ("message" := Json.string) :+:
     ("code" := nullOr Json.string) :+:
     ("level" := Json.string) :+:
     ("spans" := Json.list spanDecoder) :+:
     ("children" := Json.list messageDecoder)
  )
