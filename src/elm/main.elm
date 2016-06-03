import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (class)
import Html.App as Html
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram { model = 0, view = view, update = update }

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

view model =
  ul
    [ class "hello-world" ]
    [
      token "Emma",
      token "James"
    ]

token : String -> Html msg
token name =
  li [ class "marker" ] [ text name ]
