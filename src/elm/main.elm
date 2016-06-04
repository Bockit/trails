import Html exposing (Html, h1, div, ul, li, text, input)
import Html.Attributes exposing (class, value)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)

main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }

type Msg
  = NoOp
  | SelectToken Token
  | ChangeTokenName String

type Color
  = Black
  | Grey
  | Brown
  | Blue
  | Red
  | Purple
  | Green
  | Orange

type alias Token = {
  name : String,
  color : Color,
  id : Int
}

type alias TokenList = List Token
type alias Model = {
  tokens: TokenList,
  selectedToken: Maybe Token
}

tokens = [
  { id = 0, name = "Black", color = Black },
  { id = 1, name = "Grey", color = Grey },
  { id = 2, name = "Brown", color = Brown },
  { id = 3, name = "Blue", color = Blue },
  { id = 4, name = "Red", color = Red },
  { id = 5, name = "Purple", color = Purple },
  { id = 6, name = "Green", color = Green },
  { id = 7, name = "Orange", color = Orange }]

initialModel: Model
initialModel = {
  tokens = tokens,
  selectedToken = Nothing}

colorToCssClass: Color -> String
colorToCssClass color =
  case color of
    Black -> "black-token"
    Grey -> "grey-token"
    Brown -> "brown-token"
    Blue -> "blue-token"
    Red -> "red-token"
    Purple -> "purple-token"
    Green -> "green-token"
    Orange -> "orange-token"

update: Msg -> Model -> Model
update msg model =
  case msg of
    SelectToken token ->
      { model | selectedToken = Just token }
    ChangeTokenName newName ->
      case model.selectedToken of
        Just selectedToken ->
          updateTokenName { model | selectedToken = Just { selectedToken | name = newName } } selectedToken.id newName
        Nothing -> model
    _ ->
      model

updateTokenName model id name =
  { model | tokens = List.map (updateTokenNameMapper id name) model.tokens }

updateTokenNameMapper id newName token =
  if token.id == id then
    { token | name = newName }
  else
    token

isActiveToken token selectedToken =
  case selectedToken of 
    Just selected ->
      if token.id == selected.id then "active" else ""
    Nothing ->
      ""

view: Model -> Html Msg
view model =
  div []
  [
    h1 [ class "page-heading"] [ text "this is a heading" ],
    tokenList model,
    tokenNameChanger model.selectedToken
  ]

tokenList: Model -> Html Msg
tokenList model =
  ul [class "token-list"]
    (List.map (token model.selectedToken) model.tokens)

token: Maybe Token -> Token -> Html Msg
token selectedToken token =
  li
    [
      class ((colorToCssClass token.color) ++ " token " ++ (isActiveToken token selectedToken)),
      onClick (SelectToken token)
    ] [ text token.name ]

tokenNameChanger: Maybe Token -> Html Msg
tokenNameChanger token =
  case token of
    Just token ->
      input [ value token.name, onInput ChangeTokenName ] []
    Nothing ->
      div [] []
