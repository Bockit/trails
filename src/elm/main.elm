import Html exposing (Html, h1, div, ul, li, text, input, button, label, span)
import Html.Attributes exposing (class, value, id, for)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)

main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }

type Msg
  = NoOp
  | SelectToken TokenId
  | ChangeTokenName String
  | StartGame
  | ClickCoordinate Coordinate

type GameState = ChooseTokens | PlayGame | GameOver

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
  id : TokenId
}

type alias TokenId = Int

type alias TokenList = List Token

type alias Model = {
  tokens: TokenList,
  selectedTokenId: Maybe TokenId,
  gameState: GameState,
  tile: Tile,
  selectedCoordinate: Coordinate
}

--      A   B
--   H         C
--
--   G         D
--      F   E
type Coordinate = A | B | C | D | E | F | G | H

type alias Path = {
  from: Coordinate,
  to: Coordinate
}

type alias Tile = {
  paths: List Path}

tokens = [
  { id = 0, name = "Black", color = Black },
  { id = 1, name = "Grey", color = Grey },
  { id = 2, name = "Brown", color = Brown },
  { id = 3, name = "Blue", color = Blue },
  { id = 4, name = "Red", color = Red },
  { id = 5, name = "Purple", color = Purple },
  { id = 6, name = "Green", color = Green },
  { id = 7, name = "Orange", color = Orange }]

initialTile = {
  paths = [
    { from = A, to = C },
    { from = B, to = D },
    { from = C, to = A },
    { from = D, to = B },
    { from = E, to = G },
    { from = F, to = H },
    { from = G, to = E },
    { from = H, to = F }
  ]}

initialModel: Model
initialModel = {
  tokens = tokens,
  selectedTokenId = Nothing,
  gameState = ChooseTokens,
  tile = initialTile,
  selectedCoordinate = A}

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
    SelectToken tokenId ->
      { model | selectedTokenId = Just tokenId }
    ChangeTokenName newName ->
      case model.selectedTokenId of
        Just selectedTokenId ->
          updateTokenName model selectedTokenId newName
        Nothing ->
          model
    StartGame ->
      { model | gameState = PlayGame }
    ClickCoordinate coordinate ->
      log "model" { model | selectedCoordinate = coordinate }
    _ ->
      model

updateTokenName: Model -> TokenId -> String -> Model
updateTokenName model id name =
  { model | tokens = List.map (updateTokenNameMapper id name) model.tokens }

updateTokenNameMapper: TokenId -> String -> Token -> Token
updateTokenNameMapper id newName token =
  if token.id == id then
    { token | name = newName }
  else
    token

isActiveToken: Token -> Maybe TokenId -> String
isActiveToken token activeTokenId =
  case activeTokenId of
    Just id ->
      if token.id == id then "active" else ""
    Nothing ->
      ""

findToken tokens tokenId =
  List.filter (compareTokenToId tokenId) tokens |> List.head

compareTokenToId id token =
  token.id == id

selectedTokenName model =
  case model.selectedTokenId of
    Just tokenId -> findToken model.tokens tokenId
    Nothing -> Nothing

view: Model -> Html Msg
view model =
  case model.gameState of
    ChooseTokens ->
      chooseTokensPage model
    PlayGame ->
      playGamePage model
    GameOver ->
      gameOverPage model

chooseTokensPage model =
  div []
  [
    pageHeading "Name your tokens",
    tokenList model,
    tokenNameChanger (selectedTokenName model),
    button [ class "navigate", onClick StartGame ] [ text "Start the game" ]
  ]

playGamePage model =
  div []
  [
    pageHeading "Play Game",
    tile model.tile
  ]

gameOverPage model =
  div []
  [
    pageHeading "GameOver"
  ]

pageHeading title =
  h1 [ class "page-heading" ] [ text title ]

tokenList: Model -> Html Msg
tokenList model =
  div [ class "token-list" ]
    (List.map ( token model.selectedTokenId ) model.tokens)

token: Maybe TokenId -> Token -> Html Msg
token selectedTokenId token =
  label
    [
      class ((colorToCssClass token.color) ++ " token " ++ (isActiveToken token selectedTokenId)),
      onClick (SelectToken token.id),
      for "namer"
    ] [ text token.name ]

tokenNameChanger: Maybe Token -> Html Msg
tokenNameChanger token =
  case token of
    Just token ->
      input [ value token.name, onInput ChangeTokenName, id "namer" ] []
    Nothing ->
      div [] []

--findExit: Tile -> Coordinate -> Coordinate
--findExit tile entryPoint =
--  (Array.fromList(tile)).to

tile data =
  div [ class "tile" ] [
    span [] [ text "I'm a tile" ],
    ul [] [
      li [ class "coordinate-a", onClick ( ClickCoordinate A ) ] [ text "A" ],
      li [ class "coordinate-b", onClick ( ClickCoordinate B ) ] [ text "B" ],
      li [ class "coordinate-c", onClick ( ClickCoordinate C ) ] [ text "C" ],
      li [ class "coordinate-d", onClick ( ClickCoordinate D ) ] [ text "D" ],
      li [ class "coordinate-e", onClick ( ClickCoordinate E ) ] [ text "E" ],
      li [ class "coordinate-f", onClick ( ClickCoordinate F ) ] [ text "F" ],
      li [ class "coordinate-g", onClick ( ClickCoordinate G ) ] [ text "G" ],
      li [ class "coordinate-h", onClick ( ClickCoordinate H ) ] [ text "H" ]
    ]
  ]
