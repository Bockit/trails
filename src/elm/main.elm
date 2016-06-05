import Html exposing (Html, h1, div, ul, li, text, input, button, label, span)
import Html.Attributes exposing (class, value, id, for)
import Html.App as Html
import Html.Events exposing (onClick, onInput)
import Debug exposing (log)

main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }

type Msg
  = NoOp
  | SelectToken Int
  | ChangeTokenName String
  | StartGame

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
  id : Int
}

type alias TokenList = List Token

type alias Model = {
  tokens: TokenList,
  selectedTokenId: Maybe Int,
  gameState: GameState,
  board: Board
}

--      A   B
--   H         C
--
--   G         D
--      F   E
type PathPoint = A | B | C | D | E | F | G | H

type alias Path = {
  from: PathPoint,
  to: PathPoint
}

type alias Tile = {
  paths: List Path
}

type alias Positions = List Position
type alias Position = List PathPoint

type alias Board = {
  tilePositions: List (List Int),
  playerPositions: List PlayerPosition
}

type alias PlayerPosition = {
  x: Int,
  y: Int,
  pathPoint: PathPoint
}

board = {
  tilePositions = List.map makeBoardRow [0..5],
  playerPositions = makePlayerPositions}


makeBoardRow _ = [ 0..5 ]

makePlayerPositions =
    (filterDuplicatePositions
      (List.concat
        (List.map makePlayerPositionsFromTile [0..35])))

filterDuplicatePositions list = list

comparePositions a b =
  if List.contains [ A, B ] a.pathPoint

makePlayerPositionsFromTile index =
  List.map (\coordinate -> { x = index % 6, y = index // 6, pathPoint = coordinate }) [ A, B, C, D, E, F, G, H ]

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
  selectedTokenId = Nothing,
  gameState = ChooseTokens,
  board = board}

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
      log "wah" { model | selectedTokenId = Just tokenId }
    ChangeTokenName newName ->
      case model.selectedTokenId of
        Just selectedTokenId ->
          updateTokenName model selectedTokenId newName
        Nothing ->
          model
    StartGame ->
      { model | gameState = PlayGame }
    _ ->
      model

updateTokenName: Model -> Int -> String -> Model
updateTokenName model id name =
  { model | tokens = List.map (updateTokenNameMapper id name) model.tokens }

updateTokenNameMapper: Int -> String -> Token -> Token
updateTokenNameMapper id newName token =
  if token.id == id then
    { token | name = newName }
  else
    token

isActiveToken: Token -> Maybe Int -> String
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
    pageHeading "Play Game"
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

token: Maybe Int -> Token -> Html Msg
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
