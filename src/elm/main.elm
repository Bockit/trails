import Html exposing (Html, h1, div, text, input, button, label, span, ol, ul, li)
import Svg exposing (svg, path)
import Svg.Attributes exposing (d, width, height)
import Html.Attributes exposing (class, value, id, for)
import Html.App as Html
import Html.Events exposing (onClick, onInput, onDoubleClick)
import String
import Debug exposing (log)

main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }

type Msg
  = NoOp
  | SelectToken Int
  | ChangeTokenName String
  | StartGame
  | SelectStartingPoint PlayerCoordinate
  | PlaceTile Int

type GameState = ChooseTokens | ChoosePositions | PlayGame | GameOver

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
  id : Int,
  placed : Bool,
  position: Maybe PlayerCoordinate
}

type alias TokenList = List Token

type alias Model = {
  tokens: TokenList,
  selectedTokenId: Maybe Int,
  gameState: GameState,
  board: Board
}

--          (0,0) (0,1)
--
--   (3,0)                (1,0)
--
--   (3,1)                (1,1)
--
--          (2,0) (2,1)
type alias PathPoint = (Int, Int)

type alias Path = (PathPoint, PathPoint)

type alias Tile = {
  points: List PathPoint,
  connections: Maybe (List Path)
}

type alias Board = {
  tiles: List Tile
}

type alias PlayerCoordinate = {
  tile: Int,
  tileCoordinate: PathPoint
}

board = { tiles = List.map makeTile [0..35] }

makeTile _ =
  log "making tile" ({
    points = [
      (0, 0), (0, 1),
      (1, 0), (1, 1),
      (2, 0), (2, 1),
      (3, 0), (3, 1)
    ],
    connections = Nothing
  })

makeTileConnections =
  Just [
    ((0, 0), (1, 1)),
    ((1, 0), (2, 1)),
    ((2, 0), (3, 1)),
    ((3, 0), (0, 1))
  ]

tokens = [
  { id = 0, name = "Black", color = Black, position = Nothing, placed = False },
  { id = 1, name = "Grey", color = Grey, position = Nothing, placed = False },
  { id = 2, name = "Brown", color = Brown, position = Nothing, placed = False },
  { id = 3, name = "Blue", color = Blue, position = Nothing, placed = False },
  { id = 4, name = "Red", color = Red, position = Nothing, placed = False },
  { id = 5, name = "Purple", color = Purple, position = Nothing, placed = False },
  { id = 6, name = "Green", color = Green, position = Nothing, placed = False },
  { id = 7, name = "Orange", color = Orange, position = Nothing, placed = False }]

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
      { model | gameState = ChoosePositions }
    SelectStartingPoint position ->
      { model |
        tokens = setFirstUnsetTokenPosition model.tokens position,
        gameState = keepSelectingOrPlayGame model.tokens}
    PlaceTile index ->
      if model.gameState == PlayGame then
        placeTile model makeTileConnections index
      else
        model
    _ ->
      model

placeTile model connections tileIndex =
  { model |
    board = placeTileOnBoard connections board tileIndex }

placeTileOnBoard connections board tileIndex =
  { board | tiles = log "tiles" (List.indexedMap (addConnectionsToTile tileIndex connections) board.tiles) }

addConnectionsToTile setIndex newConnections tileIndex tile =
  if setIndex == tileIndex then
    { tile | connections = newConnections }
  else
    tile

keepSelectingOrPlayGame tokens =
  if (countUnplaced tokens) == 1 then
    PlayGame
  else
    ChoosePositions

countUnplaced tokens =
  List.length (List.filter (\token -> token.placed == False) tokens)

setFirstUnsetTokenPosition: TokenList -> PlayerCoordinate -> TokenList
setFirstUnsetTokenPosition tokens position =
  List.concat([ filterPlaced tokens, placeFirst position (filterUnplaced tokens)])

placeFirst: PlayerCoordinate -> TokenList -> TokenList
placeFirst position tokens =
  List.indexedMap (\index token ->
    if index == 0 then
      { token | position = Just position, placed = True }
    else
      token) tokens

filterUnplaced: TokenList -> TokenList
filterUnplaced = filterPlacedState False

filterPlaced: TokenList -> TokenList
filterPlaced = filterPlacedState True

filterPlacedState: Bool -> TokenList -> TokenList
filterPlacedState state tokens =
  List.filter (\token -> token.placed == state) tokens

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
    ChoosePositions ->
      choosePositionsPage model
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

choosePositionsPage model =
  div []
  [
    pageHeading "Place your tokens",
    div [ class "board-wrapper" ]
    [
      tokenOrder (List.filter (\token -> token.placed == False) model.tokens),
      boardView model.board model.tokens True
    ]
  ]

playGamePage model =
  div []
  [
    pageHeading "Play Game",
    div [ class "board-wrapper" ]
    [
      tokenOrder model.tokens,
      boardView model.board model.tokens False
    ]
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

boardView: Board -> TokenList -> Bool -> Html Msg
boardView board tokens showMarkers =
  div [ class "board" ]
    (List.indexedMap (tileViewHelper tokens showMarkers) board.tiles)

tileViewHelper tokens showMarkers index tile =
  tileView (filterTokensForTile tokens index) index tile showMarkers

filterTokensForTile tokens tileIndex =
  List.filter (\token ->
    case token.position of
      Just position -> position.tile == tileIndex
      Nothing -> False
    ) tokens

tileView: TokenList -> Int -> Tile -> Bool -> Html Msg
tileView tokens index tile showMarkers=
  div [
    class "tile",
    onDoubleClick (PlaceTile index)
  ] [
    tilePaths tile.connections,
    (if showMarkers then
      ol [ class "points" ]
        (List.map (tilePointView index) tile.points)
    else
      div [] []),
    tileTokens tokens
  ]

tilePointView: Int -> PathPoint -> Html Msg
tilePointView tileIndex (side, index) =
  if isBoardEdgePoint tileIndex side then
    li [
      class ((pathPointToString (side, index)) ++ " point"),
      onClick (SelectStartingPoint { tile = tileIndex, tileCoordinate = (side, index)})
    ] [
      text (pathPointToString (side, index))
    ]
  else
    li [] []

isBoardEdgePoint: Int -> Int -> Bool
isBoardEdgePoint tileIndex side =
  (tileIndex < 6 && side == 0)
  || (tileIndex % 6 == 0 && side == 3)
  || (tileIndex % 6 == 5 && side == 1)
  || (tileIndex // 6 == 5 && side == 2)

pathPointToString: PathPoint -> String
pathPointToString (side, index) =
  "point-" ++ (toString side) ++ "-" ++ (toString index)

tileTokens: TokenList -> Html Msg
tileTokens tokens =
  ul [ class "tokens" ]
    (List.map tileToken tokens)

tileToken: Token -> Html Msg
tileToken token =
  case token.position of
    Just position ->
      li [ class ((colorToCssClass token.color) ++ " tile-token " ++ (pathPointToString position.tileCoordinate)) ] []
    Nothing ->
      li [] []

tilePaths: Maybe (List Path) -> Html Msg
tilePaths connections =
  case connections of
    Just connections ->
      svg [ width "200px", height "200px" ]
        (List.map connectionView connections)
    Nothing ->
      svg [] []

connectionView: Path -> Html Msg
connectionView (connection) =
  path [ d (connectionPathString connection) ] []

connectionPathString: Path -> String
connectionPathString (from, to) =
  "M" ++ (connectionPathStringCoord from) ++
    "L" ++ (connectionPathStringCoord to)

connectionPathStringCoord: (Int, Int) -> String
connectionPathStringCoord point =
  case point of
    (0, 0) -> "66.666,0"
    (0, 1) -> "133.333,0"
    (1, 0) -> "200,66.666"
    (1, 1) -> "200,133.333"
    (2, 0) -> "66.666,200"
    (2, 1) -> "133.333,200"
    (3, 0) -> "0,66.666"
    (3, 1) -> "0,133.333"
    _ -> ""

tokenOrder tokens =
  ul [ class "token-order" ]
    (List.map orderToken tokens)

orderToken token =
  li [ class ((colorToCssClass token.color) ++ " order-token") ] []
