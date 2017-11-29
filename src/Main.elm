module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Keyboard
import Dict
import List
import Tuple
import Random.List
import List.Extra as ListExtra exposing (andThen)
import Random
import Svg
import Svg.Attributes as SvgAttrs


---- MODEL ----


type alias Grid =
    Dict.Dict BoxCoordinates BoxPoints


type alias GridLists =
    List (List Box)


type alias Model =
    { grid : GridLists
    , gameScore : Int
    }


type BoxPoints
    = Hidden
    | Value Int


type alias BoxCoordinates =
    ( Int, Int )


type alias Box =
    { coordinates : BoxCoordinates
    , points : BoxPoints
    }


type alias Color =
    String


getBoxColor : BoxPoints -> Color
getBoxColor points =
    case points of
        Hidden ->
            "rgba(203, 193, 181, 255)"

        Value 2 ->
            "rgba(236, 228, 219, 255)"

        Value 4 ->
            "rgba(235, 225, 203, 255)"

        Value 8 ->
            "rgba(232, 180, 130, 255)"

        Value 16 ->
            "rgba(227, 153, 106, 255)"

        Value 32 ->
            "rgba(223, 129, 101, 255)"

        Value 64 ->
            "rgba(246, 94, 59, 255)"

        Value 128 ->
            "rgba(237, 207, 114, 255)"

        Value 256 ->
            "rgba(237, 204, 97, 255)"

        Value 512 ->
            "rgba(237, 200, 80, 255)"

        Value 1024 ->
            "rgba(237, 197, 63, 255)"

        Value 2048 ->
            "rgba(237, 194, 46, 255)"

        Value 4096 ->
            "rgba(60, 58, 50, 255)"

        _ ->
            ""


generateGrid : Dict.Dict BoxCoordinates BoxPoints
generateGrid =
    List.range 0 (numBoxes - 1)
        |> andThen
            (\y ->
                List.range 0 (numBoxes - 1)
                    |> andThen
                        (\x ->
                            [ ( ( x, y ), Hidden ) ]
                        )
            )
        |> Dict.fromList


generateInitialBoxes : List BoxCoordinates -> List Box
generateInitialBoxes listBoxCoordinates =
    List.map (\boxCoordinate -> Box boxCoordinate Hidden) listBoxCoordinates



-- initialGrid =
-- generateGrid
-- |> List.map generateInitialBoxes


initialModel : Model
initialModel =
    { grid = generateGridLists
    , gameScore = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = KeyUp Int
    | GotRandomSquare (Maybe Box)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyUp keyCode ->
            let
                _ =
                    Debug.log "keyCode" keyCode
            in
                case keyCode of
                    32 ->
                        ( model
                        , Random.generate GotRandomSquare <|
                            randomSquarePickerGrid model.grid
                        )

                    37 ->
                        ( { model | grid = moveLeft model.grid }, Cmd.none )

                    38 ->
                        ( { model | grid = moveUp model.grid }, Cmd.none )

                    39 ->
                        ( { model
                            | grid = moveRight model.grid
                          }
                        , Cmd.none
                        )

                    40 ->
                        ( { model | grid = moveDown model.grid }, Cmd.none )

                    82 ->
                        ( { model
                            | grid = rotateGrid model.grid
                          }
                        , Cmd.none
                        )

                    _ ->
                        ( model, Cmd.none )

        GotRandomSquare square ->
            let
                grid =
                    square
                        |> Maybe.map (flipHiddenSquareLists model.grid)
                        |> Maybe.withDefault model.grid
            in
                ( { model | grid = grid }, Cmd.none )

        _ ->
            ( model, Cmd.none )


moveRight : GridLists -> GridLists
moveRight grid =
    List.map reduceSquares grid
        |> mapIndicesToCoordinates


moveDown : GridLists -> GridLists
moveDown =
    rotateGrid
        >> rotateGrid
        >> rotateGrid
        >> List.map reduceSquares
        >> rotateGrid


moveLeft : GridLists -> GridLists
moveLeft =
    rotateGrid
        >> rotateGrid
        >> List.map reduceSquares
        >> rotateGrid
        >> rotateGrid


moveUp : GridLists -> GridLists
moveUp grid =
    rotateGrid grid
        |> List.map reduceSquares
        |> rotateGrid
        |> rotateGrid
        |> rotateGrid


mapIndicesToCoordinates : GridLists -> GridLists
mapIndicesToCoordinates grid =
    grid
        |> List.indexedMap
            (\indexY row ->
                List.indexedMap
                    (\indexX square ->
                        { square | coordinates = ( indexX, indexY ) }
                    )
                    row
            )


getPointsFromCoordinates : GridLists -> Int -> Int -> BoxPoints
getPointsFromCoordinates grid y x =
    grid
        |> ListExtra.getAt y
        |> Maybe.andThen (\row -> ListExtra.getAt x row)
        |> Maybe.map .points
        |> Maybe.withDefault Hidden


rotateGrid : GridLists -> GridLists
rotateGrid grid =
    let
        newGrid =
            generateGridLists
                |> List.indexedMap
                    (\indexY row ->
                        List.indexedMap
                            (\indexX square ->
                                { square
                                    | points =
                                        getPointsFromCoordinates
                                            grid
                                            (numBoxes - 1 - indexX)
                                            indexY
                                }
                            )
                            row
                    )
    in
        newGrid


randomCoordinatesPicker : Random.Generator BoxCoordinates
randomCoordinatesPicker =
    Random.pair (Random.int 0 <| numBoxes - 1) (Random.int 0 <| numBoxes - 1)


randomSquarePicker :
    Grid
    -> Random.Generator (Maybe BoxCoordinates)
randomSquarePicker grid =
    let
        hiddenSquares =
            Dict.toList grid
                |> List.filter (\( _, boxPoints ) -> boxPoints == Hidden)

        randomIndexGenerator =
            Random.int 0 <| List.length hiddenSquares
    in
        Random.map
            (\index ->
                ListExtra.getAt index hiddenSquares
                    |> Maybe.map Tuple.first
            )
            randomIndexGenerator


randomBoxPicker : List Box -> Random.Generator (Maybe Box)
randomBoxPicker boxes =
    let
        hiddenBoxes =
            List.filter (\box -> box.points == Hidden) boxes
    in
        Random.map (\index -> ListExtra.getAt index hiddenBoxes)
            (Random.int 0 <|
                (List.length hiddenBoxes)
                    - 1
            )


flipHiddenSquare : Grid -> BoxCoordinates -> Grid
flipHiddenSquare grid coordinates =
    Dict.update coordinates (\_ -> Just (Value 2)) grid


flipHiddenSquareLists : GridLists -> Box -> GridLists
flipHiddenSquareLists grid square =
    List.map
        (\row ->
            List.map
                (\square_ ->
                    if square_.coordinates == square.coordinates then
                        square
                    else
                        square_
                )
                row
        )
        grid


mapBoxPoints : (Int -> Int) -> BoxPoints -> BoxPoints
mapBoxPoints func points =
    case points of
        Value x ->
            Value (func x)

        _ ->
            Hidden


reduceSquares : List Box -> List Box
reduceSquares row =
    let
        reducedRow =
            (List.foldr
                (\square acc ->
                    case square.points of
                        Hidden ->
                            acc

                        Value points ->
                            case acc of
                                prev :: rest ->
                                    if (Value points) == prev.points then
                                        { prev
                                            | points =
                                                Value (points * 2)
                                        }
                                            :: rest
                                    else
                                        { square | points = Value points } :: acc

                                _ ->
                                    { square | points = Value points } :: acc
                )
                []
                row
            )
    in
        reducedRow
            |> (-) 4
            << List.length
            |> flip List.append reducedRow
            << flip List.repeat (Box ( 0, 0 ) Hidden)
            |> Debug.log "squares"


generateGridLists : GridLists
generateGridLists =
    List.range 0 3
        |> List.map (\y -> List.range 0 3 |> List.map (\x -> Box ( x, y ) Hidden))


randomSquarePickerGrid : GridLists -> Random.Generator (Maybe Box)
randomSquarePickerGrid grid =
    grid
        |> List.concatMap
            (\row ->
                List.filter (\square -> square.points == Hidden) row
            )
        |> Debug.log "whats here"
        |> Random.List.choose
        |> Random.map (\( maybeSquare, _ ) -> maybeSquare)
        |> flip Random.pair (Random.int 1 10)
        |> Random.map
            (\( maybeSquare, num ) ->
                let
                    _ =
                        Debug.log "nums" num

                    points =
                        if num < 8 then
                            Value 2
                        else
                            Value 4
                in
                    Maybe.map (\square -> { square | points = points }) maybeSquare
            )



---- VIEW ----


backgroundSize =
    600


boxSize =
    100


numBoxes =
    4


padding =
    20


paddingAround =
    20


boardGameSize =
    numBoxes * (padding + boxSize) + 2 * paddingAround - padding


boardPosition =
    (backgroundSize - boardGameSize) // 2


viewBox : Box -> Svg.Svg msg
viewBox { coordinates, points } =
    Svg.svg
        [ SvgAttrs.x <| toString <| boardPosition + paddingAround + Tuple.first coordinates * (boxSize + padding)
        , SvgAttrs.y <|
            toString <|
                boardPosition
                    + paddingAround
                    + Tuple.second coordinates
                    * (boxSize + padding)
        , SvgAttrs.width <|
            toString
                boxSize
        , SvgAttrs.height <| toString boxSize
        , SvgAttrs.fill <| getBoxColor points
        ]
        [ Svg.rect
            [ SvgAttrs.height "100%", SvgAttrs.width "100%" ]
            []
        , Svg.text_
            [ SvgAttrs.x <| toString (boxSize / 2)
            , SvgAttrs.y <|
                toString (boxSize / 2)
            , SvgAttrs.textAnchor "middle"
            , SvgAttrs.alignmentBaseline "central"
            , SvgAttrs.fill "white"
            ]
            [ viewPoints points
            ]
        ]


viewPoints : BoxPoints -> Html.Html msg
viewPoints points =
    case points of
        Value x ->
            text <| toString x

        Hidden ->
            text ""


flattenGrid : GridLists -> List Box
flattenGrid grid =
    grid
        |> andThen (\row -> row |> andThen (\square -> [ square ]))


viewSquares : List Box -> List (Svg.Svg msg)
viewSquares squares =
    List.map viewBox squares


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , div [] [ text "Your Elm App is working!" ]
        , Svg.svg
            [ SvgAttrs.width <| toString backgroundSize
            , SvgAttrs.height <| toString backgroundSize
            , SvgAttrs.viewBox <|
                "0 0 "
                    ++ toString backgroundSize
                    ++ " "
                    ++ toString backgroundSize
            ]
          <|
            [ Svg.rect
                [ SvgAttrs.x <| toString boardPosition
                , SvgAttrs.y <| toString boardPosition
                , SvgAttrs.width <| toString boardGameSize
                , SvgAttrs.height <| toString boardGameSize
                , SvgAttrs.fill "rgba(184, 173, 162, 255)"
                ]
                []
            ]
                ++ (viewSquares <| flattenGrid model.grid)
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
