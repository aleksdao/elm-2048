module Main exposing (..)

import Html exposing (Html, text, div, img)
import Html.Attributes exposing (src)
import Keyboard
import Dict
import List
import Tuple
import List.Extra as ListExtra exposing (andThen)
import Random
import Svg
import Svg.Attributes as SvgAttrs


---- MODEL ----


type alias Grid =
    Dict.Dict BoxCoordinates BoxPoints


type alias Model =
    { grid : Grid
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



-- generateGrid : Int -> Int -> List (List BoxCoordinates)
-- generateGrid numBoxesX numBoxesY =
--     List.range 0 (numBoxesX - 1)
--         |> List.map
--             (\x ->
--                 List.range 0 (numBoxesY - 1)
--             |> List.map (\y -> ( x, y ))
-- )
-- |> Debug.log "list"
-- g -> b -> b) -> b -> List a -> b


generateGrid : Dict.Dict BoxCoordinates BoxPoints
generateGrid =
    List.range 0 (numBoxes - 1)
        |> andThen
            (\x ->
                List.range 0 (numBoxes - 1)
                    |> andThen
                        (\y ->
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
    { grid = generateGrid
    , gameScore = 0
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



---- UPDATE ----


type Msg
    = KeyUp Int
    | GotRandomSquare (Maybe BoxCoordinates)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyUp keyCode ->
            ( model
            , Random.generate GotRandomSquare <|
                randomSquarePicker model.grid
            )

        GotRandomSquare coordinates ->
            let
                grid =
                    coordinates
                        |> Maybe.map (flipHiddenSquare model.grid)
                        |> Maybe.withDefault model.grid

                -- let
                --     _
                --     grid =
                --         coordinates
                --             |> Maybe.map flipHiddenSquare
                --         case coordinates of
                --             Just validCoordinates ->
                --                 List.map (flipHiddenSquare square) model.grid
                --
                --             Nothing ->
                --                 model.grid
                -- in
                -- ( { model | grid = grid }, Cmd.none )
            in
                ( { model | grid = grid }, Cmd.none )

        _ ->
            ( model, Cmd.none )


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



-- List.length hiddenSquares
-- |> List.length
-- |> Random.int 0
-- |> Random.map (\index ->
-- randomCoordinatesGenerator
-- |> Random.map
-- (\( x, y ) ->
-- ListExtra.getAt x grid
-- |> Maybe.andThen
-- (\row ->
-- ListExtra.getAt y row
-- )
-- )


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



-- flipHiddenSquare : Box -> List Box -> List Box
-- flipHiddenSquare hiddenSquare row =
--     List.map
--         (\possiblyHiddenSquare ->
--             if possiblyHiddenSquare.coordinates == hiddenSquare.coordinates then
--                 { possiblyHiddenSquare | points = Value 2 }
--             else
--                 possiblyHiddenSquare
--         )
--         row
-- addValue : BoxPoints -> Int
-- addValue points ->
-- case points of
-- Just points_ ->
-- points_
-- _ ->


reduceSquares : List Box -> List Int
reduceSquares row =
    List.foldr
        (\{ points } acc ->
            case points of
                Hidden ->
                    acc

                Value points_ ->
                    case acc of
                        prev :: rest ->
                            if points_ == prev then
                                (prev * 2) :: rest
                            else
                                points_ :: acc

                        _ ->
                            points_ :: acc
        )
        []
        row



-- function, startingvalue, list
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


viewBox : ( BoxCoordinates, BoxPoints ) -> Svg.Svg msg
viewBox ( coordinates, points ) =
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
        , Svg.text_ [ SvgAttrs.x <| toString (boxSize / 2), SvgAttrs.y <| toString (boxSize / 2), SvgAttrs.textAnchor "middle", SvgAttrs.alignmentBaseline "central" ]
            [ text
                "1"
            ]
        ]



-- viewBoxes : List Box -> List (Svg.Svg msg)
-- viewBoxes boxes =
-- List.map viewBox boxes


viewGrid : Grid -> List (Svg.Svg msg)
viewGrid grid =
    Dict.toList grid
        |> List.map viewBox


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
                ++ viewGrid model.grid
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
