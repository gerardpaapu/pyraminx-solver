-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/time.html


module Main exposing (..)

import Array exposing (Array, get, set)
import Dict
import Html exposing (Html, select, option, p, button, div)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onClick)
import LazyList exposing (..)
import Maybe exposing (withDefault, andThen)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions model =
    Time.every (second / 100) Tick



-- MODEL


type alias Model =
    { computation : Maybe Computation
    , selectedColor : Color
    , scramble : PuzzleState
    , solution : Maybe Script
    }


type alias Computation =
    ( Stats, LazyList SearchState )


init : ( Model, Cmd Msg )
init =
    ( { computation = Nothing
      , solution = Nothing
      , selectedColor = Green
      , scramble = initState
      }
    , Cmd.none
    )


type alias Stats =
    { solution : Maybe Script
    , state : Maybe PuzzleState
    , examined : Int
    , pruned : Int
    , complete : Bool
    , depth : Int
    }


initStats =
    { solution = Nothing
    , state = Nothing
    , examined = 0
    , pruned = 0
    , complete = False
    , depth = 0
    }


stepN n stats search =
    if n > 0 then
        let
            ( stats_, search_ ) =
                step stats search
        in
            stepN (n - 1) stats_ search_
    else
        ( stats, search )


step stats search =
    case stats.solution of
        Just _ ->
            ( stats, search )

        Nothing ->
            case force search of
                Nil ->
                    ( { stats | complete = True }, search )

                Cons x xs ->
                    case x of
                        Solution state script ->
                            ( { stats
                                | solution = Just script
                                , state = Just state
                                , depth = List.length script
                              }
                            , xs
                            )

                        Prune ->
                            ( { stats | pruned = stats.pruned + 1 }, xs )

                        MisMatch state script ->
                            ( { stats
                                | examined = stats.examined + 1
                                , state = Just state
                                , depth = List.length script
                              }
                            , xs
                            )



-- UPDATE


type Msg
    = Tick Float
    | ColorSelect Color
    | SetFace Int
    | Start
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.computation of
                Just ( stats_, search_ ) ->
                    case stats_.solution of
                        Just solution ->
                            ( { model
                                | solution = Just solution
                                , computation = Nothing
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            let
                                ( stats, search ) =
                                    stepN 1673 stats_ search_
                            in
                                ( { model | computation = Just ( stats, search ) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ColorSelect clr ->
            ( { model | selectedColor = clr }, Cmd.none )

        Start ->
            let
                never _ =
                    0

                search_ =
                    search model.scramble initState
            in
                ( { model
                    | solution = Nothing
                    , computation = Just ( initStats, search_ )
                  }
                , Cmd.none
                )

        Stop ->
            ( { model | computation = Nothing }, Cmd.none )

        SetFace idx ->
            let
                faces =
                    model.scramble
                        |> Array.set (idx - 1) model.selectedColor
            in
                ( { model | scramble = faces }, Cmd.none )



-- VIEW


type Color
    = Yellow
    | Blue
    | Green
    | Red
    | Grey


type alias PuzzleState =
    Array Color


initState : PuzzleState
initState =
    Array.fromList
        [ Blue
        , Blue
        , Blue
        , Blue
        , Blue
        , Blue
        , Red
        , Red
        , Red
        , Red
        , Red
        , Red
        , Green
        , Green
        , Green
        , Green
        , Green
        , Green
        , Yellow
        , Yellow
        , Yellow
        , Yellow
        , Yellow
        , Yellow
        ]


colorToString : Color -> String
colorToString color =
    case color of
        Yellow ->
            "yellow"

        Blue ->
            "blue"

        Green ->
            "green"

        Red ->
            "red"

        Grey ->
            "grey"


min3 x y z =
    Basics.min x y |> Basics.min z


max3 x y z =
    Basics.max x y |> Basics.max z


find : (a -> Maybe b) -> List a -> Maybe b
find f ls =
    case ls of
        x :: xs ->
            case f x of
                Nothing ->
                    find f xs

                Just result ->
                    Just result

        _ ->
            Nothing


type Twist
    = Up
    | UpN
    | Left
    | LeftN
    | Right
    | RightN
    | Back
    | BackN


apply1 twist =
    case twist of
        Up ->
            twist_ ( 5, 10, 13 ) ( 3, 12, 14 ) ( 6, 8, 15 )

        UpN ->
            twist_ ( 5, 13, 10 ) ( 3, 14, 12 ) ( 6, 15, 8 )

        Right ->
            twist_ ( 17, 11, 21 ) ( 15, 9, 19 ) ( 18, 12, 23 )

        RightN ->
            twist_ ( 17, 21, 11 ) ( 15, 19, 9 ) ( 18, 23, 12 )

        Left ->
            twist_ ( 4, 16, 20 ) ( 2, 14, 19 ) ( 6, 18, 22 )

        LeftN ->
            twist_ ( 4, 20, 16 ) ( 2, 19, 14 ) ( 6, 22, 18 )

        Back ->
            twist_ ( 1, 7, 24 ) ( 3, 9, 22 ) ( 2, 8, 23 )

        BackN ->
            twist_ ( 1, 24, 7 ) ( 3, 22, 9 ) ( 2, 23, 8 )


twist_ ( a, b, c ) ( d, e, f ) ( g, h, i ) state =
    state
        |> cycle a b c
        |> cycle d e f
        |> cycle g h i


cycle : Int -> Int -> Int -> PuzzleState -> PuzzleState
cycle a b c arr =
    let
        set3 a_ b_ c_ =
            arr
                |> Array.set (a - 1) c_
                |> Array.set (b - 1) a_
                |> Array.set (c - 1) b_
    in
        set3
            (get (a - 1) arr |> withDefault Grey)
            (get (b - 1) arr |> withDefault Grey)
            (get (c - 1) arr |> withDefault Grey)


swap : Int -> Int -> PuzzleState -> PuzzleState
swap a b arr =
    let
        set2 a_ b_ =
            arr
                |> Array.set (a - 1) b_
                |> Array.set (b - 1) a_
    in
        set2
            (get (a - 1) arr |> withDefault Grey)
            (get (b - 1) arr |> withDefault Grey)


type alias Script =
    List Twist


apply script state =
    List.foldl apply1 state script


type SearchState
    = Solution PuzzleState Script
    | MisMatch PuzzleState Script
    | Prune


search : PuzzleState -> PuzzleState -> LazyList SearchState
search start goal =
    let
        twoColor_ c =
            case c of
                Green ->
                    Green

                _ ->
                    Red

        twoColor =
            Array.map twoColor_

        never _ =
            0

        toKey ps =
            toString ps

        replace l m =
            case m of
                Nothing ->
                    Just l

                Just _ ->
                    m

        add dict ss =
            case ss of
                MisMatch p s ->
                    Dict.update (toString p) (replace <| List.length s) dict

                _ ->
                    dict

        table : Dict.Dict String Int
        table =
            searchLL (twoColor goal) (twoColor goal) never 5
                |> LazyList.foldl add Dict.empty

        bestCase ps =
            Dict.get (toString <| twoColor ps) table
                |> withDefault 0
    in
        searchLL start goal bestCase 12


searchLL : PuzzleState -> PuzzleState -> (PuzzleState -> Int) -> Int -> LazyList SearchState
searchLL start goal bestCase maxdepth =
    let
        depths =
            LazyList.ofList (List.range 0 maxdepth)

        steps : LazyList Twist
        steps =
            LazyList.ofList [ Up, UpN, Left, LeftN, Right, RightN ]

        dfs : Script -> Int -> LazyList SearchState
        dfs script depth =
            let
                length =
                    List.length script

                state =
                    apply script start

                best =
                    bestCase state
            in
                if length + best > depth then
                    LazyList.singleton Prune
                else if length < depth then
                    LazyList.flatMap (\s -> dfs (script ++ [ s ]) depth) steps
                else if state == goal then
                    LazyList.singleton <| Solution state script
                else
                    LazyList.singleton <| MisMatch state script

        dfs_ depth =
            dfs [] depth
    in
        LazyList.flatMap dfs_ depths


faces : PuzzleState -> List (Html Msg)
faces state =
    let
        triangle : Color -> Int -> ( Float, Float ) -> ( Float, Float ) -> ( Float, Float ) -> Html Msg
        triangle color idx ( x1, y1 ) ( x2, y2 ) ( x3, y3 ) =
            let
                minX =
                    min3 x1 x2 x3

                minY =
                    min3 y1 y2 y3

                maxX =
                    max3 x1 x2 x3

                maxY =
                    max3 y1 y2 y3

                deltaX =
                    maxX - minX

                deltaY =
                    maxY - minY
            in
                g []
                    [ polygon
                        [ onClick (SetFace idx)
                        , points
                            (toString x1
                                ++ " "
                                ++ toString y1
                                ++ " "
                                ++ toString x2
                                ++ " "
                                ++ toString y2
                                ++ " "
                                ++ toString x3
                                ++ " "
                                ++ toString y3
                                ++ " "
                            )
                        , stroke "black"
                        , fill (colorToString color)
                        , strokeWidth "2"
                        ]
                        []
                    , Svg.text_
                        [ x <| toString <| minX + 0.5 * deltaX
                        , y <| toString <| minY + 0.5 * deltaY
                        , fontSize "14"
                        ]
                        [ Svg.text <| toString idx ]
                    ]

        face_ f =
            [ f ( 200, 0 ) ( 280, 0 ) ( 240, 80 )
              -- 2nd row
            , f ( 200, 0 ) ( 240, 80 ) ( 160, 80 )
            , f ( 280, 0 ) ( 320, 80 ) ( 240, 80 )
              -- 3rd row
            , f ( 160, 80 ) ( 240, 80 ) ( 200, 160 )
            , f ( 240, 80 ) ( 320, 80 ) ( 280, 160 )
              -- 4th row
            , f ( 240, 80 ) ( 280, 160 ) ( 200, 160 )
            ]

        face ( x, y ) =
            face_
                (\( x1, y1 ) ( x2, y2 ) ( x3, y3 ) c idx ->
                    triangle c idx ( x1 + x, y1 + y ) ( x2 + x, y2 + y ) ( x3 + x, y3 + y )
                )

        faces_ =
            (face ( -80, 0 ))
                ++ (face ( 80, 0 ))
                ++ (face ( 0, 160 ))
                ++ (face ( 0, 320 ))
    in
        List.map3
            (\f c i -> f c i)
            faces_
            (Array.toList state)
            (List.range 1 24)


viewPuzzle : PuzzleState -> Svg Msg
viewPuzzle state =
    svg [ width "480px", height "480px" ]
        (faces state)


selectColor c =
    ColorSelect <|
        (case c of
            "Red" ->
                Red

            "Green" ->
                Green

            "Yellow" ->
                Yellow

            "Blue" ->
                Blue

            _ ->
                Grey
        )


startStopButton model =
    case model.computation of
        Nothing ->
            button [ onClick Start ] [ text "Start" ]

        _ ->
            button [ onClick Stop ] [ text "Stop" ]


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput selectColor ]
            [ option [ value "Green" ] [ text "Green" ]
            , option [ value "Red" ] [ text "Red" ]
            , option [ value "Yellow" ] [ text "Yellow" ]
            , option [ value "Blue" ] [ text "Blue" ]
            , option [ value "Grey" ] [ text "Grey" ]
            ]
        , startStopButton model
        , div [] [ viewPuzzle model.scramble ]
        , div []
            [ viewStats_ model
            , viewSolution model
            ]
        ]


viewSolution model =
    case model.solution of
        Just s ->
            text <| "Solved: " ++ (toString s)

        Nothing ->
            text "Unsolved"


viewStats_ model =
    case model.computation of
        Nothing ->
            text ""

        Just ( stats, _ ) ->
            viewStats stats


viewStats : Stats -> Html msg
viewStats stats =
    let
        { solution, state, pruned, examined, complete, depth } =
            stats
    in
        div []
            [ p [] [ text <| "Depth: " ++ (toString depth) ]
            , p [] [ text <| "Exhausted: " ++ (toString complete) ]
            , p [] [ text <| "Examined " ++ (toString examined) ]
            , p [] [ text <| "Pruned: " ++ (toString pruned) ]
            ]
