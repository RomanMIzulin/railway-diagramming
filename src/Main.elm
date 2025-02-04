module Main exposing (..)

--    (|.) means “parse this, but ignore the result”
--  (|=) means “parse this, and keep the result”
-- import modules for converting to svg and then to html
-- Import some example diagrams
-- View source code:
-- https://github.com/jcberentsen/elm-wiring-diagrams/blob/main/src/Cartesian/Examples.elm

import Browser
import Cartesian as C exposing (before)
import Cartesian.Examples exposing (a, b, c)
import Cartesian.Svg as Svg
import Debug exposing (toString)
import Diagram.Svg as Svg
import Html exposing (Attribute, Html, div, input, text, textarea)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Parser exposing ((|.), (|=), Parser, float, spaces, succeed, symbol, variable)
import Parser.Advanced exposing (run)
import Set


type alias Point =
    { x : Float
    , y : Float
    }


type alias Transform =
    { from : String
    , to : String
    }



-- ( 3, 4 )


point : Parser Point
point =
    succeed Point
        |. symbol "("
        |. spaces
        |= float
        |. spaces
        |. symbol ","
        |. spaces
        |= float
        |. spaces
        |. symbol ")"


transform : Parser Transform
transform =
    succeed Transform
        |. spaces
        |= variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.empty }
        |. symbol " "
        |= variable { start = Char.isLower, inner = \c -> Char.isAlphaNum c || c == '_', reserved = Set.empty }
        |. spaces


test_a_b =
    run transform "a-b" == Ok { from = "a", to = "b" }



-- MODEL


type alias Model =
    { content : String
    }


init : Model
init =
    { content = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newContent ->
            { model | content = newContent }



-- VIEW


svg label_name =
    let
        diagram =
            a |> before b |> before c
    in
    Svg.view Svg.mediumViewport [ Svg.fromDiagram diagram ]


view : Model -> Html Msg
view model =
    div [ style "display" "flex", style "flex-direction" "row" ]
        [ div [ style "display" "grid", style "width" "50%" ] [ textarea [ placeholder "Text to parse", value model.content, onInput Change ] [], text (toString (run transform model.content)) ]
        , div []
            [ svg "a"
            ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
