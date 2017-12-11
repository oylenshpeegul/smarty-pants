-- Cribbed from https://github.com/kelonye/demo-bmi-calc

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


main = Html.program { init = init 0 0 0 0
                    , view = view
                    , update = update
                    , subscriptions = subscriptions
                    }


-- MODEL

type alias Model = { calories: Float
                   , satfat: Float
                   , sugar: Float
                   , protein: Float
                   , sp: Int
                   }


init : Float -> Float -> Float -> Float -> (Model, Cmd Msg)
init calories satfat sugar protein =
  (Model calories satfat sugar protein 0, Cmd.none)


-- UPDATE

type Msg = Calories String
         | SatFat String
         | Sugar String
         | Protein String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Calories calories ->
      let
        c = Result.withDefault 0 (String.toFloat calories)
        s = sp c model.satfat model.sugar model.protein
      in
        ({model|calories = c, sp = s }, Cmd.none)

    SatFat satfat ->
      let
        a = Result.withDefault 0 (String.toFloat satfat)
        s = sp model.calories a model.sugar model.protein
      in
        ({model|satfat = a, sp = s }, Cmd.none)

    Sugar sugar ->
      let
        u = Result.withDefault 0 (String.toFloat sugar)
        s = sp model.calories model.satfat u model.protein
      in
        ({model|sugar = u, sp = s }, Cmd.none)

    Protein protein ->
      let
        p = Result.withDefault 0 (String.toFloat protein)
        s = sp model.calories model.satfat model.sugar p
      in
        ({model|protein = p, sp = s }, Cmd.none)



sp : Float -> Float -> Float -> Float -> Int
sp calories satfat sugar protein =
  let
    s = round((calories
              + 4 * sugar
              + 9 * satfat
              - 3.2 * protein
              )/33.0)
  in
    if (s < 0)  then 0 else s


-- VIEW

view : Model -> Html Msg
view model =
  table [] [
    tr [] [
      td [] [ text "Calories:" ],
      td [] [ input [ type_ "number", onInput Calories ] [] ]
    ],
    tr [] [
      td [] [ text "Saturated Fat (g):" ],
      td [] [ input [ type_ "number", onInput SatFat ] [] ]
    ],
    tr [] [
      td [] [ text "Sugar (g):" ],
      td [] [ input [ type_ "number", onInput Sugar ] [] ]
    ],
    tr [] [
      td [] [ text "Protein (g):" ],
      td [] [ input [ type_ "number", onInput Protein ] [] ]
    ],
    tr [] [
      td [] [ text "SP:"],
      td [] [ text (toString model.sp) ]
    ]
  ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
