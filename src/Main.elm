module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (toFloat)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Option =
    { strikePrice : Float
    , premium : Float
    , cost : Float -> Float -> Float
    }


type alias Model =
    { sharePrice : Float
    , call : Option
    , put : Option
    }


model : Model
model =
    Model 0 (option (\a b -> a + b)) (option (\a b -> a - b))


option : (Float -> Float -> Float) -> Option
option fn =
    { strikePrice = 0
    , premium = 0
    , cost = fn
    }



-- UPDATE


type Msg
    = Share String
    | CallStrike String
    | PutStrike String
    | CallPremium String
    | PutPremium String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Share price ->
            { model | sharePrice = toFloat price }

        CallStrike price ->
            { model | call = setStrikePrice model.call (toFloat price) }

        CallPremium price ->
            { model | call = setPremium model.call (toFloat price) }

        PutStrike price ->
            { model | put = setStrikePrice model.put (toFloat price) }

        PutPremium price ->
            { model | put = setPremium model.put (toFloat price) }


setPremium : Option -> Float -> Option
setPremium opt premium =
    { opt | premium = premium }


setStrikePrice : Option -> Float -> Option
setStrikePrice opt strike =
    { opt | strikePrice = strike }



-- toFloat converts user input to a floating point number
-- toFloat returns 0 if user entered non-float


toFloat : String -> Float
toFloat str =
    Result.withDefault 0 (String.toFloat str)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Share Price", onInput Share ] []
        , input [ placeholder "Call Price", onInput CallStrike ] []
        , input [ placeholder "Put Price", onInput PutStrike ] []
        , input [ placeholder "Call Premium", onInput CallPremium ] []
        , input [ placeholder "Put Premium", onInput PutPremium ] []
        , div [] [ text <| breakEven model ]
        ]


breakEven : Model -> String
breakEven model =
    let
        call =
            model.call

        put =
            model.put

        callBreakEven =
            call.cost call.strikePrice <| call.cost call.premium put.premium

        putBreakEven =
            put.cost put.strikePrice <| call.cost put.premium call.premium
    in
        "callBreakEven:" ++ toString callBreakEven ++ "putBreakEven:" ++ toString putBreakEven
