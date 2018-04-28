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

type Option
    = Call OptionInfo
    | Put OptionInfo


type alias OptionInfo =
    { strikePrice : Float
    , premium : Float
    }


type alias Model =
    { sharePrice : Float
    , call : Option
    , put : Option
    }


model : Model
model =
    Model 0 (Call option) (Put option)


option : OptionInfo
option =
    { strikePrice = 0
    , premium = 0
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
    case opt of
        Call info ->
            Call
                { premium = premium
                , strikePrice = info.strikePrice
                }

        Put info ->
            Put
                { premium = premium
                , strikePrice = info.strikePrice
                }


setStrikePrice : Option -> Float -> Option
setStrikePrice opt strike =
    case opt of
        Call info ->
            Call
                { premium = info.premium
                , strikePrice = strike
                }

        Put info ->
            Put
                { premium = info.premium
                , strikePrice = strike
                }



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
        callInfo =
            getInfo model.call

        putInfo =
            getInfo model.put

        callBreakEven =
            callInfo.strikePrice + callInfo.premium + putInfo.premium

        putBreakEven =
            putInfo.strikePrice - putInfo.premium - callInfo.premium
    in
        "callBreakEven:" ++ toString callBreakEven ++ "putBreakEven:" ++ toString putBreakEven


getInfo : Option -> OptionInfo
getInfo o =
    case o of
        Call info ->
            info

        Put info ->
            info
