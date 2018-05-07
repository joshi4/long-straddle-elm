module Main exposing (..)

import Css exposing (focus, backgroundColor)
import Css.Colors exposing (yellow)
import Html.Styled exposing (Html, div, input, text, styled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput, onClick)
import String exposing (toFloat)


main : Program Never Model Msg
main =
    Html.Styled.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Option =
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
    Model 0 option option


option : Option
option =
    Option 0 0



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


type CallOption
    = Call Option


type PutOption
    = Put Option


breakEven : Model -> String
breakEven model =
    let
        call =
            model.call

        put =
            model.put

        cBreakEven =
            callBreakEven (Call call) (Put put)

        pBreakEven =
            putBreakEven (Call call) (Put put)
    in
        "callBreakEven:" ++ toString cBreakEven ++ "putBreakEven:" ++ toString pBreakEven



-- callBreakEven ensures it is a compiler error if
-- arguments are provided in the wrong order


callBreakEven : CallOption -> PutOption -> Float
callBreakEven (Call call) (Put put) =
    call.strikePrice + call.premium + put.premium



-- callBreakEven ensures it is a compiler error if
-- arguments are provided in the wrong order


putBreakEven : CallOption -> PutOption -> Float
putBreakEven (Call call) (Put put) =
    put.strikePrice - (call.premium + put.premium)


styledInput : String -> (String -> Msg) -> Html Msg
styledInput defaultText msg =
    styled input
        [ focus [ backgroundColor yellow ] ]
        [ placeholder defaultText
        , onInput msg
        ]
        []


view : Model -> Html Msg
view model =
    div []
        [ styledInput "Share Price" Share
        , styledInput "Call Price" CallStrike
        , styledInput "Put Price" PutStrike
        , styledInput "Call Premium" CallPremium
        , styledInput "Put Premium" PutPremium
        , div [] [ text <| breakEven model ]
        ]
