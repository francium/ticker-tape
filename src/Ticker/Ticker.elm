module Ticker.Ticker exposing (..)

import Debug
import Html exposing (div, span)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Time
import Task
import Ticker.Api


type alias Model =
    { symbol : String
    , companyName : String
    , primaryExchange : String
    , price : Float
    , openPrice : Float
    , time : Time.Time
    , latestUpdate : Time.Time
    }


type Msg
    = PriceChange Float
    | Tick Time.Time
    | TimeNow Time.Time
    | QuoteUpdate (Result Http.Error Ticker.Api.QuoteModel)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuoteUpdate result ->
            case result of
                Ok quote ->
                    { model
                        | companyName = quote.companyName
                        , primaryExchange = quote.primaryExchange
                        , openPrice = quote.open
                        , price = quote.latestPrice
                        , latestUpdate = quote.latestUpdate
                    }
                        ! []

                Err err ->
                    let
                        err_ =
                            Debug.log "Error getting quote" err
                    in
                        -- TODO: Error handling
                        model ! []

        PriceChange price ->
            { model | price = price } ! []

        Tick time ->
            { model | time = time - model.latestUpdate }
                ! [ Ticker.Api.getQuote QuoteUpdate model.symbol ]

        TimeNow time ->
            { model | latestUpdate = time } ! []


view model =
    div [ Attrs.class "Ticker card" ]
        [ div [ Attrs.class "TickerCompanySymbol" ] [ Html.text model.symbol ]
        , div [ Attrs.class "TickerCompanyName" ] [ Html.text model.companyName ]
        , div
            [ Attrs.class "TickerPrice" ]
            [ Html.text <|
                if model.price == 0 then
                    ""
                else
                    toString model.price
            ]
        , div
            [ Attrs.class "TickerOpenPrice" ]
            [ Html.text <|
                if model.openPrice == 0 then
                    ""
                else
                    toString model.openPrice
            ]
        , div
            [ Attrs.class "TickerChangePercent" ]
            [ Html.text <| percentChange model.price model.openPrice ]
        ]


percentChange original current =
    let
        change =
            roundToTwoPlaces <| (original - current) / original * 100
    in
        case isNaN change of
            True ->
                ""

            False ->
                if change > 0 then
                    "+" ++ (toString change)
                else
                    toString change


roundToTwoPlaces : Float -> Float
roundToTwoPlaces number =
    toFloat (round (number * 100.0)) / 100


init : String -> ( Model, Cmd Msg )
init symbol =
    ( Model symbol "" "" 0 0 0 0
    , Task.perform TimeNow Time.now
    )


subscriptions model =
    Time.every (5 * Time.second) Tick
