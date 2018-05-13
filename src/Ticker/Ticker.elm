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
    , priceChangePercent : Float
    , time : Time.Time
    , latestUpdate : Time.Time
    }


type ExposedMsg
    = Destroy


type Msg
    = Tick Time.Time
    | TimeNow Time.Time
    | QuoteUpdate (Result Http.Error Ticker.Api.QuoteModel)
    | RequestDestroy


update : Msg -> Model -> ( Model, Cmd Msg, Maybe ExposedMsg )
update msg model =
    case msg of
        QuoteUpdate result ->
            case result of
                Ok quote ->
                    ( { model
                        | companyName = quote.companyName
                        , primaryExchange = quote.primaryExchange
                        , openPrice = quote.open
                        , price = quote.latestPrice
                        , latestUpdate = quote.latestUpdate
                        , priceChangePercent = percentChange quote.latestPrice quote.open
                      }
                    , Cmd.none
                    , Nothing
                    )

                Err err ->
                    let
                        err_ =
                            Debug.log "Error getting quote" err
                    in
                        -- TODO: Error handling
                        ( model, Cmd.none, Nothing )

        Tick time ->
            ( { model | time = time - model.latestUpdate }
            , Ticker.Api.getQuote QuoteUpdate model.symbol
            , Nothing
            )

        TimeNow time ->
            ( { model | latestUpdate = time }, Cmd.none, Nothing )

        RequestDestroy ->
            ( model, Cmd.none, Just Destroy )


view model =
    div [ Attrs.class "Ticker card" ]
        [ div [ Attrs.class "TickerClose", Events.onClick RequestDestroy ] []
        , div [ Attrs.class "TickerCompanySymbol" ] [ Html.text model.symbol ]
        , div [ Attrs.class "TickerCompanyName" ] [ Html.text model.companyName ]
        , div
            [ Attrs.class "TickerPrice"
            , Attrs.title "Latest price"
            ]
            [ Html.text <|
                if model.price == 0 then
                    ""
                else
                    toString model.price
            ]
        , div
            [ Attrs.class "TickerOpenPrice"
            , Attrs.title "Open price"
            ]
            [ Html.text <|
                if model.openPrice == 0 then
                    ""
                else
                    toString model.openPrice
            ]
        , div
            [ Attrs.class <|
                (++) "TickerChangePercent " <|
                    if model.priceChangePercent > 0 then
                        "TickerChangePercentPositive"
                    else if model.priceChangePercent < 0 then
                        "TickerChangePercentNegative"
                    else
                        ""
            ]
            [ Html.text <|
                if model.priceChangePercent > 0 then
                    (++) "+" <| toString model.priceChangePercent
                else
                    toString model.priceChangePercent
            ]
        ]


percentChange original current =
    roundToTwoPlaces <| (original - current) / original * 100


roundToTwoPlaces : Float -> Float
roundToTwoPlaces number =
    toFloat (round (number * 100.0)) / 100


init : String -> ( Model, Cmd Msg )
init symbol =
    ( Model symbol "" "" 0 0 0 0 0
    , Task.perform TimeNow Time.now
    )


subscriptions model =
    Time.every (5 * Time.second) Tick
