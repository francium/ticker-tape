module Ticker.Ticker exposing (..)

import Debug
import Html exposing (div)
import Html.Attributes as Attrs
import Html.Events as Events
import Http
import Time
import Task
import Ticker.Api


type alias Model =
    { symbol : String
    , price : Float
    , startingPrice : Float
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
                        | price = quote.latestPrice
                        , latestUpdate = quote.latestUpdate
                        , startingPrice = quote.open
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
    div [ Attrs.class "Ticker" ]
        [ div
            [ Attrs.class "TickerCompany" ]
            [ Html.text model.symbol ]
        , div
            [ Attrs.class "TickerPrice" ]
            [ Html.text <| toString model.price ]
        , div
            [ Attrs.class "TickerStartingPrice" ]
            [ Html.text <| toString model.startingPrice ]
        , div
            [ Attrs.class "TickerTime" ]
            [ Html.text <| toString <| round <| model.time / 1000 ]
        ]


init : String -> Float -> Float -> ( Model, Cmd Msg )
init symbol price startingPrice =
    Model symbol price startingPrice 0 0
        ! [ Task.perform TimeNow Time.now ]


subscriptions model =
    Time.every (5 * Time.second) Tick
