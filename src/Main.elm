module Main exposing (..)

import Html
import Html.Attributes as Attrs
import Html.Events as Events
import Dict
import List
import Ticker.Ticker as Ticker


type alias Model =
    { tickers : Dict.Dict String Ticker.Model
    , inputText : String
    }


type Msg
    = TickerMsg String Ticker.Msg -- Key Value
    | AddTicker
    | UpdateInputText String
    | Noop


update msg model =
    case msg of
        AddTicker ->
            let
                ( ticker, subCmd ) =
                    Ticker.init model.inputText 0 0
            in
                { model | tickers = Dict.insert model.inputText ticker model.tickers }
                    ! [ Cmd.map (TickerMsg model.inputText) subCmd ]

        UpdateInputText name ->
            { model | inputText = name } ! []

        TickerMsg key subMsg ->
            let
                ( tickers, cmds ) =
                    updateTickers key subMsg model.tickers
            in
                { model | tickers = tickers } ! [ cmds ]

        Noop ->
            model ! []


updateTickers :
    String
    -> Ticker.Msg
    -> Dict.Dict String Ticker.Model
    -> ( Dict.Dict String Ticker.Model, Cmd Msg )
updateTickers key subMsg tickers =
    let
        maybeTicker =
            Dict.get key tickers

        maybeUpdated =
            case maybeTicker of
                Just ticker ->
                    Just (Ticker.update subMsg ticker)

                Nothing ->
                    Nothing
    in
        case maybeUpdated of
            Just ( updatedTicker, subCmd ) ->
                Dict.update key (\_ -> Just updatedTicker) tickers
                    ! [ Cmd.map (TickerMsg key) subCmd ]

            Nothing ->
                tickers ! []


view model =
    Html.div
        [ Attrs.id "Main" ]
        [ Html.form
            [ Attrs.class "MainInput"
            , Events.onSubmit AddTicker
            ]
            [ Html.input
                [ Events.onSubmit AddTicker
                , Events.onInput UpdateInputText
                , Attrs.placeholder "Stock name"
                ]
                []
            ]
        , viewTickers model.tickers
        ]


viewTickers tickers =
    let
        tickerKeysAndViews =
            List.map
                (\( key, value ) -> ( key, Ticker.view value ))
                (Dict.toList tickers)

        mappedViews =
            List.map
                (\( key, view ) -> Html.map (TickerMsg key) view)
                tickerKeysAndViews
    in
        Html.div [ Attrs.class "MainTickers" ] mappedViews


initialSymbols =
    [ "fb", "aapl", "mtch", "extr" ]


init =
    let
        ( symbols, tickerInits ) =
            ( initialSymbols
            , List.map (\sym -> Ticker.init sym 0 0) initialSymbols
            )

        tickersDict =
            List.map Tuple.first tickerInits
                |> List.map2 (,) symbols
                |> Dict.fromList

        tickerCmdsMapped =
            List.map Tuple.second tickerInits
                |> List.map2 (\sym subCmd -> Cmd.map (TickerMsg sym) subCmd) symbols

        textInput =
            ""

        model =
            Model tickersDict textInput
    in
        model ! tickerCmdsMapped


subscriptions model =
    let
        tickers =
            List.map
                (\( key, value ) -> Sub.map (TickerMsg key) (Ticker.subscriptions value))
                (Dict.toList model.tickers)
    in
        Sub.batch tickers


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
