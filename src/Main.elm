port module Main exposing (main)

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
    | ReceiveSavedSymbols (List String)
    | Noop


update msg model =
    case msg of
        AddTicker ->
            let
                ( ticker, subCmd ) =
                    Ticker.init model.inputText
            in
                { model | tickers = Dict.insert model.inputText ticker model.tickers }
                    ! [ Cmd.map (TickerMsg model.inputText) subCmd
                      , saveSymbols <| (::) model.inputText <| Dict.keys model.tickers
                      ]

        ReceiveSavedSymbols symbols ->
            let
                ( tickers, cmds ) =
                    initTickers symbols
            in
                { model | tickers = tickers } ! [ cmds ]

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
            Just ( updatedTicker, subCmd, maybeSubMsg ) ->
                case maybeSubMsg of
                    Nothing ->
                        Dict.update key (\_ -> Just updatedTicker) tickers
                            ! [ Cmd.map (TickerMsg key) subCmd ]

                    Just subMsg ->
                        case subMsg of
                            Ticker.Destroy ->
                                let
                                    updatedTickers =
                                        Dict.remove key tickers

                                    keys =
                                        Dict.keys updatedTickers
                                in
                                    updatedTickers ! [ saveSymbols keys ]

            Nothing ->
                tickers ! []


view model =
    Html.div
        [ Attrs.id "Main" ]
        [ Html.form
            [ Attrs.class "MainInput container"
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
        Html.div [ Attrs.class "MainTickers container" ] mappedViews


initTickers : List String -> ( Dict.Dict String Ticker.Model, Cmd Msg )
initTickers symbols =
    let
        tickerInits =
            List.map Ticker.init symbols

        tickersDict =
            List.map Tuple.first tickerInits
                |> List.map2 (,) symbols
                |> Dict.fromList

        tickerCmdsMapped =
            List.map Tuple.second tickerInits
                |> List.map2 (\sym subCmd -> Cmd.map (TickerMsg sym) subCmd) symbols
    in
        tickersDict ! tickerCmdsMapped


init =
    ( Model Dict.empty ""
    , loadSymbols Nothing
    )


subscriptions model =
    let
        tickers =
            List.map
                (\( key, value ) -> Sub.map (TickerMsg key) (Ticker.subscriptions value))
                (Dict.toList model.tickers)
    in
        Sub.batch <| (symbolsPort ReceiveSavedSymbols) :: tickers



-- Outgoing


port saveSymbols : List String -> Cmd msg


port loadSymbols : Maybe String -> Cmd msg



-- Incoming


port symbolsPort : (List String -> msg) -> Sub msg


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
