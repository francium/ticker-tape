module Ticker.Api exposing (QuoteModel, getQuote)

import Http
import Json.Decode as Decode
import String.Extra exposing (replace)
import Time


type alias QuoteModel =
    { symbol : String
    , companyName : String
    , primaryExchange : String
    , open : Float
    , latestPrice : Float
    , latestUpdate : Time.Time
    }


quoteUrlStockPlaceholder =
    "{{ stock }}"


quoteUrl =
    "https://api.iextrading.com/1.0/stock/" ++ quoteUrlStockPlaceholder ++ "/book"


stockQuoteUrlBuilder stock =
    replace quoteUrlStockPlaceholder stock quoteUrl


getQuote msg stock =
    Http.send msg <| Http.get (stockQuoteUrlBuilder stock) quoteDecoder


quoteDecoder =
    Decode.at [ "quote" ] quoteDataDecoder


quoteDataDecoder =
    Decode.map6
        QuoteModel
        (Decode.field "symbol" Decode.string)
        (Decode.field "companyName" Decode.string)
        (Decode.field "primaryExchange" Decode.string)
        (Decode.field "open" Decode.float)
        (Decode.field "latestPrice" Decode.float)
        (Decode.field "latestUpdate" Decode.float)
