const LS_STOCK_SYMBOLS_KEY = 'stockSymbols';

const app = Elm.Main.fullscreen();

app.ports.loadSymbols.subscribe(_ =>
{
  try {

    const symbolsJson = localStorage.getItem(LS_STOCK_SYMBOLS_KEY);
    const symbols = JSON.parse(symbolsJson);
    if (symbols && symbols instanceof Array)
    {
      app.ports.symbolsPort.send(symbols);
    }
  } catch (err) {
    console.error('Error occured while trying to handle `loadSymbols` port message', err);
  }
});

app.ports.saveSymbols.subscribe(symbols =>
{
  localStorage.setItem(LS_STOCK_SYMBOLS_KEY, JSON.stringify(symbols));
})
