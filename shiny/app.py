import matplotlib.pyplot as plt
from datetime import datetime, timedelta
from shiny import App, render, ui, reactive
from pandas_datareader import data as pdr
plt.rcParams["axes.spines.top"] = False
plt.rcParams["axes.spines.right"] = False


# Configuration
time_end = datetime.now()
time_start = datetime.now() - timedelta(days=30)
tickers = {"AAPL": "Apple", "MSFT": "Microsoft", "GOOG": "Google", "AMZN": "Amazon"}

# App UI - One input to select a ticker and two outputs for chart and table
app_ui = ui.page_fluid(
    # Adjust the styles to center everything
    ui.tags.style("#container {display: flex; flex-direction: column; align-items: center;}"),
    # Main container div
    ui.tags.div(
        ui.h2("Historical Stock Prices"),
        ui.input_select(id="ticker", label="Ticker:", choices=tickers),
        ui.output_plot("viz"),
        ui.output_table("table_data"),
    id="container")
)


# Server logic
def server(input, output, session):
    # Store data as a result of reactive calculation
    @reactive.Calc
    def data():
        df = pdr.get_data_yahoo(input.ticker(), time_start, time_end)
        return df.reset_index()

    # Chart logic
    @output
    @render.plot
    def viz():
        fig, ax = plt.subplots()
        ax.plot(data()["Date"], data()["Adj Close"])
        ax.scatter(data()["Date"], data()["Adj Close"])
        ax.set_title(f"{input.ticker()} historical prices")
        return fig


    # Table logic
    @output
    @render.table
    def table_data():
        return data()


# Connect everything
app = App(app_ui, server)