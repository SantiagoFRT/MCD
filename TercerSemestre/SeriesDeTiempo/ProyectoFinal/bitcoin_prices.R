### Install packages----
install.packages(plotly) # for candlestick chart
install.packages(quantmod) # for financial data from the US stock market

### Set up libraries ---
library(plotly)
library(quantmod)

### Get Bitcoin-USD data from yahoo finance ----
getSymbols("BTC-USD",src='yahoo')

### Set dataframe ----
df <- data.frame(Date=index(`BTC-USD`),coredata(`BTC-USD`))
# df <- tail(df, 30)

### Set candlestick chart ----
candlestick <- df %>% plot_ly(x = ~Date, type="candlestick", # set x axis data and kind of chart
                      open = ~BTC.USD.Open, close = ~BTC.USD.Close, # set dates when markets open and close
                      high = ~BTC.USD.High, low = ~BTC.USD.Low) # set higher and lower stock prices
candlestick <- candlestick %>% layout(title = "Daily Bitcoin stock prices: September 17, 2017 - October 16, 2023", # chart title
                      xaxis = list(rangeslider = list(visible = F))) 

candlestick

### Patterns ----
# https://www.investopedia.com/articles/technical/112601.asp
# https://www.quora.com/How-far-are-the-chart-patterns-reliable-for-stock-trading


# Double Top and Bottom
# The double top or bottom are reversal patterns, 
# signaling areas where the market has made two unsuccessful 
# attempts to break through a support or resistance level.
# 
# A double top often looks like the letter M and is an 
# initial push up to a resistance level followed by a 
# second failed attempt, resulting in a trend reversal.