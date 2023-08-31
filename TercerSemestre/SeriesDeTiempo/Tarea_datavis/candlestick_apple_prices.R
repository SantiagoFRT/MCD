# Install packages
install.packages(plotly) # for candlestick chart
install.packages(quantmod) # for financial data from the US stock market

# Set up libraries
library(plotly)
library(quantmod)

# Get Apple Stock data from yahoo finance
getSymbols("AAPL",src='yahoo')

## [1] "AAPL"

# set dataframe
df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)

# set candlestick chart
candlestick <- df %>% plot_ly(x = ~Date, type="candlestick", # set x axis data and kind of chart
                      open = ~AAPL.Open, close = ~AAPL.Close, # set dates when markets open and close
                      high = ~AAPL.High, low = ~AAPL.Low) # set higher and lower stock prices
candlestick <- candlestick %>% layout(title = "Apple stock prices: June 20 to August 30, 2023", # chart title
                      xaxis = list(rangeslider = list(visible = F))) 

candlestick
