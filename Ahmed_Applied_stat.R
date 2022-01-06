
#1. Downloading the stock data

## Set parameters
first.date <- Sys.Date() - 2500
last.date <- Sys.Date()
freq.data <- "monthly"
tickers <- c("AAPL")

#2. Get Stock Prices
require("BatchGetSymbols")

stocks <- BatchGetSymbols(tickers = tickers,
                          first.date = first.date,
                          last.date = last.date,
                          freq.data = freq.data,
                          do.cache = FALSE,
                          thresh.bad.data = 0)


#2. Get Returns
require("data.table")
stocks_DT <- stocks$df.tickers %>% setDT() %>%          # Convert to data.table
  .[order(ticker, ref.date)]                           # Order by ticker and date

#3. Summary of each feature
summary(stocks_DT)

#4. Moving average
require("zoo")
require("dplyr")
stocks_DT <- stocks_DT %>%
  mutate(adjusted10 = rollmean(price.adjusted, k = 10, fill = NA),
         adjusted30 = rollmean(price.adjusted, k = 30, fill = NA),)

#4. Graph Returns, Prices and moving average
require("ggplot2")
returns_plot_all <- ggplot(stocks_DT, aes(x= ref.date, y = ret.adjusted.prices, colour = ticker)) +
  geom_line() + theme_bw() + labs(title = "", x = "Date", y= "Monthly Returns", subtitle = "")

price_plot_all <- ggplot(stocks_DT, aes(x= ref.date, y = price.close, colour = ticker)) +
  geom_line() + theme_bw() + labs(title = "", x = "Date", y= "Closing Price", subtitle = "")

returns_sep <- ggplot(stocks_DT[ticker %in% c("AAP")], aes(x = ref.date, y = ret.adjusted.prices)) + geom_line() + facet_wrap(~ticker, scales = "free_y") + theme_bw()
prices_sep <- ggplot(stocks_DT[ticker %in% c("AAPL")], aes(x = ref.date, y = price.close)) + geom_line() + facet_wrap(~ticker, scales = "free_y") + theme_bw()

ggplot(stocks_DT, aes(x=ref.date))+
  geom_line(aes(y=price.adjusted, color="Adjusted price"))+
  geom_line(aes(y=adjusted10, color="10 days mouving average"))+
  geom_line(aes(y=adjusted30, color="30 days mouving average"))+
  guides(colour=guide_legend(title=""))+
  ggtitle("Adjusted price and moving averages of adjusted price of Apple")+
  theme_bw()+
  theme(legend.position = "bottom")

