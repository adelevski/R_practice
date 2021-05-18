library(quantmod)
library(PerformanceAnalytics)

tickers <- c("FB", "AAPL", "AMZN", "NFLX")
weights <- c(.25, .25, .25, .25)

portfolioPrices <- NULL
for(ticker in tickers){
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols.yahoo(ticker, from='2016-01-03', periodicity = 'daily', auto.assign = F)[,4])
}
colSums(is.na(portfolioPrices))
portfolioReturns <- na.omit(ROC(portfolioPrices))

benchmarkPrices = getSymbols.yahoo('^GSPC', from='2016-01-03', periodicity = 'daily', auto.assign = F)[,4]
colSums(is.na(benchmarkPrices))
benchmarkReturns <- na.omit(ROC(benchmarkPrices))

portfolioReturn <- Return.portfolio(portfolioReturns)

CAPM.beta(portfolioReturn, benchmarkReturns, 0.035/252)

CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, 0.035/252)

SharpeRatio(portfolioReturn, .035/252)

table.AnnualizedReturns(portfolioReturn)
table.CalendarReturns(portfolioReturn)
