library(readr)
library(tidyverse)
library(xts)
library(tseries)
library(fPortfolio)

all_coins_resume <- read_csv("all-coins-resume.csv")
binance <- read_csv("binance-usd-rates.csv")
dai <- read_csv("dai-rates.csv")
gemini <- read_csv("gemini-dollar-rates.csv")
susd <- read_csv("s-usd-rates.csv")
tusd <- read_csv("true-usd-rates.csv")
usdc <- read_csv("usd-coin-rates.csv")
usdp <- read_csv("usdp-rates.csv")
usdt <- read_csv("usdt-coin-rates.csv")

names <- c('binance', 'dai', 'gemini', 'susd', 'tusd','usdc', 'usdp', 'usdt')

coins <- list(binance, dai, gemini, susd,  tusd ,usdc, usdp, usdt)
names(coins) <- names

data = list()
for (i in names(coins)) {
  for (j in names(coins[[i]])[c(6,8)]) {
    coins[[i]][j] = coins[[i]][j]+1
  }
  coins[[i]] <- coins[[i]][coins[[i]]$`x/hours`==19,]
  data[[i]] <- zoo::as.Date(with(coins[[i]], paste(`x/year`, `x/month`, `x/date`, `x/hours`, sep = '-')))
  # coins[[i]] <- xts(coins[[i]], order.by = data, frequency = 'months', unique = T)
  }


list2env(coins, .GlobalEnv)

binance <- xts(binance, order.by = data$binance)
dai <- xts(dai, order.by = data$dai)
gemini <- xts(gemini, order.by = data$gemini)
susd <- xts(susd, order.by = data$susd)
tusd <- xts(tusd, order.by = data$tusd)
usdc <- xts(usdc, order.by = data$usdc)
usdp <- xts(usdp, order.by = data$usdp)
usdt <- xts(usdt, order.by = data$usdt)
coins <- list(binance, dai, gemini, susd,  tusd ,usdc, usdp, usdt)
names(coins) <- names

returns <- xts()
for (i in names) {
  returns <- merge(returns, coins[[i]]$variableBorrowRate_avg, all = T)
}
names(returns) <- names

markowitz.base1 <- returns[complete.cases(returns[,1:6]),1:6]
markowitz.base1 <- as.timeSeries(markowitz.base1)

markowitz1.tangency <- tangencyPortfolio(markowitz.base1, spec = portfolioSpec())

markowitz1.minvar <- minvariancePortfolio(markowitz.base1, spec = portfolioSpec())

markowitz1.frontier <- portfolioFrontier(markowitz.base1, spec = portfolioSpec())

plot(markowitz1.frontier,c(1,2,3))
