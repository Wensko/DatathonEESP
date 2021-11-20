library(readr)
library(tidyverse)
library(xts)
library(tseries)
library(fPortfolio)
library(quantmod)

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

## Portfolios so c 6 moedas (as que tem dado ha mais tempo)

spec <- portfolioSpec( model = list(
  type = "MV", optimize = "minRisk",           
  estimator = "covEstimator", tailRisk = list(),
  params = list(alpha = 0.05)),
portfolio = list(
  weights = NULL, targetReturn = NULL,
  targetRisk = NULL, riskFreeRate = 0.02, nFrontierPoints = 50,
  status = NA),
optim = list(
  solver = "solveRquadprog", 
  objective = c("portfolioObjective", "portfolioReturn", "portfolioRisk"),
  options = list(meq = 2), control = list(), trace = FALSE),
messages = list(
  messages = FALSE, note = ""),
ampl = list(
  ampl = FALSE, project = "ampl", solver = "ipopt",
  protocol = FALSE, trace = FALSE)
)

markowitz.base1 <- returns[complete.cases(returns[,1:6]),1:6]
markowitz.base1 <- as.timeSeries(markowitz.base1, format = '%Y-%m-%d')

markowitz1.tangency <- tangencyPortfolio(markowitz.base1, spec = spec)

markowitz1.minvar <- minvariancePortfolio(markowitz.base1, spec = spec)

markowitz1.frontier <- portfolioFrontier(markowitz.base1, spec = spec)

plot(markowitz1.frontier,c(1,2,3))

##Portfolios com as 8 moedas

markowitz.base2 <- returns[complete.cases(returns[,1:8]),1:8]
markowitz.base2 <- as.timeSeries(markowitz.base2, format = '%Y-%m-%d')

markowitz2.tangency <- tangencyPortfolio(markowitz.base2, spec = spec)
markowitz2.minvar <- minvariancePortfolio(markowitz.base2, spec = spec)
markowitz2.frontier <- portfolioFrontier(markowitz.base2, spec=spec)

plot(markowitz2.frontier,c(1,2,3))

## Portfolio com 6 moedas e ultra short ETF e equity

getSymbols('JPST', from = '2021-01-01', to = '2021-11-14')
getSymbols('SPY', from = '2021-01-01', to = '2021-11-14')

ultrashort.etf.returns <- na.omit(diff(log(JPST[,4])))
spy.returns <- na.omit(diff(log(SPY[ ,4])))

markowitz.base3 <- merge(markowitz.base1, ultrashort.etf.returns)
markowitz.base3 <- merge(markowitz.base3, spy.returns)
markowitz.base3 <- markowitz.base3[complete.cases(markowitz.base3),]


markowitz3.tangency <- tangencyPortfolio(markowitz.base3, spec = spec)
markowitz3.minvar <- minvariancePortfolio(markowitz.base3, spec = spec)
markowitz3.frontier <- portfolioFrontier(markowitz.base3, spec = spec)

