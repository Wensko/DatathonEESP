library(mvtnorm)
library(BLModel)
library(PerformanceAnalytics)
library(quantmod)

getwd()

all_coins_resume <- read.csv2("all-coins-resume.csv", header = TRUE, sep = ",")
binance <- read.csv2("binance-usd-rates.csv", header = TRUE, sep = ",")
dai <- read.csv2("dai-rates.csv", header = TRUE, sep = ",")
gemini <- read.csv2("gemini-dollar-rates.csv", header = TRUE, sep = ",")
susd <- read.csv2("s-usd-rates.csv", header = TRUE, sep = ",")
tusd <- read.csv2("true-usd-rates.csv", header = TRUE, sep = ",")
usdc <- read.csv2("usd-coin-rates.csv", header = TRUE, sep = ",")
usdp <- read.csv2("usdp-rates.csv", header = TRUE, sep = ",")
usdt <- read.csv2("usdt-coin-rates.csv", header = TRUE, sep = ",")




  k = 3
num =100
dat <- cbind(rmvnorm (n=num, mean = rep(0,k), sigma=diag(k)), matrix(1/num,num,1))
# a data sample with num rows and (k+1) columns for k assets;
returns_freq = 52 # we assume that data frequency is 1 week
w_m <- rep(1/k,k) # benchmark portfolio, a vector of length k,
SR = 0.5 # Sharpe ratio
Pe <- diag(k) # we assume that views are "absolute views"
qe <- rep(0.05, k) # user's opinions on future returns (views)
tau = 0.02
BL_post_distr(dat, returns_freq, NULL, w_m, SR, Pe, qe, tau, risk = "MAD", alpha = 0,
              views_distr = observ_normal, "diag", cov_matrix = NULL)

w_m <- rep(1/k,k) # market portfolio.
RM = 0.05 # market expected return.
equilibrium_mean (dat, w_m, RM, risk = "CVAR", alpha = 0.95)

observ_normal (x = matrix(c(rep(0.5,k),rep(0.2,k)),k,2), q = matrix(0,k,1),
               covmat = diag(k))
observ_powerexp (x = matrix(c(rep(0.5,k),rep(0.2,k)),k,2), q = matrix(0,k,1),
                 covmat = diag(k), beta = 0.6)

observ_ts (x = matrix(c(rep(0.5,k),rep(0.2,k)),k,2), q = matrix(0,k,1), covmat = diag(k),
           df=5)




