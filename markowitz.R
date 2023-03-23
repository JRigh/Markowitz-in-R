#------------------------------------------------
# RETRIEVE STOCK PRICES AND COMPUTE DAILY RETURNS
#------------------------------------------------

library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(corrplot)

# retrieve stock prices from Yahoo finance
# Tesla, Inc.
TSLA <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2022-06-01", auto.assign = FALSE)
# Apple Inc.
AAPL <- getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2022-06-01", auto.assign = FALSE)
# Meta Platforms, Inc.
META <- getSymbols("META", src = "yahoo", from = "2020-01-01", to = "2022-06-01", auto.assign = FALSE)
# Amazon.com, Inc.
AMZN <- getSymbols("AMZN", src = "yahoo", from = "2020-01-01", to = "2022-06-01", auto.assign = FALSE)

# time series plot of the different stocks
plot(TSLA$TSLA.Adjusted, main = 'Stock prices of Tesla (black), Apple (red), Meta (blue), Amazon (green)')
lines(AAPL$AAPL.Adjusted, col = 'darkred')
lines(META$META.Adjusted, col = 'darkblue')
lines(AMZN$AMZN.Adjusted, col = 'darkgreen')

# daily log returns

# construction of correlation plot
assets <- matrix(cbind(TSLA$TSLA.Adjusted, AAPL$AAPL.Adjusted, META$META.Adjusted, AMZN$AMZN.Adjusted), 
       byrow = FALSE, ncol = 4)
colnames(assets) <- c("TESLA", "APPLE", "META", "AMZN")

# means
means = colMeans(assets)
# TESLA APPLE  META  AMZN 
#   199   125   270   150 
# standard deviations
sds = apply(assets,2, sd)
# TESLA APPLE  META  AMZN 
#   103    32    58    25 
# covariances
covs = round(cov(assets))
#       TESLA APPLE META AMZN
# TESLA 10578  3146 3088 1741
# APPLE  3146  1044 1015  568
# META   3088  1015 3382 1166
# AMZN   1741   568 1166  628
Assets <- cor(assets)
#            TESLA      APPLE       META       AMZN
# TESLA  1.0000000  0.9806590 -0.9958627 -0.6402810
# APPLE  0.9806590  1.0000000 -0.9883881 -0.6081574
# META  -0.9958627 -0.9883881  1.0000000  0.5797376
# AMZN  -0.6402810 -0.6081574  0.5797376  1.0000000
corrplot(Assets, method="number")



# retrieve stock prices from Yahoo finance
# Tesla, Inc.
TSLA <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2022-06-01", 
                   auto.assign = FALSE)
# Apple Inc.
AAPL <- getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2022-06-01",
                   auto.assign = FALSE)
# Meta Platforms, Inc.
META <- getSymbols("META", src = "yahoo", from = "2020-01-01", to = "2022-06-01", 
                   auto.assign = FALSE)
# Amazon.com, Inc.
AMZN <- getSymbols("AMZN", src = "yahoo", from = "2020-01-01", to = "2022-06-01",
                   auto.assign = FALSE)
# time series plot of the different stocks
plot(TSLA$TSLA.Adjusted, main = 'Stock prices ...')
lines(AAPL$AAPL.Adjusted, col = 'darkred')
lines(META$META.Adjusted, col = 'darkblue')
lines(AMZN$AMZN.Adjusted, col = 'darkgreen')

# daily log returns
TSLAreturns = Return.calculate(TSLA[,6],method="log")
TSLAreturns = TSLAreturns[(-1)]
AAPLreturns = Return.calculate(AAPL[,6],method="log")
AAPLreturns = AAPLreturns[(-1)]
METAreturns = Return.calculate(META[,6],method="log")
METAreturns = METAreturns[(-1)]
AMZNreturns = Return.calculate(AMZN[,6],method="log")
AMZNreturns = AMZNreturns[(-1)]


returns<- data.frame(cbind(TSLAreturns, AAPLreturns, METAreturns, AMZNreturns))


# mean returns
mean.returns <- as.numeric(colMeans(returns))
# anualized risk (stadard deviation) of returns
cov.returns.anualized <- cov(returns) * 252
# simulations
nsim <- 10000
# storage objects
Aweights <- matrix(rep(0, nsim*4), nrow = nsim, ncol = 4)
Returns.Port <- numeric(nsim)
Risk.Port <- numeric(nsim)
weights <- numeric(4)
Weights <-matrix(rep(0, nsim*4), nrow = nsim, ncol = 4)

set.seed(2023)
for(i in 1:nsim) {
  weights <- runif(4)
  sweights <- sum(weights)
  Weights[i, ] <- weights/sweights
  # Portfolio return
  returns.Port <- sum((weights/sweights) * mean.returns)
  Returns.Port[i] <- ((returns.Port + 1)^252) - 1
  # Rortfolio risk
  Risk.Port[i] <- sqrt(t((weights/sweights)) %*% (cov.returns.anualized  
                                                  %*% (weights/sweights)))
}
Portfolios <- matrix(cbind(Weights,Returns.Port,Risk.Port), 
                     byrow = FALSE, ncol = 6)
colnames(Portfolios) <- c("TESLA", "APPLE", "META", "AMZN", "Return", "Risk")
head(round(Portfolios,4))

# portfolios with risk < 36% and return > 25%
Portfolios <- data.frame(Portfolios)

Portfolios %>%
  ggplot(aes(x = Risk, y = Return, color = Risk)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_gradient(low = "firebrick4", high = "coral") +
  labs(x = 'Annualized Risk (standard deviation)',
       y = 'Annualized Returns',
       title = "Portfolios with risk < 30% and return > 20%") +
  geom_point(aes(x = Risk,
                 y = Return), data = Portfolios[which(Portfolios$Risk < 0.36 & 
                                                        Portfolios$Return > 0.25), ], 
             color = 'black', size = 5) +
  annotate('text', x = 0.39, y = 0.7, label = "Portfolio selection (black)") +
  theme_minimal()

PortfolioSelection <- Portfolios[which(Portfolios$Risk < 0.36 &
                                         Portfolios$Return > 0.25), ]

barplot(colMeans(PortfolioSelection[, 1:4]), 
        main = 'Optimal composition for Portfolios with risk < 36% and return > 25%',
        xlab = 'Assets', ylab = 'Weight in Portfolio',
        col = c("blue", "darkblue", "darkred", "red"),
        legend = round(colMeans(PortfolioSelection[, 1:4]), 4)*100, beside = TRUE)

#----
# end
#----

#------------------
# tangent portfolio
#------------------

# retrieve stock prices from Yahoo finance
# Tesla, Inc.
TSLA <- getSymbols("TSLA", src = "yahoo", from = "2020-01-01", to = "2022-06-01", 
                   auto.assign = FALSE)
# Apple Inc.
AAPL <- getSymbols("AAPL", src = "yahoo", from = "2020-01-01", to = "2022-06-01",
                   auto.assign = FALSE)
# Meta Platforms, Inc.
META <- getSymbols("META", src = "yahoo", from = "2020-01-01", to = "2022-06-01", 
                   auto.assign = FALSE)
# Amazon.com, Inc.
AMZN <- getSymbols("AMZN", src = "yahoo", from = "2020-01-01", to = "2022-06-01",
                   auto.assign = FALSE)
# time series plot of the different stocks
plot(TSLA$TSLA.Adjusted, main = 'Stock prices ...')
lines(AAPL$AAPL.Adjusted, col = 'darkred')
lines(META$META.Adjusted, col = 'darkblue')
lines(AMZN$AMZN.Adjusted, col = 'darkgreen')

# daily log returns
TSLAreturns = Return.calculate(TSLA[,6],method="log")
TSLAreturns = TSLAreturns[(-1)]
AAPLreturns = Return.calculate(AAPL[,6],method="log")
AAPLreturns = AAPLreturns[(-1)]
METAreturns = Return.calculate(META[,6],method="log")
METAreturns = METAreturns[(-1)]
AMZNreturns = Return.calculate(AMZN[,6],method="log")
AMZNreturns = AMZNreturns[(-1)]

returns<- data.frame(cbind(TSLAreturns, AAPLreturns, METAreturns, AMZNreturns))


# mean returns
mean.returns <- as.numeric(colMeans(returns))
# anualized risk (stadard deviation) of returns
cov.returns.anualized <- cov(returns) * 252
# simulations
nsim <- 10000
# storage objects
Aweights <- matrix(rep(0, nsim*4), nrow = nsim, ncol = 4)
Returns.Port <- numeric(nsim)
Risk.Port <- numeric(nsim)
weights <- numeric(4)
Weights <-matrix(rep(0, nsim*4), nrow = nsim, ncol = 4)

set.seed(2023)
for(i in 1:nsim) {
  weights <- runif(4)
  sweights <- sum(weights)
  Weights[i, ] <- weights/sweights
  # Portfolio return
  returns.Port <- sum((weights/sweights) * mean.returns)
  Returns.Port[i] <- ((returns.Port + 1)^252) - 1
  # Rortfolio risk
  Risk.Port[i] <- sqrt(t((weights/sweights)) %*% (cov.returns.anualized  
                                                  %*% (weights/sweights)))
}
Portfolios <- matrix(cbind(Weights,Returns.Port,Risk.Port), 
                     byrow = FALSE, ncol = 6)
colnames(Portfolios) <- c("TESLA", "APPLE", "META", "AMZN", "Return", "Risk")
head(round(Portfolios,4))

# tangent portfolio (maximizing mean return over risk)
Portfolios <- data.frame(Portfolios)

p1<-  ggplot(Portfolios, aes(x = Risk, y = Return, color = Risk)) +
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  scale_colour_gradient2() +
  geom_point(aes(x = Risk,
                 y = Return), data = Portfolios[which.max(mean(Portfolios$Return)/Portfolios$Risk), ], 
             color = 'black', size = 3) +
  annotate('text', x = 0.39, y = 0.7, label = "Portfolio selection (black)") +
  labs(title = 'Tangent Portfolio - Maximizing the Sharpe ratio',
       subtitle = 'Portfolio of 4 stocks retrieved from Yahoo finance',
       y="Annualized Returns", x="Annualized Risk (standard deviation)") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))

# Portfolio selection
PortfolioSelection <- Portfolios[which(Portfolios$Risk < 0.36 &
                                         Portfolios$Return > 0.25), ]

dataset <- data.frame('Assets' = colnames(PortfolioSelection[1:4]), 
                      'Weights' = colMeans(PortfolioSelection[, 1:4]))
rownames(dataset) <- NULL

ggplot(dataset, aes(x = Assets, y = Weights, fill = Weights)) +
  geom_bar(stat="identity") + 
  scale_colour_gradient2() +
  labs(title = 'Portfolio selection for risk < 0.36 and return > 0.25',
       subtitle = 'Portfolio of 4 stocks retrieved from Yahoo financet',
       y="Weight in the portfolio", x="Assets") +
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        plot.subtitle=element_text(size=10, face="italic", color="darkred"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        panel.grid.major = element_line(colour = "grey90"))


#----
# end
#----
