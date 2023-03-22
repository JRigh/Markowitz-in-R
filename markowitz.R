#------------------------------------------------
# RETRIEVE STOCK PRICES AND COMPUTE DAILY RETURNS
#------------------------------------------------

library(quantmod)
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
library(corrplot)
Assets <- cor(assets)
#            TESLA      APPLE       META       AMZN
# TESLA  1.0000000  0.9806590 -0.9958627 -0.6402810
# APPLE  0.9806590  1.0000000 -0.9883881 -0.6081574
# META  -0.9958627 -0.9883881  1.0000000  0.5797376
# AMZN  -0.6402810 -0.6081574  0.5797376  1.0000000
corrplot(Assets, method="number")



#------------------------------------------------
# CHOLESKI TO SIMULATE FROM MULTIVARIATE GAUSSIAN
#------------------------------------------------

# parameters
n <- 10000
assets <- matrix(cbind(TSLA$TSLA.Adjusted, AAPL$AAPL.Adjusted, META$META.Adjusted,
                       AMZN$AMZN.Adjusted), byrow = FALSE, ncol = 4)
colnames(assets) <- c("TESLA", "APPLE", "META", "AMZN")
Assets <- cor(assets)
# correlations
rho_12 <- Assets[1,2]; rho_13 <- Assets[1,3]; rho_14 <- Assets[1,4] 
rho_23 <- Assets[2,3]; rho_24 <- Assets[2,4]; rho_34 <- Assets[3,4]
# standard deviations
sds = apply(assets,2, sd)
s1 <- sds[[1]]; s2 <- sds[[2]]; s3 <- sds[[3]]; s4 <- sds[[4]]
# means
mu <- as.numeric(colMeans(assets))
# covariance matrix, eg. cov_12 = s1*s2*rho_12
Sigma <- round(matrix(c(s1^2, s1*s2*rho_12, s1*s3*rho_13, s1*s4*rho_14,
                  s1*s2*rho_12, s2^2, s2*s3*rho_23, s2*s4*rho_24, 
                  s1*s3*rho_13, s2*s3*rho_23, s3^2, s2*s4*rho_24,
                  s1*s4*rho_14, s2*s4*rho_24,s3*s4*rho_34,s4^2), nrow = 4))

# function to sample from a Multivariate Normal distribution
mv.cholesky <- function(n, mu, Sigma) {
  
  d <- length(mu)
  Q <- chol(Sigma)
  Z <- matrix(rnorm(n*d), nrow = n, ncol = d)
  X <- Z %*% Q +  rep(1,n) %*% t(mu)
  X <- data.frame(X)
  return(X)
}

# call the function and create a sample of desired size
set.seed(1986)
random.sample <- mv.cholesky(n = n, mu = mu, Sigma = Sigma)


## plotting

library(ggplot2)
library(gridExtra)
library(grid)

htop <- ggplot(data=random.sample, aes(x=X1)) + 
  geom_histogram(aes(y=..density..), fill = "grey90", color = "black", binwidth = 0.3) + 
  stat_density(colour = "red3", geom="line", size = 1.2, position="identity", show.legend=FALSE) +
  theme(axis.title.x = element_blank(),
        panel.background = element_blank())

blank <- ggplot() + geom_point(aes(1,1), colour="white") +
  theme(axis.ticks=element_blank(), panel.background=element_blank(), panel.grid=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank())

scatter <- ggplot(data=random.sample, aes(x=X1, y=X2)) + 
  geom_point(size = 0.6, pch=3) + 
  stat_ellipse(level = 0.80, linetype = 1, color="red2", size=1) +
  stat_ellipse(level = 0.95, linetype = 1, color="red2", size=0.8) +
  stat_ellipse(level = 0.99, color="red3", size=0.6) +
  theme_light()

hright <- ggplot(data=random.sample, aes(x=X2)) + 
  geom_histogram(aes(y=..density..), fill = "grey90", color = "black", binwidth = 0.3) + 
  stat_density(colour = "red3", geom="line", size = 1.2, position="identity", show.legend=FALSE) +
  coord_flip() + theme(axis.title.y = element_blank(),
                       panel.background = element_blank())

grid.arrange(htop, blank, scatter, hright, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4),
             top=textGrob("Scatterplot of the simulated joint distribution of asset 1 (TESLA) and asset 2 (APPLE)"))
