mds <- cmdscale(dist(mtcars))
mds
plot(mds)

library(ggplot2)
mds <- as.data.frame(mds)

ggplot(mds, aes(V1, V2, label = rownames(mtcars))) + geom_text()

mtcars
which(rownames(mtcars) == 'Camaro Z28')
mds[24, ]

str(dist(mtcars))      

str(as.matrix(dist(mtcars)))

sort(as.matrix(dist(mtcars))[, 24])

str(mtcars)
library(data.table)  
dtcars <- data.table(mtcars, keep.rownames = TRUE)
dtcars

setorder(dtcars, rn)
dtcars

setorder(dtcars, hp)
dtcars

mds

## standardizing --> transform data to mean = 0, sd = 1
dtcars$hp
dtcars$am

summary(dtcars)

x <- (dtcars$hp - mean(dtcars$hp)) / sd(dtcars$hp)
sd(x)
hist(x)

scale(dtcars$hp)
x

plot(x, scale(dtcars$hp)) # manually calculated vs with the scale function -> pretty much the same! :) 

scale(mtcars)

mds <- cmdscale(dist(scale(mtcars)))
mds <- as.data.frame(mds)
ggplot(mds, aes(V1, V2, label = rownames(mtcars))) + geom_text()

##########################################################################################
## time-series

install.packages('devtools')
devtools::install_github('daroczig/binancer') #using solely that function from the package

library(binancer)
?binance_klines
binance_klines('ETHBTC')

prices <- binance_klines('BTCUSDT', interval = '1d')
str(prices)

?ts
tsx <- ts(prices$close, frequency = 7)
plot(tsx)
plot(decompose(tsx))
str(decompose(tsx))

decompose(tsx)$seasonal[1:7]

install.packages('forecast')
library(forecast)

plot(tsx)
fit <- naive(tsx)
plot(fit)
fit
accuracy(fit) ## in-sample => cross validation

?ma
lines(ma(tsx, 2), col = 'blue')
lines(ma(tsx, 7), col = 'red')
lines(ma(tsx, 4 * 7), col = 'green')

fit <- ses(tsx)
plot(fit)
lines(fitted(fit), col = 'red')
accuracy(fit)
accuracy(fit)

fit <- auto.arima(tsx)
plot(fit)
fit
fit
plot(predict(fit))

#############################################################################################

install.packages('fpp2')
library(fpp2)
plot(gasoline)

autoplot(forecast(gasoline))

naive(gasoline)
autoplot(forecast(naive(gasoline)))
autoplot(forecast(naive(gasoline, h = 52))) # h -> how many forecasts I would like to see here

ma(gasoline, 4)
ses(gasoline)
?ma

autoplot(forecast(ses(gasoline, h = 52)))

accuracy(forecast(ses(gasoline, h = 52)))
accuracy(forecast(naive(gasoline, h = 52)))

fit <- ets(gasoline)
frequency(gasoline)

fit <- ets(ts(gasoline, frequency = 4)) # frequency = 4 -> monthly patterns (4 weeks)
autoplot(forecast(fit, h = 52))

accuracy(forecast(fit, h = 52))

## slow
fit <- tbats(gasoline)
fit
autoplot(forecast(fir, h = 52))
autoplot(forecast(fir, h = 52))