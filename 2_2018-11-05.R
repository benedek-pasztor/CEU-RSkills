#2_2018-11-05

## last time
minus_or_plus <- function(n) {
  sample(c(-1,1), n, replace = TRUE)
}

minus_or_plus(5)

?replicate
res <- replicate(n = 1000, expr = sum(minus_or_plus(500)))
hist(res)

?lapply
res <- lapply(1:1000, function (i) cumsum(minus_or_plus(500)))
str(res) # list of 500-long vectors 

res <- do.call(cbind, res) # calls all vectors into a matrix
str(res) # matrix

####
res <- sapply(1:1000, function (i) cumsum(minus_or_plus(500)))
str(res) ### matrix straight




set.seed(42)
res <- sapply(1:1000, function (i) cumsum(minus_or_plus(500)))
str(res) ### matrix straight

## TODO list all simulations from res when the value did not become negative in the simulation
## egy esetben sem volt negativ
res[500,1]
res[500,2]
?apply  # effective with this exercise
apply(res, 2, function(x) which(x >= 0))

x <- res[,1] # 1st simulation
which(x >= 0)
min(x)  # min value of 1st simulation

apply(res, MARGIN = 2, FUN = min) # by column
apply(res, MARGIN = 2, FUN = min) >= 0 # logical vector of 1000
which(apply(res, MARGIN = 2, FUN = min) >= 0) ### only the columnindex of TRUE vectors
res[,23]




## TODO roll 3 dices (1-6), print the result
sample(1:6, 3, replace = TRUE)
hist(sample(1:6, 1000, replace = TRUE)) ## OK
hist(round(runif(n=1000, min = 1, max = 6))) ## not good, 1 and 6 are only half probability

dices <- function() sample(1:6, 3, replace = TRUE)
dices()  
dices <- function(n = 3) sample(1:6, n, replace = TRUE) # default value set
dices(5)

dices_sum <- function() sum(dices())
dices_sum()



## TODO roll 3 dices 1000 times and plot the number of points in ach simulation
dices()
hist(replicate(1e6, dices_sum())) # for some reason 18 is less likely than 3 - probably it is 
x <- replicate(1e4, dices_sum())
str(x)

barplot(table(x))
hist(x, breaks = 1:19)

## TODO how many times out of 1K simulaitons we had the same points on each dice?
set.seed(42)
res <- replicate(1000, dices())
str(res)
apply(res, MARGIN = 2, sd) == 0
which(apply(res, MARGIN = 2, sd) == 0)
res[, 998]

?duplicated

## TODO play some roulette -- we are betting on 18+ x 100
## we start with $100, always betting $1 # if it is higher than 18, then double, if less, then lose everything

roulette <- function() sample(0:36, 1, replace = TRUE)
roul_startm <- 100
y <- replicate(100, roulette())
if(y > 18){roul_startm + 1
}

#else {roul_startm -1
}

## Solving it together
roulette <- function() sample(0:36, 1, replace = TRUE)
wallet <- 100
bet <- 1
for (i in 1:100){
  wallet <- wallet - bet
  number <- roulette()
  if (number > 18){
    cat('WIN!!! \n')
    wallet <- wallet + bet * 2
  } else {
    cat('LOST :/ \n')
  }
}
wallet 

cat(5)
print(5)

## TODO vectorize
sample(0:36, 100, replace = TRUE) > 18
## TRUE => +1
## FALSE => -1
100 + cumsum(ifelse(sample(0:36, 100, replace = TRUE) > 18, +1, -1))

download.file('http://bit.ly/hun-cities-distance', 'cities.xls', mode = 'wb')
library(readxl)
cities <- read_excel('cities.xls')
str(cities)

cities <- cities[, -1]
str(cities)              
cities <- cities[1:40, ]
str(cities)

## Multi-Dimensional Scaling
mds <- cmdscale(as.dist(cities))
plot(mds)
text(mds[, 1], mds[, 2], names(cities))
a <-dist(mds)


mds2 <- cmdscale(as.dist(eurodist))
plot(mds2)
text(mds2[, 1], mds2[, 2], names(mds2))


mtcars
mds <- cmdscale(dist(mtcars))
mds
plot(mds)
text(mds[, 1], mds[, 2], row.names(mds))

str(mds)
mds <- as.data.frame(mds)
str(mds)
mds$car <- row.names(mtcars)
str(mds)

library(ggplot2)

ggplot(mds, aes(x = V1, y = V2, label = car)) + geom_text()

library(ggrepel)
ggplot(mds, aes(x = V1, y = V2, label = car)) + geom_text_repel()
