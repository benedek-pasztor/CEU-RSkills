## TODO draw a sine wave
curve(sin, 0, pi * 2) # solution oneliner

x <- seq(0, pi * 2, by = 0.1) #solution with helper variable
sin(x)
plot(x, sin(x), type = 'l') 


## TODO draw 2x+1
f <- function(x){
  2 * x + 1
  invisible(NULL)}
f
f(pi)

plot(x, f(x), type = 'l')
str(f(1:5))

## TODO draw 2x+1 and x+3
g <- function(x){
  x + 3
}

plot(x, f(x), type = 'l')
plot(x, g(f), type = 'l')

?curve
curve(f, 0, 10)
curve(g, 0, 10, add = TRUE, col = 'red')


## TODO simulation Brownian motion (random walk) in 1D for 25 steps
vector <- 0:500
steps <- seq(-1, 1, by = 2)
i=2
while(i <= 25){
  vector(i) <- vector(i-1) + sample(c(-1,1), 1)
  i = i + 1
}

#many ways are in Onenote

x <- 0
for (i in 1:25) {
  x <- x + sample(c(-1, 1), 1)
} # it's too complicated, let's do it in another way

sum(round(runif(25)) *2 - 1)
sum(sample(c(-1, 1), 25, replace = TRUE))

plot(cumsum(sample(c(-1, 1), 500, replace = TRUE)), type = 's')
lines(cumsum(sample(c(-1, 1), 500, replace = TRUE)), type = 's', col = 'red')

for (i in 1:5){
  lines(cumsum(round(runif(500)) *2 -1), col = i, type = 's')
}

set.seed(42) # - setting the random number generator to a specific state-kindof


## TODO plot a histogram of results of running the above simulation for 1K times each with 500 rounds
library(data.table)
df <- data.frame(
  'v' = numeric()
)

x <- sum(round(runif(500)) *2 -1)
for (i in 1:1000){
  x <- c(x, sum(round(runif(500)) *2 -1))
}

x <- 1:1000
for (i in 1:1000){
  x[i] <- sum(round(runif(500)) *2 -1)
}
hist(x)

res <- replicate(n = 1000, sum(round(runif(500)) *2 -1))

system.time(replicate(n = 1000, sum(round(runif(500)) *2 -1)))


## TODO plot a histogram doing the same simulation but after the 400th iteration
res <- replicate(n = 1000, cumsum(round(runif(500)) *2 -1))
hist(res[400, ])

res <- lapply(1:1000, function(i){
  cumsum(round(runif(500)) *2 -1)
})

res <- do.call(rbind, res)
str(res)

library(data.table)
res <- lapply(1:1000, function(i){
  data.frame(cumsum(round(runif(500)) *2 -1))
})
rbindlist(res)

## TODo plot a histogram doing the same simulation but after the 100th iteration
library(animation)
install.packages('animation')
saveGIF({
  for (i in seq(1, 500, by = 10)){
    hist(res[, i], main = i)
  }
})
?saveGIF

## TODO compute the minimum value of each simulation
apply(res, 1, min)
apply(res, 2, min)
