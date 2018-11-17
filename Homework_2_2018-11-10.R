# 1. Set the seed to 42 and run a simulation 1000 times: playing roulette for 100 rounds, 
#starting with $100 budget, always betting $1 on numbers 18-36 in each round. 
#After saving the results in a helper variable, answer the below questions by using your saved results:
set.seed(42)

roulette <- function(){
  x <- 100 + sum(ifelse(sample(0:36, 100, replace = TRUE) >= 18, +1, -1))
}
result <- replicate(1000, roulette())
result <- as.data.frame(result)

#   1. Draw a histogram on the (1000) resulting budgets at the end of the 100-100 rounds.
library(ggplot2)
ggplot(result, aes(x = result)) +
  geom_histogram(color = 'black', fill = 'white') +
  labs(title="Roulette histogram",x="End budget ($)", y = "Count") +
  theme_classic()

# 2. What's the average amount of lost dollars in 100 rounds?
avglostdollars <- 100 - mean(result[which(result < 100), ])
#8.5 is the average lost dollars in 100 rounds. (amongst those which have losing outcome)


# 3. How many times (out of 1000) did we win at least $1?
atleastone <- length(result[which(result >= 101), ])
#555 times we have won at least 1 dollar

# 2. Draw a ggplot2 2D "map" of European cities by applying MDS on the "eurodist" dataset
citymap <- cmdscale(as.dist(eurodist))
citymap <- as.data.frame(citymap)
citymap$country <- row.names(citymap)
str(citymap)

# 1. Start with a scatterplot.
ggplot(citymap, aes(x = V1, y = V2, label = country)) + geom_point()

# 2. Add the city names as labels to the scatterplot.
ggplot(citymap, aes(x = V1, y = V2, label = country)) + geom_text()
library(ggrepel)
ggplot(citymap, aes(x = V1, y = V2, label = country)) + geom_text_repel()

# Fix the north-south and east-west orientation if needed.
ggplot(citymap, aes(x = V1, y = -V2, label = country)) + geom_text_repel()
# North-South mirror was needed ->  -V2
