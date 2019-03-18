library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

options(scipen=999)

pokemon <- read.csv('data/Pokemon.csv')
pokemon <- rename(pokemon, 'Special.Attack' = 'Sp..Atk', 'Special.Defense' = 'Sp..Def')


# Summary
str(pokemon)
summary(pokemon)

colorYellow = '#FADA5E'
colorBlue = '#378CC7'


# Total
ggplot(pokemon) + geom_bar(aes(x = Generation), fill = colorYellow)
ggplot(pokemon) + geom_bar(aes(x = Legendary), fill = colorYellow)
ggplot(pokemon) + geom_bar(aes(x = Type.1), fill = colorBlue) 
ggplot(pokemon) + geom_bar(aes(x = Type.2), fill = colorBlue)

ggplot(pokemon) + geom_density(aes(x = Total), fill = colorYellow, colour = colorYellow) 
ggplot(pokemon) + geom_density(aes(x = Total), fill = colorBlue, colour = colorBlue) + facet_grid(Legendary ~ .)
ggplot(pokemon) + geom_density(aes(x = Total), fill = colorYellow, colour = colorYellow) + facet_grid(Generation ~ .)
ggplot(pokemon) + geom_density(aes(x = Total), fill = colorBlue, colour = colorBlue) + facet_grid(Generation ~ Legendary)

# Other
p1 <- ggplot(pokemon) + geom_density(aes(x = HP), fill = colorYellow, colour = colorYellow)
p2 <- ggplot(pokemon) + geom_density(aes(x = Attack), fill = colorYellow, colour = colorYellow)
p3 <- ggplot(pokemon) + geom_density(aes(x = Defense), fill = colorYellow, colour = colorYellow)
p4 <- ggplot(pokemon) + geom_density(aes(x = Special.Attack), fill = colorYellow, colour = colorYellow)
p5 <- ggplot(pokemon) + geom_density(aes(x = Special.Defense), fill = colorYellow, colour = colorYellow)
p6 <- ggplot(pokemon) + geom_density(aes(x = Speed), fill = colorYellow, colour = colorYellow)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

########### Correlation

library(corrplot) 

# Correlation plot
pokemonCorrelation <- cor(pokemon[, c(6:11)], method="pearson") 
print(dataCorrelation, digits=2)
corrplot(dataCorrelation, order ="alphabet")



