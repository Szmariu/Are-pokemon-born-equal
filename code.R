library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

options(scipen=999)

pokemon <- read.csv('data/Pokemon.csv')
pokemon <- rename(pokemon, 'Special.Attack' = 'Sp..Atk', 'Special.Defense' = 'Sp..Def')

#### Are Pokemon born equal? 

# Pikachu!
pokemon %>% filter(pokemon$Name == 'Pikachu')

# Summary
dim(pokemon)
str(pokemon)
summary(pokemon)

# Define nice colors
cYellow = '#FADA5E'
cBlue = '#378CC7'


# Total
ggplot(pokemon) + geom_bar(aes(x = Generation), fill = cYellow)
ggplot(pokemon) + geom_bar(aes(x = Legendary), fill = cYellow)
ggplot(pokemon) + geom_bar(aes(x = Type.1), fill = cBlue) 
ggplot(pokemon) + geom_bar(aes(x = Type.2), fill = cBlue)

ggplot(pokemon) + geom_density(aes(x = Total), fill = cYellow, colour = cYellow) 
ggplot(pokemon) + geom_density(aes(x = Total), fill = cBlue, colour = cBlue) + facet_grid(Legendary ~ .)
ggplot(pokemon) + geom_density(aes(x = Total), fill = cYellow, colour = cYellow) + facet_grid(Generation ~ .)
ggplot(pokemon) + geom_density(aes(x = Total), fill = cBlue, colour = cBlue) + facet_grid(Generation ~ Legendary)

# Other
p1 <- ggplot(pokemon) + geom_density(aes(x = HP), fill = cYellow, colour = cYellow)
p2 <- ggplot(pokemon) + geom_density(aes(x = Attack), fill = cYellow, colour = cYellow)
p3 <- ggplot(pokemon) + geom_density(aes(x = Defense), fill = cYellow, colour = cYellow)
p4 <- ggplot(pokemon) + geom_density(aes(x = Special.Attack), fill = cYellow, colour = cYellow)
p5 <- ggplot(pokemon) + geom_density(aes(x = Special.Defense), fill = cYellow, colour = cYellow)
p6 <- ggplot(pokemon) + geom_density(aes(x = Speed), fill = cYellow, colour = cYellow)
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 2)

########### Correlation

library(corrplot) 

# Correlation plot
pokemonCorrelation <- cor(pokemon[, c(6:11)], method="pearson") 
print(dataCorrelation, digits=2)
corrplot(dataCorrelation, order ="alphabet")


# Basic MDS

# 3 warianty danych do wyboru - jakoś pokazać
poke <- pokemon[, c(5:11)]

# Bez total, są podobne
poke <- pokemon[, c(6:11)]

# Tylko legendarne, ciekawe wyniki
poke <- pokemon %>% filter(Legendary == 'True')
poke <- poke[, c(6:11)]
poke2 <- pokemon %>% filter(Legendary == 'True')

# Classical multidim scaling
poke.dist<-dist(poke) 
as.matrix(poke.dist)[1:10, 1:10]
poke.mds.1 <- cmdscale(poke.dist, k=2) 
summary(poke.mds.1)
plot(poke.mds.1) 
  
# Shuckle poprawnie zidentyfikowany - napisać o tym!
plot(poke.mds.1, type = 'n')
text(poke.mds.1, labels = pokemon$Name, cex=0.8, adj = 0.5)

# Surfaces in PCO
library("labdsv")
poke.mds.2<-pco(poke.dist, k=2) 
dev.off()
par(mfrow=c(2,4))
plot(poke.mds.2)
title(main = "PCO")

plot(poke.mds.2)
title(main = "Total")
surf(poke.mds.2, poke$Total)

plot(poke.mds.2)
title(main = "HP")
surf(poke.mds.2, poke$HP)

plot(poke.mds.2)
title(main = "Attack")
surf(poke.mds.2, poke$Attack)

plot(poke.mds.2)
title(main = "Defense")
surf(poke.mds.2, poke$Defense)

plot(poke.mds.2)
title(main = "Special Attack")
surf(poke.mds.2, poke$Special.Attack)

plot(poke.mds.2)
title(main = "Special Defense")
surf(poke.mds.2, poke$Special.Defense)

plot(poke.mds.2)
title(main = "Speed")
surf(poke.mds.2, poke$Speed)

par(mfrow=c(1,1))


# Analisying variables
poke.dist.t<-dist(t(poke))
poke.mds.3<-cmdscale(poke.dist.t, k=2) 
summary(poke.mds.3)	
plot(poke.mds.3, type = 'n') 
text(poke.mds.3, rownames(poke.mds.3), cex=0.8, adj = 0.5) 

# Without the total variable
poke.dist.t.2<-dist(t(poke[, c(2:7)])) 
poke.mds.4<-cmdscale(poke.dist.t.2, k=2) 
summary(poke.mds.4)
plot(poke.mds.4, type = 'n') 
text(poke.mds.4, rownames(poke.mds.4), cex=0.8, adj = 0.5) 






