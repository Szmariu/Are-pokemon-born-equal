library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(corrplot) 
library(labdsv)
library(smacof)
library(psych)
library(pca3d)
library(NbClust)
library(ClusterR)
library(wesanderson)
library(factoextra)
library(clustertend)
library(knitr)
options(scipen=999)


# Define nice colors
cYellow = '#FADA5E'
cBlue = '#378CC7'
 
# Read data
pokemon <- read.csv('data/Pokemon.csv')
pokemon <- rename(pokemon, 'Special.Attack' = 'Sp..Atk', 'Special.Defense' = 'Sp..Def')

#### Are Pokemon born equal? 

# Pikachu!
pokemon %>% filter(pokemon$Name == 'Pikachu')

# Summary
dim(pokemon)
str(pokemon)
summary(pokemon)

# 3 warianty danych do wyboru - jakoś pokazać
poke <- pokemon[, c(5:11)]

# Bez total, są podobne
poke <- pokemon[, c(6:11)]
# Overall view
plot(poke, col = cBlue, pch = 19)

# Tylko legendarne, ciekawe wyniki
poke <- pokemon %>% filter(Legendary == 'True')
poke <- poke[, c(6:11)]
poke2 <- pokemon %>% filter(Legendary == 'True')


# Summary plots
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

################# Correlation

# Correlation plot
pokemonCorrelation <- cor(pokemon[, c(6:11)], method="pearson") 
print(pokemonCorrelation, digits=2)
corrplot(pokemonCorrelation, order ="alphabet", method = 'number')


################ MDS

# Classical multidim scaling
poke.dist<-dist(poke) 
as.matrix(poke.dist)[1:10, 1:10]
poke.mds.1 <- cmdscale(poke.dist, k=2) 
summary(poke.mds.1)
plot(poke.mds.1) 
  
# Shuckle poprawnie zidentyfikowany - napisać o tym!
plot(poke.mds.1, type = 'n')
text(poke.mds.1, labels = pokemon$Name, cex=0.8, adj = 0.5)

# Surfaces in PCO + this for legendary
poke.mds.2<-pco(poke.dist, k=2) 
#dev.off()
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
poke <- pokemon[, c(6:11)]

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


################ Goodness of fit

# MDS 
poke <- pokemon[, c(6:11)]
poke.dist <- dist(t(poke))  
poke.mds.4 <- mds(poke.dist, ndim=2,  type="ordinal") # from smacof::
poke.mds.4
summary(poke.mds.4)
plot(poke.mds.4)

stress.random.matrix <- randomstress(n=800, ndim=2, nrep = 1) 
poke.mds.4$stress/ mean(stress.random.matrix)
# 0.11 - fair



############### PCA 

poke <- pokemon[, c(6:11)]

# PCA
poke.pca.1<-prcomp(poke, center=TRUE, scale.=TRUE) # stats::
a <- poke.pca.1$rotation
kable(a, digits = 2)
summary(poke.pca.1)
plot(poke.pca.1, type = "l")
fviz_pca_var(poke.pca.1, col.var="black")

# PCA 2
poke.pca.2<-princomp(poke)
loadings(poke.pca.2)
plot(poke.pca.2)
fviz_pca_var(poke.pca.2, col.var="black")

# Rotated and cut - easy interpretation

poke.pca.3 <- principal(poke, nfactors=3, rotate="varimax")
poke.pca.3
summary(poke.pca.3)
biplot(poke.pca.3, hist.col = cYellow, smoother = TRUE)
# printing only the significant loadings
print(loadings(poke.pca.3), digits=2, cutoff=0.4, sort=TRUE)


# Visualise 
fviz_pca_ind(poke.pca.3, col.ind="cos2", geom = "point", gradient.cols = c(cYellow, cBlue))

# 3d
poke.group <- factor(pokemon$Legendary)
pca3d(poke.pca.1, group = poke.group, legend="topleft")
pca3d(poke.pca.1, group = poke.group, biplot=TRUE, biplot.vars=3, legend="topleft") 


snapshotPCA3d(file="first_plot.png") 

############## Clustering k-means
# Prepare data
poke.dist<-dist(poke) 
poke.mds.1 <- cmdscale(poke.dist, k=2) 
poke.mds.1.center <- center_scale(poke.mds.1)
poke.mds.1.center <- poke.mds.1

# Optimal number of clusters
c3<-NbClust(poke.mds.1.center , distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="silhouette")
c3$All.index

fviz_nbclust(as.data.frame(poke.mds.1), FUNcluster=pam) 


# From ClusteR
poke.km <- KMeans_rcpp(poke.mds.1.center, clusters=4, num_init=30, max_iters = 10000) 
ggplot(as.data.frame(poke.mds.1.center)) + geom_point(aes(x = V1, y = V2, colour = poke.km$clusters)) + scale_colour_gradientn(colours=wes_palette(n=3, name="BottleRocket2"))

poke.km.pca <- KMeans_rcpp(poke.pca.2$scores[, 1:2], clusters=4, num_init=30, max_iters = 10000) 
ggplot(as.data.frame(poke.pca.2$scores[, 1:2])) + geom_point(aes(x = Comp.1, y = Comp.2, colour = poke.km.pca$clusters)) + scale_colour_gradientn(colours=wes_palette(n=3, name="BottleRocket2"))


x1 <- ggplot(as.data.frame(poke.mds.1.center)) + geom_point(aes(x = V1, y = V2, colour = poke.km$clusters)) + scale_colour_gradientn(colours=wes_palette(n=3, name="BottleRocket2"))
x2 <- ggplot(as.data.frame(poke.pca.2$scores[, 1:2])) + geom_point(aes(x = Comp.1, y = Comp.2, colour = poke.km.pca$clusters)) + scale_colour_gradientn(colours=wes_palette(n=3, name="BottleRocket2"))
grid.arrange(x1, x2, ncol=2)

# From Factoextra
poke.km.2 <- eclust(as.data.frame(poke.mds.1), "kmeans", k = 4)
fviz_silhouette(poke.km.2)

poke.km.2 <- eclust(as.data.frame(poke.pca.2$scores[, 1:2]), "kmeans", k = 4)
fviz_silhouette(poke.km.2)


# Praktycznie identyczne
poke.pam <- eclust(as.data.frame(poke.mds.1), "pam", k = 4)
fviz_silhouette(poke.pam)

poke.pam <- eclust(as.data.frame(poke.pca.2$scores[, 1:2]), "pam", k = 4)
fviz_silhouette(poke.pam)

### Wnioski
#
# W danych są dwa clustry - przed i po ewolucją
#
# Wychodzi na to, że te high tier dielą się na - zwykłe (attack + hp), te z dużym def i sp.def i te z dużym speed i sp.at
#
#
#



