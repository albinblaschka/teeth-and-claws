
# Head ----

## Title: Analysis of development of functional groups
## Author: Albin Blaschka - albin.blaschka@standortsanalyse.net

## This script is part of the analysis done for my doctoral thesis. For a full description see (in german):
## Blaschka, A., 2015. Mit Zähnen und Klauen: Erhalt und Wiederherstellung von Ökosystemleistungen
## einer alpinen Kulturlandschaft. Doctoral Thesis, Paris Lodron Universität Salzburg.
## doi:10.13140/RG.2.1.3582.6724


library(vegan)
library(MASS)
library(stargazer)

# Reading data file ----

funcGroupsData <- read.csv('data_betadiversity.csv', as.is = TRUE)
dist <- vegdist(funcGroupsData,'euclid', na.rm = TRUE)

groups <- substr(funcGroupsData[,1],1,1)
groups <- factor(groups, labels = c("A","B","C","D"))

## Calculate multivariate dispersions
mod <- betadisper(dist, groups)
mod
## Perform test
t.mod <- anova(mod)
summary(t.mod)
stargazer(t.mod, type='text')

pt <- permutest(mod, pairwise = TRUE, strata = groups)

## Tukey's Honest Significant Differences
mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes

syms <- c(rep(1:4, each = 16))
modplot <- plot(mod, axes = c(1,2), main='')

