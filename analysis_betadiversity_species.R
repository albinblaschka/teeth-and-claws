
library(vegan)
library(vegan3d)
library(MASS)
library(reshape)
library(stargazer)


dta <- read.csv2("https://raw.githubusercontent.com/albinblaschka/teeth-and-claws/master/data_betadiversity_species.csv",
				  header = TRUE, sep = ',')

dta.wide <- cast(dta, id~species, value='frequency', fun.aggregate='sum')
names(dta.wide) <- make.cepnames(names(dta.wide))
dta.wide[] <- lapply(dta.wide,function(x) replace(x, is.na(x), 0))

species.dist <- vegdist(dta.wide,'euclid', na.rm = TRUE)

groups <- substr(dta.wide[,1],1,1)
groups <- factor(groups, labels = c("A","B","C", "D"))

## Calculate multivariate dispersions
mod <- betadisper(species.dist, groups)
mod
## Perform test
t.mod <- anova(mod)
summary(t.mod)
stargazer(t.mod, type='text')
stargazer(t.mod, type='latex')

## Tukey's Honest Significant Differences
mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
cols <- c('darkgreen','brown', 'blue')
syms <- c(rep(1:4, each = 16))

#pdf('PCoA_Arten.pdf', onefile=TRUE, width = 5, height = 5, paper = 'special', pointsize = 12, useDingbats = FALSE)
modplot <- plot(mod, axes = c(1,2), main='')
#dev.off()

ordipointlabel(mod, display="sites", add=TRUE)
#identify(modplot, 'sites', cex=0.7)

ordiplot3d(mod, display='sites')
ordirgl(mod, display='sites')
