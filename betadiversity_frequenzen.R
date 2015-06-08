library(RPostgreSQL)
library(vegan)
library(MASS)
library(reshape)
library(stargazer)

setwd("~/Projekte/Diss/Skriptorium/gfx")
m <- dbDriver("PostgreSQL")
con <- dbConnect(m, user="postgres", password="lisanto", dbname="agram", host="192.168.0.105")
frequenzen <- dbGetQuery(con,"SELECT kennung, 
				      CASE WHEN frequenz_deckungsklassen.deckungsklasse = 1 then 'Boden'
					  WHEN frequenz_deckungsklassen.deckungsklasse = 2 then 'Flechten'
					  WHEN frequenz_deckungsklassen.deckungsklasse = 3 then 'Moose'
					  WHEN frequenz_deckungsklassen.deckungsklasse = 4 then 'Zwergsträucher'
					  WHEN frequenz_deckungsklassen.deckungsklasse = 5 then 'Kräuter' END
					as klasse,
					   frequenz
				FROM vegetation.frequenz_deckungsklassen;")
m <- dbDisconnect(con)

f.wide <- cast(frequenzen, kennung~klasse, value='frequenz')

f.wide[] <- lapply(f.wide,function(x) replace(x, is.na(x), 0))

# write.csv(f.wide,'')

f.dist <- vegdist(f.wide,'euclid', na.rm = TRUE)

groups <- substr(f.wide[,1],1,1)
groups <- factor(groups, labels = c("B","0","M", "S"))

## Calculate multivariate dispersions
mod <- betadisper(f.dist, groups)
mod
## Perform test
t.mod <- anova(mod)
summary(t.mod)
stargazer(t.mod, type='text')

pt <- permutest(mod, pairwise = TRUE, strata = groups)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
cols <- c('darkgreen','brown', 'blue')
syms <- c(rep(1:4, each = 16))

#pdf('PCoA_Frequenzen.pdf', onefile=TRUE, width = 5, height = 5, paper = 'special', pointsize = 12, useDingbats = FALSE)
modplot <- plot(mod, axes = c(1,2), main='')
#dev.off()

ordipointlabel(mod, display="sites", add=TRUE)
#identify(modplot, 'sites', cex=0.7)
#ordirgl(mod, display='sites')

