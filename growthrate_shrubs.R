

# Head ----

## Title: Analysis of development of functional groups
## Author: Albin Blaschka - albin.blaschka@standortsanalyse.net

## This script is part of the analysis done for my doctoral thesis. For a full description see (in german):
## Blaschka, A., 2015. Mit Zähnen und Klauen: Erhalt und Wiederherstellung von Ökosystemleistungen
## einer alpinen Kulturlandschaft. Doctoral Thesis, Paris Lodron Universität Salzburg.
## doi:10.13140/RG.2.1.3582.6724

## Data file: data_functionalgroups_development.csv: 178 observations of 5 variables
## Definition of columns is also available via a *.csvt - file: see http://www.gdal.org/drv_csv.html for explanation

## Columns:
## variant: Factor, 4 levels "A","B","C","D"
##      A: Null variant, no impact
##      B: First mown, followed by targeted pasturing
##      C: Only targeted pasturing
##      D: Extensive pasturing, browsing
## year: int - denotes the duration of the trial (4 years); variant D was started one year later as the other variants (3 years)
## replicate: int - four replicates were used for all variants
## functionaltype: Factor, 3 levels - "open soil","dwarf shrubs", "herbs"
## frequency: cover of the functional types, measured with a quadrat (1m², divided by 10cm by 10cm): Dependent variable

# Initialization of variables, check if needed packages are available, installing, loading  -----

library(xtable)

# Reading data file ----

funcGroupsData <- read.csv('data_functionalgroups_development.csv')

# Assigning factors for funtional groups ----
# 1 ... open soil
# 4 ... dwarf shrubs
# 5 ... herbs
# Remark: The missing groups (2 and 3 -> mosses and lichens) are not considered here because of low abundance

# Some hints for naming variables: Letters B, C, D rpresent variants, LU stands for livestock unit, cum stands for cumulative
# and dwarfs stands for dwarf shrubs, meaning the functional groups, herbs is the same

funcGroupsData$functionaltype <- factor(funcGroupsData$functionaltype,labels = c('open soil','dwarf shrubs','herbs'))

# Variante A/0?? siehe Original - Skript...

dta.BC <- subset(funcGroupsData, variant== c('B','C'))
dta.D <- subset(funcGroupsData, variant=='D')
BC.dwarfs <- subset(dta.BC, functionaltype == 'dwarf shrubs')
D.dwarfs <- subset(dta.D, functionaltype == 'dwarf shrubs')
BC.herbs <- subset(dta.BC, functionaltype == 'herbs')
D.herbs <- subset(dta.D, functionaltype == 'herbs')

## Join cumulated Livestock Units (LU) to dataframe
LU.BC <- c(1.1,0.3,0.53)
LU.D <- c(0.15,0.17,0.20)
LU.cum.BC <- c(1.1,2.2,2.5,3.03)
LU.cum.D <- c(0.15,0.32,0.52)
x.BC <- dta.BC$year
x.D <- dta.D$year
level <- c(0,1,2,3)
cumLU.BC <- LU.cum.BC[match(x.BC,level)]
cumLU.D <- LU.cum.D[match(x.D,level)]

BC.dwarfs.2008 <- subset(BC.dwarfs, year == '0')
BC.dwarfs.2009 <- subset(BC.dwarfs, year == '1')
BC.dwarfs.2010 <- subset(BC.dwarfs, year == '2')
BC.dwarfs.2011 <- subset(BC.dwarfs, year == '3')

BC.herbs.2008 <- subset(BC.herbs, year == '0')
BC.herbs.2009 <- subset(BC.herbs, year == '1')
BC.herbs.2010 <- subset(BC.herbs, year == '2')
BC.herbs.2011 <- subset(BC.herbs, year == '3')

D.dwarfs.2009 <- subset(D.dwarfs, year == '1') # meaning 2009
D.dwarfs.2010 <- subset(D.dwarfs, year == '2') # meaning 2010
D.dwarfs.2011 <- subset(D.dwarfs, year == '3') # meaning 2011

D.herbs.2009 <- subset(D.herbs, year == '1') # meaning 2009
D.herbs.2010 <- subset(D.herbs, year == '2') # meaning 2010
D.herbs.2011 <- subset(D.herbs, year == '3') # meaning 2011

BC.dwarfs.2010 <- BC.dwarfs.2010[BC.dwarfs.2010$replicate != 1 & BC.dwarfs.2010$variant == 'B', ]
BC.dwarfs.2010 <- BC.dwarfs.2010[BC.dwarfs.2010$replicate != 3 & BC.dwarfs.2010$variant == 'B', ]

BC.dwarf.rate.0809 <- BC.dwarfs.2009$frequency / BC.dwarfs.2008$frequency
BC.dwarf.rate.0910 <- BC.dwarfs.2010$frequency / BC.dwarfs.2009$frequency
BC.dwarf.rate.1011 <- BC.dwarfs.2011$frequency / BC.dwarfs.2010$frequency

BC.herbs.rate.0809 <- BC.herbs.2009$frequency / BC.herbs.2008$frequency
BC.herbs.rate.0910 <- BC.herbs.2010$frequency / BC.herbs.2009$frequency
BC.herbs.rate.1011 <- BC.herbs.2011$frequency / BC.herbs.2010$frequency

# ------------------------------------

k.0910 <- knappl.2010$frequenz / knappl.2009$frequenz 
k.1011 <- knappl.2011$frequenz / knappl.2010$frequenz 

kG.0910 <- knappl.G2010$frequenz / knappl.G2009$frequenz 
kG.1011 <- knappl.G2011$frequenz / knappl.G2010$frequenz 



qrframe <- data.frame(q = r.0809, variante = ross.2009$variante, wh = ross.2009$wh, jahr=rep(2009,12), kumgve = rep(c(0,1.1,1.1),4))
qrframe.tmp <- data.frame(q = r.0910, variante = ross.2010$variante, wh = ross.2010$wh, jahr=rep(2010,12), kumgve = rep(c(0,2.2,2.2),4))
qrframe <- rbind(qrframe,qrframe.tmp)
qrframe.tmp <- data.frame(q = r.1011, variante = ross.2011$variante, wh = ross.2011$wh, jahr=rep(2010,10), kumgve = c(0,2.5,0,2.5,2.5,0,2.5,0,2.5,2.5))

qrframe.tmp <- data.frame(q = k.0910, variante = knappl.2010$variante, wh = knappl.2010$wh, jahr=rep(2010,4), kumgve = rep(0.15,4))
qrframe <- rbind(qrframe,qrframe.tmp)
qrframe.tmp <- data.frame(q = k.1011, variante = knappl.2011$variante, wh = knappl.2011$wh, jahr=rep(2011,4), kumgve = rep(0.32,4))
qrframe <- rbind(qrframe,qrframe.tmp)

q0frame <- qrframe[qrframe$variante == 0,]
qrframe <- qrframe[qrframe$variante != 0,]
qrframe

qrframe.tmp <- NULL

# Compiling dataframe herbs -----

qrframe.G <- data.frame(q = rG.0809, variante = ross.2009$variante, wh = ross.2009$wh, jahr=rep(2009,12), kumgve = rep(c(0,1.1,1.1),4))
qrframe.tmp <- data.frame(q = rG.0910, variante = ross.2010$variante, wh = ross.2010$wh, jahr=rep(2010,12), kumgve = rep(c(0,2.2,2.2),4))
qrframe.G <- rbind(qrframe.G,qrframe.tmp)
qrframe.tmp <- data.frame(q = rG.1011, variante = ross.2011$variante, wh = ross.2011$wh, jahr=rep(2010,10), kumgve = c(0,2.5,0,2.5,2.5,0,2.5,0,2.5,2.5))

qrframe.tmp <- data.frame(q = kG.0910, variante = knappl.2010$variante, wh = knappl.2010$wh, jahr=rep(2010,4), kumgve = rep(0.15,4))
qrframe.G <- rbind(qrframe.G,qrframe.tmp)
qrframe.tmp <- data.frame(q = kG.1011, variante = knappl.2011$variante, wh = knappl.2011$wh, jahr=rep(2011,4), kumgve = rep(0.32,4))
qrframe.G <- rbind(qrframe.G,qrframe.tmp)

q0frame.G <- qrframe.G[qrframe.G$variante == 0,]
qrframe.G <- qrframe.G[qrframe.G$variante != 0,]
qrframe.G

qrframe.tmp <- NULL

# Model for dwarf shrubs -----

qmod <- lm(q ~ kumgve, data=qrframe)
summary(qmod)
fitted.Z <- predict.lm(qmod, interval = "confidence")

coefs <- coef(qmod)
coefs

# Model for herbs -----

qmod.G <- lm(q ~ kumgve, data=qrframe.G)
summary(qmod.G)
fitted.G <- predict.lm(qmod.G, interval = "confidence")

coefs.G <- coef(qmod.G)
coefs.G

# Plot -----

#pdf(file = 'regression_ZwergQuot.pdf', width=7, onefile=TRUE, paper='special',pointsize = 14, useDingbats = FALSE)
plot(q ~ kumgve, data=qrframe, ylim=c(0,2), xlim=c(0,2.8),
     ylab="Änderungsrate", xlab=expression(kumulierte~GVE/ha%*%a))

u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = "grey85", border = "grey30")
abline(h=seq(from=0,to=2,by=0.5), col='white')
abline(v=seq(from=0,to=3,by=0.5), col='white')
abline(h=1,col="brown")
abline(qmod)
abline(qmod.G)
points(q ~ kumgve, data=qrframe, pch=16, col="darkgreen")
points(q ~ kumgve, data=qrframe.G, pch=16, col="orange")

# Zwergsträucher Quotient = 1 bei -----
q1 <- (1-coefs[1])/coefs[2]
abline(v=q1)

## 0,75 GVE/ha*a im Durchschnitt Rossfeldsattel (Annahme)
quot.r <- coefs[2] * 0.75 + coefs[1]

#
text(x=q1,y=1.95, paste('Nullwachstum bei',round(q1, digits=2)), cex=0.8, pos=4)
text(x=1.06,y=1.95, expression(frac(GVE, ha%*%a)), cex=0.8, pos=4)
text(x=1.8, y=1.95, paste('y = ',round(coefs[2],4),'x + ',round(coefs[1],4)), cex=0.8, pos=4)


#dev.off()
#plot(qmod)
#predict.lm(qmod)

40 * exp(-1.1*4)
