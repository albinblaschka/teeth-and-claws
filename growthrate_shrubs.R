
library(RPostgreSQL)
library(xtable)

setwd("~/Projekte/Diss/Skriptorium/gfx")
m <- dbDriver("PostgreSQL")
con <- dbConnect(m, user="postgres", password="lisanto", dbname="agram", host="127.0.0.1")
frequenzen <- dbGetQuery(con,
      "SELECT
          frequenz_klassen.flaeche,
      	  frequenz_klassen.variante,
      	  frequenz_klassen.a_jahr as jahr,
      	  frequenz_klassen.wh,
      	  frequenz_klassen.deckungsklasse,
      	  count(frequenz_klassen.deckungsklasse) AS frequenz

      FROM vegetation.frequenz_klassen

      WHERE frequenz_klassen.deckungsklasse in (1,4,5)           

      GROUP BY
          frequenz_klassen.flaeche,
          frequenz_klassen.variante,
      	  frequenz_klassen.a_jahr,
      	  frequenz_klassen.wh,
      	  frequenz_klassen.deckungsklasse
     
      ORDER BY
      	  a_jahr,
      	  wh,
          variante;")

m <- dbDisconnect(con)

# 1 ... offener Boden
# 4 ... Zwergsträucher/verholzt
# 5 ... Gräser/Kräuter/Leguminosen
frequenzen$deckungsklasse <- factor(frequenzen$deckungsklasse,labels = c('Boden','Zwergsträucher','Gräser/Kräuter'))

dta.ross <- subset(frequenzen, flaeche=='Rossfeldsattel')
dta.knappl <- subset(frequenzen, flaeche=='Knappl')

gve.Ross <- c(1.1,0.3,0.53)
gve.knappl <- c(0.15,0.17,0.20)

gve.kum.ross <- c(1.1,2.2,2.5,3.03)
#gve.kum.ross <- c(1.1,2.3,2.8,3.2) # Besatzformel

gve.kum.knappl <- c(0.15,0.32,0.52)

## Anfügen der kumulierten GVE/ha/a
x.ross <- dta.ross$jahr
x.knappl <- dta.knappl$jahr

level <- c(2008,2009,2010,2011)

kumgve.ross <- gve.kum.ross[match(x.ross,level)]
kumgve.knappl <- gve.kum.knappl[match(x.knappl,level)]

#dta.ross$jahr <- dta.ross$jahr-2008
#dta.knappl$jahr <- dta.knappl$jahr-2009

ross.Z <- subset(dta.ross, deckungsklasse == 'Zwergsträucher')
knappl.Z <- subset(dta.knappl, deckungsklasse == 'Zwergsträucher')

ross.G <- subset(dta.ross, deckungsklasse == 'Gräser/Kräuter')
knappl.G <- subset(dta.knappl, deckungsklasse == 'Gräser/Kräuter')

knappl.2009 <- subset(knappl.Z, jahr == '2009')
knappl.2010 <- subset(knappl.Z, jahr == '2010')
knappl.2011 <- subset(knappl.Z, jahr == '2011')

knappl.G2009 <- subset(knappl.G, jahr == '2009')
knappl.G2010 <- subset(knappl.G, jahr == '2010')
knappl.G2011 <- subset(knappl.G, jahr == '2011')

k.0910 <- knappl.2010$frequenz / knappl.2009$frequenz 
k.1011 <- knappl.2011$frequenz / knappl.2010$frequenz 

kG.0910 <- knappl.G2010$frequenz / knappl.G2009$frequenz 
kG.1011 <- knappl.G2011$frequenz / knappl.G2010$frequenz 

ross.2008 <- subset(ross.Z, jahr == '2008')
ross.2009 <- subset(ross.Z, jahr == '2009')
ross.2010 <- subset(ross.Z, jahr == '2010')
ross.2011 <- subset(ross.Z, jahr == '2011')

ross.G2008 <- subset(ross.G, jahr == '2008')
ross.G2009 <- subset(ross.G, jahr == '2009')
ross.G2010 <- subset(ross.G, jahr == '2010')
ross.G2011 <- subset(ross.G, jahr == '2011')

r.0809 <- ross.2009$frequenz / ross.2008$frequenz
r.0910 <- ross.2010$frequenz / ross.2009$frequenz
r.1011 <- ross.2011$frequenz / ross.2010$frequenz 

rG.0809 <- ross.G2009$frequenz / ross.G2008$frequenz
rG.0910 <- ross.G2010$frequenz / ross.G2009$frequenz
rG.1011 <- ross.G2011$frequenz / ross.G2010$frequenz 

ross.2010 <- ross.2010[ross.2010$wh != 1 & ross.2010$variante == 'M', ]
ross.2010 <- ross.2010[ross.2010$wh != 3 & ross.2010$variante == 'M', ]

r.1011 <- ross.2011$frequenz / ross.2010$frequenz 

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

# Aufbau Dataframe Gräser -----

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

# Modellierung Zwergsträucher -----

qmod <- lm(q ~ kumgve, data=qrframe)
summary(qmod)
fitted.Z <- predict.lm(qmod, interval = "confidence")

coefs <- coef(qmod)
coefs

# Modellierung Gräser/Kräuter -----

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