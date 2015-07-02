
# Head ----

## Title: Analysis of development of functional groups
## Author: Albin Blaschka - albin.blaschka@standortsanalyse.net

## This script is part of the analysis done for my doctoral thesis. For a full description see (in german):
## Blaschka, A., 2015. Mit Zähnen und Klauen: Erhalt und Wiederherstellung von Ökosystemleistungen
## einer alpinen Kulturlandschaft. Doctoral Thesis, Paris Lodron Universität Salzburg.
## doi:10.13140/RG.2.1.3582.6724

## To study the changes in vegetation and restoration of pastures already infested with shrubs by targeted
## pasturing, a trial following a factorial design was set up, with four replicates. The development of
## three functional groups (dwarf shrubs, herbs and open soil) is analysed with a GLM.

## The script takes advantage of features of the IDE R-Studio (http://www.rstudio.com/products/RStudio),
## namely of the different panes, but is fully functional on the plain vanilla R-Console
## It was developed under R version 3.2.0 (2015-04-16) -- "Full of Ingredients"
## It uses the stargazer package, Version 5.1 (see below)

## Metadata:
## See also the repository on GitHub: https://github.com/albinblaschka/teeth-and-claws,
## specially the file metadata.txt

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

# Are we using R-Studio? If yes, we can use the output pane for nice formatted output and easy export
viewer <- getOption("viewer")    

# A place for tempory data
tempDir <- tempfile()
dir.create(tempDir)


# Using the package stargazer for nice formatting of output
# Hlavac, Marek (2014). stargazer: LaTeX/HTML code and ASCII text for well-formatted regression
#         and summary statistics tables. R package version 5.1. http://CRAN.R-project.org/package=stargazer

chkPackage <- require('stargazer', quietly = TRUE)
if (chkPackage == FALSE) {
    install.packages('stargazer', dependencies = TRUE)
    checkAgain <- require('stargazer', quietly = TRUE)
}else{
    checkAgain <- TRUE
}
if (checkAgain == FALSE) {
    stop('Package Stargazer could not be loaded/installed! Aborting...')
}

# Functions ----

# Calculation of a Pseudo R² following
# Zuur, A.F., Hilbe, J.M., Ieno, E.N., 2013. A Beginner’s Guide to GLM and GLMM with R. A frequentist and
# Bayesian perspective for ecologists. Highland Statistics Ltd., Newburgh, United Kingdom, Page 13

glmR2 <- function(glmSum) {
    nullDeviance <- glmSum$null.deviance
    resDeviance <- glmSum$deviance
    r2 <- 100 * ((nullDeviance - resDeviance)/nullDeviance)
    r2 <- round(r2, digits = 0)
}

ci.lines <- function(model, predfor, lcol) {
    
    zhat <- predict(model, se.fit = TRUE)       # result on logit or log scale
    zupper <- zhat$fit + 1.96 * zhat$se.fit
    zlower <- zhat$fit - 1.96 * zhat$se.fit
    
    yupper <- unique(exp(zupper))                   # for log link
    ylower <- unique(exp(zlower))
    
    lines(predfor[order(predfor)], yupper[order(predfor)], lty = 2, col = lcol)
    lines(predfor[order(predfor)], ylower[order(predfor)], lty = 2, col = lcol)
    
}


# Plotting the Model
plotGLM <- function(model.soil,model.shrubs, model.herbs, period, cap) {
    plot(frequency ~ year, as.data.frame(model.soil$model), pch=20, col = 'brown', xlab = "Year",
         ylab = "Frequency [%]", ylim = c(0,100), main="", sub = cap,
         xaxt="n",yaxt="n")
    points(frequency ~ year, as.data.frame(model.shrubs$model), pch=18, col = 'darkgreen') 
    points(frequency ~ year, as.data.frame(model.herbs$model), pch=17, col = 'blue')
    endpos <- length(period)-1
    axis(1, at=0:endpos, labels=period)
    axis(2, at=seq(from=0,to=100,by=10), labels=seq(from=0,to=100,by=10))
    curve(exp(coef(model.soil)[1]) * exp(coef(model.soil)[2] * x), add = TRUE, col = "brown")
    curve(exp(coef(model.shrubs)[1]) * exp(coef(model.shrubs)[2] * x), add = TRUE, col = "darkgreen")
    curve(exp(coef(model.herbs)[1]) * exp(coef(model.herbs)[2] * x), add = TRUE, col = "blue")
}


# Reading data file ----

funcGroupsData <- read.csv('data_functionalgroups_development.csv')

# Assigning factors for funtional groups ----
# 1 ... open soil
# 4 ... dwarf shrubs
# 5 ... herbs
# Remark: The missing groups (2 and 3 -> mosses and lichens) are not considered here because of low abundance

funcGroupsData$functionaltype <- factor(funcGroupsData$functionaltype,labels = c('open soil','dwarf shrubs','herbs'))


# Splitting data following variants ----

freq.A <- subset(funcGroupsData, variant == 'A') # Null variant
freq.B <- subset(funcGroupsData, variant == 'B') # First mown, followed by targeted pasturing
freq.C <- subset(funcGroupsData, variant == 'C') # Only targeted pasturing
freq.D <- subset(funcGroupsData, variant == 'D') # extensively pastured, browsing


# Splitting datasets following the functional groups ----

freq.A.soil <- subset(freq.A, functionaltype == 'open soil')
freq.A.shrubs <- subset(freq.A, functionaltype == 'dwarf shrubs')
freq.A.herbs <- subset(freq.A, functionaltype == 'herbs')

freq.B.soil <- subset(freq.B, functionaltype == 'open soil')
freq.B.shrubs <- subset(freq.B, functionaltype == 'dwarf shrubs')
freq.B.herbs <- subset(freq.B, functionaltype == 'herbs')

freq.C.soil <- subset(freq.C, functionaltype == 'open soil')
freq.C.shrubs <- subset(freq.C, functionaltype == 'dwarf shrubs')
freq.C.herbs <- subset(freq.C, functionaltype == 'herbs')

freq.D.soil <- subset(freq.D, functionaltype == 'open soil')
freq.D.shrubs <- subset(freq.D, functionaltype == 'dwarf shrubs')
freq.D.herbs <- subset(freq.D, functionaltype == 'herbs')


# Doing the Analysis: GLM ----

# Variant A, Null ----
# Variant A, open soil
GLM.A.soil <- glm(frequency ~ year, family = poisson, data = freq.A.soil)
pseudoR2.A.soil <- glmR2(GLM.A.soil)

# Variant A, dwarf shrubs
GLM.A.shrubs <- glm(frequency ~ year, family = poisson, data = freq.A.shrubs)
pseudoR2.A.shrubs <- glmR2(GLM.A.shrubs)

# Variant A, herbs 
GLM.A.herbs <- glm(frequency ~ year, family = poisson, data = freq.A.herbs)
pseudoR2.A.herbs <- glmR2(GLM.A.herbs)

# Variant B, Mown, targeted pasturing ----
# Variant B, open soil
GLM.B.soil <- glm(frequency ~ year, family = poisson, data = freq.B.soil)
pseudoR2.B.soil <- glmR2(GLM.B.soil)

# Variant B, dwarf shrubs
GLM.B.shrubs <- glm(frequency ~ year, family = poisson, data = freq.B.shrubs)
pseudoR2.B.shrubs <- glmR2(GLM.B.shrubs)

# Variant B, herbs 
GLM.B.herbs <- glm(frequency ~ year, family = poisson, data = freq.B.herbs)
pseudoR2.B.herbs <- glmR2(GLM.B.herbs)

# Variant C, targeted pasturing ----
# Variant C, open soil
GLM.C.soil <- glm(frequency ~ year, family = poisson, data = freq.C.soil)
pseudoR2.C.soil <- glmR2(GLM.C.soil)

# Variant C, dwarf shrubs
GLM.C.shrubs <- glm(frequency ~ year, family = poisson, data = freq.C.shrubs)
pseudoR2.C.shrubs <- glmR2(GLM.C.shrubs)

# Variant C, herbs 
GLM.C.herbs <- glm(frequency ~ year, family = poisson, data = freq.C.herbs)
pseudoR2.C.herbs <- glmR2(GLM.C.herbs)

# Variant D, browsing, extensive pasturing ----
# Variant D, open soil
GLM.D.soil <- glm(frequency ~ year, family = poisson, data = freq.D.soil)
pseudoR2.D.soil <- glmR2(GLM.D.soil)

# Variant D, dwarf shrubs
GLM.D.shrubs <- glm(frequency ~ year, family = poisson, data = freq.D.shrubs)
pseudoR2.D.shrubs <- glmR2(GLM.D.shrubs)

# Variant D, herbs 
GLM.D.herbs <- glm(frequency ~ year, family = poisson, data = freq.D.herbs)
pseudoR2.D.herbs <- glmR2(GLM.D.herbs)


# Output of GLM table ----
# if in R-Studio as HTML in the viewer pane, otherwise as formatted text on the console

r2.A <- list(c('Pseudo R<sup>2</sup>',pseudoR2.A.soil,pseudoR2.A.shrubs,pseudoR2.A.herbs))
r2.B <- list(c('Pseudo R<sup>2</sup>',pseudoR2.B.soil,pseudoR2.B.shrubs,pseudoR2.B.herbs))
r2.C <- list(c('Pseudo R<sup>2</sup>',pseudoR2.C.soil,pseudoR2.C.shrubs,pseudoR2.C.herbs))
r2.D <- list(c('Pseudo R<sup>2</sup>',pseudoR2.D.soil,pseudoR2.D.shrubs,pseudoR2.D.herbs))

if (!is.null(viewer)){
    filename.A <- 'GLM.A.html'
    filename.B <- 'GLM.B.html'
    filename.C <- 'GLM.C.html'
    filename.D <- 'GLM.D.html'
    htmlFile.A <- file.path(tempDir, filename.A)
    htmlFile.B <- file.path(tempDir, filename.B)
    htmlFile.C <- file.path(tempDir, filename.C)
    htmlFile.D <- file.path(tempDir, filename.D)
    
    stargazer(GLM.A.soil, GLM.A.shrubs, GLM.A.herbs, type="html", title = 'Development of cover, variant A (GLM)',
                        add.lines = r2.A, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'),
                        out = htmlFile.A)
    stargazer(GLM.B.soil, GLM.B.shrubs, GLM.B.herbs, type="html", title = 'Development of cover, variant B (GLM)',
                        add.lines = r2.B, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'),
                        out = htmlFile.B)
    stargazer(GLM.C.soil, GLM.C.shrubs, GLM.C.herbs, type="html", title = 'Development of cover, variant C (GLM)',
                        add.lines = r2.C, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'),
                        out = htmlFile.C)
    stargazer(GLM.D.soil, GLM.D.shrubs, GLM.D.herbs, type="html", title = 'Development of cover, variant D (GLM)',
                        add.lines = r2.D, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'),
                        out = htmlFile.D)
    viewer(htmlFile.A)
    viewer(htmlFile.B)
    viewer(htmlFile.C)
    viewer(htmlFile.D)
}else{
    stargazer(GLM.A.soil, GLM.A.shrubs, GLM.A.herbs, type="text", title = 'Development of cover, variant A (GLM)',
                        add.lines = r2.A, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'))
    stargazer(GLM.B.soil, GLM.B.shrubs, GLM.B.herbs, type="text", title = 'Development of cover, variant B (GLM)',
                        add.lines = r2.B, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'))
    stargazer(GLM.C.soil, GLM.C.shrubs, GLM.C.herbs, type="text", title = 'Development of cover, variant C (GLM)',
                        add.lines = r2.C, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'))
    stargazer(GLM.D.soil, GLM.D.shrubs, GLM.D.herbs, type="text", title = 'Development of cover, variant D (GLM)',
                        add.lines = r2.D, column.labels = c('Open Soil','Dwarf Shrubs','Herbs'), omit.stat = c('aic'))
}


# Plotting ----

# It makes more sense to put variant D to variant A, for an easier comparision...
par(mfrow=c(2,2))
plotGLM(GLM.A.soil, GLM.A.shrubs, GLM.A.herbs, c(2008,2009,2010,2011), 'Null variant (A)')
ci.lines(GLM.A.shrubs, seq(0,3,1), 'lightgreen')
ci.lines(GLM.A.herbs, seq(0,3,1), 'dodgerblue')
plotGLM(GLM.D.soil, GLM.D.shrubs, GLM.D.herbs, c(2008,2009,2010,2011), 'Browsing (D)')
# Variant D was startet a year later!
ci.lines(GLM.D.shrubs, seq(1,3,1), 'lightgreen')
ci.lines(GLM.D.herbs, seq(1,3,1), 'dodgerblue')
plotGLM(GLM.B.soil, GLM.B.shrubs, GLM.B.herbs, c(2008,2009,2010,2011), 'Mown, targeted pasturing (B)')
ci.lines(GLM.B.shrubs, seq(0,3,1), 'lightgreen')
ci.lines(GLM.B.herbs, seq(0,3,1), 'dodgerblue')
plotGLM(GLM.C.soil, GLM.C.shrubs, GLM.C.herbs, c(2008,2009,2010,2011), 'Targeted pasturing (C)')
ci.lines(GLM.C.shrubs, seq(0,3,1), 'lightgreen')
ci.lines(GLM.C.herbs, seq(0,3,1), 'dodgerblue')
