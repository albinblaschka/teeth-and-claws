
# Head ----

## Analysis of development of functional groups
## Albin Blaschka

## Metadata:
##
##


# Initialization of variables, check if needed packages are available, installing, loading  -----

# Are we using R-Studio? If yes, we can use the output pane for nice formatted output and easy export
viewer <- getOption("viewer")    

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


# Start doing stuff: Reading data file, assignin factors and splitting it according to variants ----

funcGroupsData <- read.csv('data_functionalgroups_development.csv')

# TODO: Faktoren zuweisen: Funktionale Typen
# Assigning factors:
# 1 ... open soil
# 4 ... dwarf shrubs
# 5 ... herbs
# Remark: The missing groups (2 and 3 -> mosses and lichens) are not considered because of low abundance

funcGroupsData$functionaltype <- factor(funcGroupsData$functionaltype,labels = c('open soil','dwarf shrubs','herbs'))

freq.A <- subset(funcGroupsData, variant == 'A') # Null variant
freq.B <- subset(funcGroupsData, variant == 'B') # First mown, followed by targeted pasturing
freq.C <- subset(funcGroupsData, variant == 'C') # Only targeted pasturing
freq.D <- subset(funcGroupsData, variant == 'D') # extensively pastured, browsing

# TODO: Drop unused factors?

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

# Variant A, open soil
GLM.A.soil <- glm(frequency ~ year, family = poisson, data = freq.A.soil)
coef.A.soil <- coef(GLM.A.soil)

# Variant A, dwarf shrubs
GLM.A.shrubs <- glm(frequency ~ year, family = poisson, data = freq.A.shrubs)
coef.A.shrubs <- coef(GLM.A.shrubs)

# Variant A, herbs 
GLM.A.herbs <- glm(frequency ~ year, family = poisson, data = freq.A.herbs)
coef.A.herbs <- coef(GLM.A.herbs)

# Output of GLM table: if in R-Studio as HTML in the viewer pane, otherwise as formatted text on the console ----

if (!is.null(viewer)){
    output <- stargazer(GLM.A.soil, GLM.A.shrubs, GLM.A.herbs, type="html", title = 'Development of cover, variant A (GLM)')
    filename <- 'GLM.A.html'
    tempDir <- tempfile()
    dir.create(tempDir)
    htmlFile <- file.path(tempDir, filename)
    filehandle <- file(htmlFile, 'w')  # open an output file connection
    cat(output, file = filehandle, sep = "\n")
    close(filehandle)
    viewer(htmlFile)
}else{
    stargazer(GLM.A.soil, GLM.A.shrubs, GLM.A.herbs, title = 'Development of cover, variant A (GLM)', type="text")
}

# Plotting ----

plot(frequency ~ year, freq.A.shrubs, pch=20, col = 'darkgreen', xlab = "Year",
     ylab = "Frequency [%]", ylim = c(0,100), main="", sub ="Null variant (A)",
     xaxt="n",yaxt="n")
points(frequency ~ year, freq.A.herbs, pch=18, col = 'blue') 
points(frequency ~ year, freq.A.soil, pch=17, col = 'brown')

axis(1, at=0:3, labels=c("2008", "2009", "2010", "2011"))
axis(2, at=seq(from=0,to=100,by=10), labels=seq(from=0,to=100,by=10))
curve(exp(coef.A.shrubs[1]) * exp(coef.A.shrubs[2] * x), add = TRUE, col = "darkgreen")
curve(exp(coef.A.herbs[1]) * exp(coef.A.herbs[2] * x), add = TRUE, col = "blue")
curve(exp(coef.A.soil[1]) * exp(coef.A.soil[2] * x), add = TRUE, col = "brown")


