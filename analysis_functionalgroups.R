
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


# Start doing stuff ----
