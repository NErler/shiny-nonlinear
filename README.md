# shiny-nonlinear
**shiny-nonlinear** is a tool to test if (one or more) predictor variables in a
standard regression model have a non-linear relationship with the outcome,
using natural cubic splines.


This app uses the packages **shinyBS**, **stringr**, **splines**, **foreign**,
**survival** and **lmtest**, which need to be installed before the app can be used.

The packages can be installed with the following syntax:
```r
# check if necessary packages are installed and install if not
need_packages <- c("stringr", "splines", "foreign", "survival", "lmtest", "shiny", "shinyBS")

new_packages <- need_packages[!(need_packages %in% installed.packages()[, "Package"])]
if (length(new.packages) > 0) install.packages(new_packages)
```

To download and run this app directly you can then use:
```r
shiny::runGitHub("shiny-nonlinear", "NErler")
```
