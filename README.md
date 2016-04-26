# shiny-nonlinear
**shiny-nonlinear** is a tool to test if (one or more) predictor variables in a standard regression model have a
non-linear relationship with the outcome, using natural splines.


This app currently uses the packages **shinyBS**, **stringr**, **splines**, **foreign**, **survival** and **lmtest**.

Packages that are not installed will be automatically installed from within the app, only **shiny** needs to be installed manually. This can be done using the following line of code:
```r
install.packages("shiny")
```

To download and run this app directly you can use:
```r
shiny::runGitHub("shiny-nonlinear", "NErler")
```
