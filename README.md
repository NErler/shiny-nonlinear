# shiny-nonlinear
**shiny-nonlinear** is a tool to test if (one or more) predictor variables in a standard regression model have a
non-linear relationship with the outcome, using natural splines.


This app currently uses the packages *shinyBS*, *stringr*, *splines*, *foreign*, *survival* and *lmtest*.

Packages that are not installed will be automatically installed from within the app.

For this application to run without error, new versions of the packages *shiny* and *shinyBS* are needed. They can be
downloaded from GitHub using the following code:

```r
if (!require("devtools"))
  install.packages("devtools")

devtools::install_github("rstudio/shiny")
devtools::install_github("ebailey78/shinyBS", ref = "shinyBS3")
```


To download and run this app directly you can use:
```r
shiny::runGitHub("shiny-nonlinear", "NErler", "residuals")
```
