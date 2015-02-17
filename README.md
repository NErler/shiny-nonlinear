# shiny-nonlinear
**shiny-nonlinear** is a tool to test if a predictor variable in a standard regression model should be modeled as a non-linear effect or if the relationship between that variable and the outcome is linear.

### This is a preliminary version and not yet complete!!!

This app currently uses the packages *shinyBS*, *stringr*, *splines*, *foreign*, *survival* and *lmtest*.
Packages that are not installed will be automatically installed from within the app.

For compatibility, shiny version 0.10.2.1 and shinyBS version 0.25 are needed. Shiny can be installed by using
```r
devtools::install_version("shiny", version = "0.10.2.1")
```

