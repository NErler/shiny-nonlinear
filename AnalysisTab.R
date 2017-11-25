#++++++++++++++++++++++++++#
# Analysis Tab Server File #
#++++++++++++++++++++++++++#

source("functions.R")
# load Data-------------------------------------------------------------------------------------------------------------
OData <- reactive({
  ds <- input$askData
  if (is.null(ds))
    return(NULL)

  if (input$inputType == "sav") {
    library(foreign)
    Data <- read.spss(file = ds$datapath, to.data.frame = T)
  }

  if (input$inputType == "csv") {
    Data <- read.csv(file = ds$datapath, sep = input$fieldsep,
                     dec = switch(input$decsep, "dot" = ".", "comma" = ","),
                     stringsAsFactors = T )
  }
  Data
})


# Update the data input options ----------------------------------------------------------------------------------------
VNames <- reactive({
  if (is.null(OData())) {return(NULL)}
  VNames <- as.list(colnames(OData()))
  names(VNames) = VNames
  VNames
})

# Update options to select the response variable
observe({
  updateSelectInput(session, inputId = "outcome",
                    choices = c(Choose = "", VNames()))
})

# Update options to select time and event variables for Cox-model
observe({
  updateSelectInput(session, inputId = "CoxTime",
                    choices = c(Choose = "", VNames()))
})
observe({
  updateSelectInput(session, inputId = "CoxEvent",
                    choices = c(Choose = "", VNames()))
})

# Update options to select the (linear) covariates
observe({
  updateSelectizeInput(session, inputId = "covars",
                       choices = VNames())
})

# vector indicating which variables are coded as numeric and are not in the covariates vector
nonlincand <- reactive({
  numcand <- unlist(lapply(OData(), is.numeric))
  numcand
})

# Update options to select non-linear covariates
observe({
  updateSelectizeInput(session, inputId = "nonlin",
                       choices = VNames()[nonlincand()])
})



# Create sliders to select the df for each non-linear effect -----------------------------------------------------------
output$sliders <- renderUI({
  numSl <- length(input$nonlin)
  if (numSl > 0) {
    fluidRow(
      column(6,
             lapply(1:ceiling(numSl/2), function(i) {
               sliderInput(paste("dfSlider", i, sep = ""),
                           paste("df for", input$nonlin[i]),
                           min = 1, max = 4, value = 1)
             })
      ),
      column(6,
             if (numSl > 1) {
               lapply(min(numSl, ceiling(numSl/2) + 1):numSl, function(i) {
                 sliderInput(paste("dfSlider", i, sep = ""),
                             paste("df for", input$nonlin[i]),
                             min = 1, max = 4, value = 1)
               })
             }
             )
      )
  } else {
    "No non-linear effect(s) selected."
  }
})

# built non-linear predictor -------------------------------------------------------------------------------------------

# get df from the sliders
dfList <- reactive({
  inpList <- reactiveValuesToList(input)
  inpList[grep("dfSlider", names(inpList))]
})

# built non-linear part of the predictor
nonlinpred <- reactive({
  if (length(input$nonlin) < 1) return(NULL)
  bkn <- lapply(lapply(OData()[,input$nonlin, drop = F], quantile,
                       c(0.05, 0.95), na.rm = T), round, 2)
  bkn <- unlist(lapply(bkn, paste, collapse = ", "))
  paste("ns(", input$nonlin,", df = ", unlist(dfList()),
        ", Boundary.knots = c(", bkn, "))", sep = "")
})


# model formula --------------------------------------------------------------------------------------------------------
fmla <- reactive({
  if(!any(input$outcome !="", (input$CoxEvent !="" & input$CoxTime != "")))return(NULL)

  # if no covariates are selected (linear or non-linear), set only an intercept
  pred <- if(length(nonlinpred()) + length(input$covars)<1){
    1
    }else{
      # paste linear and non-linear part of the predictor
      paste(c(input$covars, nonlinpred()), collapse = " + ")
    }

  # combine left and right part of the model formula
  if(input$modType == "cox"){
    paste("Surv(time=", input$CoxTime, ", event = as.numeric(", input$CoxEvent, ")) ~ ", pred, sep="")
  }else{
    paste(input$outcome, "~", pred)
  }
})


# Dataset with selected variables---------------------------------------------------------------------------------------
Data <- reactive({
  D <- get_all_vars(as.formula(fmla()), OData())
  D <- D[complete.cases(D), , drop = F]
  D
})


# original model -------------------------------------------------------------------------------------------------------
fam <- reactive({
  switch(input$modType,
         "lin" = "gaussian",
         "log" = binomial(link = 'logit'),
         "poi" = poisson(link = 'log'),
         "cox" = "")
})



model <- reactive({
  if (is.null(validModel()))
    return(NULL)
  if (any(is.null(fmla()), !validModel())) {
    return(NULL)
  } else {
    if (input$outcome %in% c(input$covars, input$nonlin))
      return(NULL)
    if (input$modType %in% c("lin", "log", "poi")) {
      glm(formula = fmla(), family = fam(), data = Data())
    } else {
      if (input$modType == "cox") {
        coxph(as.formula(fmla()), data = Data(), x = T)
      }
    }
  }
})



output$modsummary <- renderPrint({
  if (!is.null(model()))
    summary(model())
})


# comparison model -----------------------------------------------------------------------------------------------------
# model formula for comparison model
subform <- function(form, nlin) {
  if (!is.character(form)) {
    form <- paste(as.character(form)[c(2, 1, 3)], collapse = " ")
  }
  splits <- strsplit(strsplit(form, split = " ~ ", fixed = T)[[1]][2], split = " + ", fixed = T)[[1]]
  nlinpos <- grep(pattern = paste("ns(", nlin, sep = ""), x = splits, fixed = T)
  splits[nlinpos] <- paste("ns(", nlin, ", df=1)", sep = "")
  form.new <- update(as.formula(form), as.formula(paste(".~", paste(splits, collapse = " + "))))
  return(form.new)
}

# comparison model
model2 <- reactive({
  if (!length(input$nonlin) > 0)
    return(NULL)
  useform <- model()$formula
  lapply(1:length(input$nonlin), function(i) {
    update(model(), formula = subform(useform, input$nonlin[i]))
  })
})


# predict non-linear fit & CIs -----------------------------------------------------------------------------------------
fitCI <- reactive({
  if (length(input$nonlin) < 1)
    return(NULL)
  if (is.null(model()))
    return(NULL)
  type <- if (input$modType == "cox") {
    "lp"
  } else {
    "link"
  }
  get.fitCI(input$nonlin, model(), Data(), fmla(), type = type)
})


# plot -----------------------------------------------------------------------------------------------------------------
# dynamic height of the plot, depending on number of rows of plots
myheight <- function() {
  if (is.null(input$nonlin)) {
    return(100)
  } else {
    return(length(input$nonlin)*300)
  }
}


# trans <- reactive({
# #   if(input$modType %in% c("lin", "log", "poi")){
# #     family(model())$linkinv
# #   }else{
#     function(eta)
#       eta
# #   }
# })


observe(
  for (i in 1:length(input$nonlin)){
    local({
      myi <- i
      plotname <- paste("plot", myi, sep = "")
      output[[plotname]] <- renderPlot(.myplot(), height = 300, bg = "transparent")

      type <- if (input$modType == "cox") {
        "lp"
      } else {
        "link"
      }


      .myplot <- reactive(plotfunc(k = myi, predDF = fitCI(),
                                   nonlin = input$nonlin, model = model(),
                                   Dat = Data(), modType = input$modType,
                                   plotResid = input$plotResid,
                                   plotKnots = input$plotKnots,
                                   quants = dfList(), type = type))
    })
  }
)



# create panel to check for overfitting --------------------------------------------------------------------------------
output$overfit <- renderUI({
  wellPanel(
    if (is.null(input$nonlin))
      return("No non-linear effects have been included in the model."),
    if (is.null(model()))
      return(NULL),
    HTML(paste(
    "<table>
      <tr>
      <td><b> Number of observations:</b></td>
      <td style='text-align:right'> N=", nobs(model(), use.fallback = T), "</td>
      </tr>",
    if (input$modType == 'cox'){
      paste("<tr>
             <td><b> Number of events:</b></td>
             <td style='text-align:right'>", model()$nevent, "</td>
             </tr>")
    },"
      <tr>
      <td><b> Max. number of coefficients to avoid overfitting:</b></td>
      <td style='text-align:right'>",
    if (input$modType == 'lin'){floor(nobs(model())/10)},
    if (input$modType == "log"){floor(min(table(Data()[,input$outcome]))/10)},
    if (input$modType == "poi"){"???"},
    if (input$modType == "cox"){floor(model()$nevent/10)},"
      </td>
      </tr>
      <tr>
      <td><b> Number of coefficients:</b></td>
      <td style='text-align:right'>", length(model()$coef), "</td>
      </tr>
    <table>
    "
  , sep="")))
})


output$checkbox1 <- renderUI({
  if(all(length(input$nonlin) > 0, any(input$outcome != "", (input$CoxTime != "" & input$CoxEvent != "")))){
    checkboxInput("plotKnots", "display location of knots", value=FALSE)
  }
})

output$checkbox2 <- renderUI({
  if(all(length(input$nonlin) > 0, any(input$outcome != "", (input$CoxTime != "" & input$CoxEvent != "")), input$modType=="lin")){
    checkboxInput("plotResid", "display partial residuals", value=TRUE)
  }
})

# LR test results ------------------------------------------------------------------------------------------------------
output$table <- renderUI({
  if (length(input$nonlin) < 1)
    return(NULL)
  if (is.null(model()))
    return(NULL)

  lapply(1:length(input$nonlin), function(i) {
    lr <- lrtest(model2()[[i]], model())
    sigcol <- c(AGEgreen, "red")[as.numeric(lr$"Pr(>Chisq)"[2] > 0.05) + 1]
    wellPanel(fluidRow(
      column(6,
             HTML(paste(
               "<style> th {background-color: #C6E2FF;
                            text-align:left;
                            font-family: Arial;
                            padding: 3px 7px 2px 7px;
                            font-size: 1.3em;
                           }
                        table {font-family: 'Trebuchet MS', Arial, Helvetica, sans-serif;
                               font-size: 1.1em;
                              }
                        td {padding: 5px;}
                </style>",
               "<table>
                  <colgroup>
                    <col span='1' width='20%'>
                    <col span='1' width='20%'>
                    <col width='60%'>
                  </colgroup>
                  <th colspan = 3> <b>LR-Test:</b> linear vs. non-linear effect of <span style='color:", AGEblue, "'> ",
                                   input$nonlin[i], " </span></th>
                  <tr>
                    <td><b> DF: </b></td>
                    <td> ", lr$Df[2]," </td>
                    <td rowspan='3' frame='vsides'>",
                        if(lr$"Pr(>Chisq)"[2]<0.05){
                          paste("<b>Conclusion: </b> <br> The Null-Hypothesis can be rejected.",
                                "<span style='color:",AGEblue,"'>", input$nonlin[i], "</span> seems to have a",
                                "<span style='color:",AGEgreen,"'><b> non-linear effect. </b></span>",
                                "<div class='contact-msg'> For further analysis please contact a statistician. </div>"
                                )
                        }else{
                          paste("<b>Conclusion: </b> <br> The Null-Hypothesis can not be rejected.
                                There is no evidence against a <span style='color:red'><b> linear effect </b></span> of",
                                "<span style='color:",AGEblue,"'>", input$nonlin[i], ".</span>")
                        },"
                    </td>
                  </tr>
                  <tr>
                    <td><b> Chi<sup>2</sup>-Statistic: </b></td>
                    <td> ", round(lr$Chisq,2)[2], " </td>
                  </tr>
                  <tr>
                    <td><b> p-value: </b></td>
                    <td><b> <span style='color:", sigcol, "'>", format(lr$'Pr(>Chisq)'[2],digits=4), "</span></b> </td>
                  </tr>
                </table>") # end of paste
               ) # end of HTML
             ), # end of column
      column(6, plotOutput(paste("plot", i, sep=""), height='auto')
             )# end of column
      )) # end of wellPanel & fluidRow
  }) # end of lapply
}) # end of renderUI

