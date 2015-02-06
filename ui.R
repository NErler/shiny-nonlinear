library(shinyBS) # this has to be here, doesn't work if loaded from server.R

shinyUI(fluidPage(

  # style file, containing warning messages etc.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
  ),


  titlePanel(h1("Non-linear effects")),
  tabsetPanel(

    # Tab: How this works...--------------------------------------------------------------------------------------------
    tabPanel("How this works...",
             uiOutput("HelpTab")
    ),

    # Tab: Test for non-linear effects ---------------------------------------------------------------------------------
    tabPanel("Test for non-linear effects",
             fluidRow(
               column(4, # left column +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                      wellPanel(
                        h4("Dataset"),
                        fileInput("askData", label="", multiple=F),

                        h4("Model type"),
                        radioButtons("modType", "",
                                     choices = list("linear" = "lin", "logistic" = "log",
                                                    "poisson" = "poi", "Cox" = "cox"),
                                     selected = "lin"),
                        tags$style(type="text/css", HTML("#modType>*{float: left; margin-right: 15px;
                                                         height: 20px;} #modType {height: 20px;}")),

                        br(),
                        h4("Model structure"),

                        h5("Response"),
                        conditionalPanel(
                          condition = "input.modType == 'cox'",
                          column(6, selectInput(inputId="CoxTime", 'Time variable',
                                                choices = c(Choose=''), multiple=F)),
                          column(6, selectInput(inputId="CoxEvent", 'Event indicator',
                                                choices = c(Choose=''), multiple=F))
                        ),
                        fluidRow(
                          column(5,
                                 conditionalPanel(
                                   condition = "input.modType != 'cox'",
                                   selectInput(inputId="outcome", '', choices = c(Choose=""), multiple=F)
                                 )),
                          column(7,
                                 uiOutput("err.modelmisspecified")
                          )),

                        h5("Covariates"),
                        uiOutput("err.resp_covar"),
                        uiOutput("err.covar_nonlin"),
                        selectizeInput(inputId="covars", '', choices = list("Choose" = ""), multiple=T, width = '100%'),

                        h5("Nonlinear effect(s)"),
                        uiOutput("err.resp_nonlin"),
                        selectizeInput(inputId="nonlin", '', choices = list("Choose" = ""), multiple=T, width = '100%'),

                        br(),
                        h4("Splines"),
                        uiOutput("sliders")
                      ) # end of wellpanel of left column
               ), # end of left column

               column(8,  # right column +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                      fluidRow(
                        column(6,
                               h4("Model Dimension"),
                               uiOutput("overfit")
                        ),
                        column(6,
                               h4("Model Summary"),
                               bsButton(inputId="moTrig", label="Open Model Summary", value=NULL, style = "primary"),
                               bsModal("moMod", "Model Summary", trigger = "moTrig",
                                       tags$head(
                                         tags$style(HTML("
                                             .modal{
                                               width: 60%;
                                               left: 20%;
                                               margin-left:auto;
                                               margin-right:auto;
                                             }
                                           "))), # end of tags$head
                                       verbatimTextOutput("modsummary")
                               )
                        )
                      ), # end of top row on right column

                      fluidRow(
                               h4("Results:"),
                               uiOutput("table")
                      )
               ) # end of right column
             ) # end of overall fluid row
    ), # end of tabPanel 'Test for non-linear effects'

    # Tab: Data Check-------------------------------------------------------------------------------------------------------
    tabPanel("Data Check",
             uiOutput("message") # in the ui.R
    )
  ) # end of tabsetPanel

)) # end of shinyUI(fluidPage(
