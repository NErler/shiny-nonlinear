if (!"shinyBS" %in% installed.packages()[, "Package"]) {
  install.packages("shinyBS")
}
library(shiny)
library(shinyBS) # this has to be here, doesn't work if loaded from server.R

AGEblue <- "#004681"

shinyUI(fluidPage(

  # style file, containing warning messages etc.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "mycss.css")
  ),


  titlePanel(HTML(paste("<span style='color:",AGEblue,"; font-weight:bold'> Non-linear effects <span>"))),
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
                        radioButtons("inputType", h4("Dataset"), choices = list(".sav (SPSS)" = "sav", ".csv" = "csv"),
                                     selected = "sav", inline=T),
                        conditionalPanel(condition = "input.inputType == 'csv'",
                                         fluidRow(
                                         column(6,
                                                selectInput("decsep", strong("decimal separator"), choices = list("comma", "dot"))
                                                ),
                                         column(6,
                                                textInput("fieldsep", strong("field separator"), value="")
                                                )
                                         )),
                        fileInput("askData", label="", multiple=F),

                        radioButtons("modType", h4("Model type"),
                                     choices = list("linear" = "lin", "logistic" = "log",
                                                    "poisson" = "poi", "Cox" = "cox"),
                                     selected = "lin", inline=T),

                        br(),
                        h4("Model structure"),

                        conditionalPanel(
                          condition = "input.modType == 'cox'",
                          div(strong("Outcome")),
                          column(6, selectInput(inputId="CoxTime", 'Time variable',
                                                choices = c(Choose=''), multiple=F)),
                          column(6, selectInput(inputId="CoxEvent", 'Event indicator',
                                                choices = c(Choose=''), multiple=F))
                        ),
                        fluidRow(
                          column(5,
                                 conditionalPanel(
                                   condition = "input.modType != 'cox'",
                                   selectInput(inputId="outcome", "Outcome", choices = c(Choose=""), multiple=F)
                                 )),
                          column(7,
                                 uiOutput("err.modelmisspecified")
                          )),

                        uiOutput("err.resp_covar"),
                        uiOutput("err.covar_nonlin"),
                        selectizeInput(inputId="covars", 'Covariates', choices = list("Choose" = ""), multiple=T, width = '100%'),

                        uiOutput("err.resp_nonlin"),
                        selectizeInput(inputId="nonlin", 'Nonlinear effect(s)', choices = list("Choose" = ""), multiple=T, width = '100%'),

                        br(),
                        h4("Splines"),
                        uiOutput("sliders")
                      ) # end of wellpanel of left column
               ), # end of left column

               column(8,  # right column +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                      fluidRow(
                        column(12,
                               h3("Model Dimension & Summary"),
                               column(6,
                                      uiOutput("overfit")
                               ),
                               column(6,
                                      actionButton(inputId = "moTrig", "Open Model Summary", class = "btn-primary"),

                                      bsModal(id = "moMod", title = "Model Summary", trigger = "moTrig", size="large",
                                              #                                        tags$head(
                                              #                                          tags$style(HTML("
                                              #                                              .modal{
                                              #                                                width: 60%;
                                              #                                                left: 20%;
                                              #                                                margin-left:auto;
                                              #                                                margin-right:auto;
                                              #                                              }
                                              #                                            "))), # end of tags$head
                                              verbatimTextOutput("modsummary")
                                      ),
                                      bsAlert("mainAlert") # need this for the modal to work. No idea why.
                               )
                        )), # end of top row on right column

                      fluidRow(column(12,
                               h3("Results:"),
                               fluidRow(column(6, uiOutput("checkbox1")),
                                        column(6, uiOutput("checkbox2"))),
                               uiOutput("table")
                      ))
               ) # end of right column
             ) # end of overall fluid row
    ), # end of tabPanel 'Test for non-linear effects'

    # Tab: Data Check-------------------------------------------------------------------------------------------------------
    tabPanel("Data Check",
             uiOutput("message") # in the ui.R
    )
  ) # end of tabsetPanel

)) # end of shinyUI(fluidPage(
