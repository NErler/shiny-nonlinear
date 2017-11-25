# load packages
library(splines)
library(lmtest)
library(survival)
library(stringr)

# define ErasmusAGE colors
AGEyellow <- "#F28F00"
AGEblue <- "#004681"
AGEgreen <- "#59A83D"



shinyServer(function(input, output, session){

  options(shiny.maxRequestSize = 20*1024^2)

  source("HelpTab.R", local = T)
  source("AnalysisTab.R", local = T)
  source("CheckData.R", local = T)
  source("ErrorMessages.R", local = T)

})
