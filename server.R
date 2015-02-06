# check if necessary packages are installed and install if not
list.of.packages <- c("shiny", "shinyBS", "stringr", "splines", "foreign", "survival", "lmtest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(packageVersion("shinyBS")!="0.25"){
  devtools::install_github("ebailey78/shinyBS", ref = "working")
}

# load packages
lapply(list.of.packages, require, character.only=T)


# define ErasmusAGE colors
AGEyellow <- "#F28F00"
AGEblue <- "#004681"
AGEgreen <- "#59A83D"



shinyServer(function(input, output, session){

  source("HelpTab.R", local=T)
  source("AnalysisTab.R", local=T)
  source("CheckData.R", local=T)
  source("ErrorMessages.R", local=T)

})
