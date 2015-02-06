tryCatch.W.E <- function(expr) {
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}


validModel <- reactive({
  if(any(input$outcome=="" & input$modType %in% c("lin", "log", "poi"),
         any(input$CoxTime=="", input$CoxEvent=="") & input$modType=="cox"))
    return(NULL)
  !any(
    if(input$modType=="log"){length(table(OData()[,input$outcome]))!=2},
    if(input$modType=="poi"){sum(OData()[,input$outcome] != round(as.numeric(OData()[,input$outcome])), na.rm=T)>0},
    if(input$modType=="lin"){is.factor(OData()[,input$outcome])},
      if(input$modType=="cox"){
        any(length(table(OData()[,input$CoxEvent]))!=2,
            is.factor(OData()[,input$CoxEvent]))
      }
      )
})

output$err.modelmisspecified <- renderUI({
  if(is.null(validModel())){
    return(NULL)
  }else{
    if(!validModel()){
      HTML(paste("
      <table class='warning'>
        <tr>
        <td><img src='Warning.png' style='width:60px;height:50px'></td>
        <td> The selected response variable doesn't match the selected model type.</td>
        </tr>
       </table>", sep=''))
    }
  }
})


output$err.resp_covar <- renderUI({
  if(input$outcome==""){
    return(NULL)
  }else{
  if(input$outcome %in% input$covars){
    HTML(paste('
      <table class="warning">
        <tr>
        <td><img src="Warning.png" style="width:60px;height:50px"></td>
        <td> You selected the variable "', input$outcome,
               '" for both the response and as a covariate.</td>
        </tr>
       </table>', sep=''))
  }}
})


output$err.resp_nonlin <- renderUI({
  if(input$outcome=="")return(NULL)
  if(input$outcome %in% input$nonlin){
    HTML(paste('
    <table class="warning">
      <tr>
      <td><img src="Warning.png" style="width:60px;height:50px"></td>
      <td> You selected the variable "', input$outcome,
               '" for both the response and as a non-linear effect.</td>
      </tr>
    </table>', sep=''))
  }
})


output$err.covar_nonlin <- renderUI({
  if(sum(input$nonlin %in% input$covars)>0){
    dbl <- which(input$nonlin %in% input$covars)
    HTML(paste('
    <table class="warning">
      <tr>
      <td><img src="Warning.png" style="width:60px;height:50px"></td>
      <td> You selected the variable(s) "',
               paste(input$nonlin[dbl], collapse ='" and "'),
           '" for both the covariates and as a non-linear effect(s).</td>
      </tr>
    </table>', sep=''))
  }
})

output$test <- renderPrint({
  get.warning(input$nonlin, model(), Data(), fmla())
})
