output$message <- renderUI({
  if(is.null(OData()))return(h4("No dataset has been uploaded."))
  dataset <- OData()

  classes <- sapply(dataset, class)
  classes[classes == "numeric" | classes == "integer"] <- "numeric (continuous)"

  levs <- sapply(dataset, function(x){
    xlev <- levels(x)
    if(length(xlev)>20){
      xlev <- xlev[1:20]
      xlev[20] <- "..."
    }
    return(paste(xlev, sep=", "))
  })

  HTML(paste(
    "<h4> The dataset you loaded contains the following variables: </h4>
    <table class='data-table'>
    <tr> <th> variable name </th>
         <th> measurement level </th>
         <th> levels </th>
    </tr>",
    paste(lapply(1:ncol(dataset), function(i){
      paste('<tr> <td><b>', colnames(dataset)[i], '</b></td>',
            '<td> ', classes[i], '</td>',
            '<td> ', paste(levs[[i]], collapse=", "), '</td>',
            '</tr>')}), collapse=""),
    "</table>"),
    "<p class='contact-msg'><b> If categorical variables have not been imported correctly please set
    the levels appropriately (e.g. in SPSS)</b></p><br><br>")

})


