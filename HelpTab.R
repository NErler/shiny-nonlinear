output$HelpTab <- renderPrint({
  fluidRow(
    column(2),
    column(8,
           h3(HTML(paste("<span style='color:",AGEblue,"; font-weight:bold; font-size:18pt; font-family:Arial'> How this App works </span>"))),
           fluidRow(
             column(
               6,
               h4("Aim"),
               HTML("This application provides a tool to <span style='color:", AGEblue,"'> test for non-linear effects </span>
              in standard regression models (<span style='color:",AGEblue,"'>linear, logistic, poisson</span> and
              <span style='color:",AGEblue,"'>Cox regression</span>) using natural splines.
              The non-linear fit is presented in a graph and the test results are given together with the resulting conclusion.
              <br>
              This App does not provide a tool for extensive analyses."
               ),
               br(),br(),
               h4("Structure"),
               HTML("
              This app consists of three tabs that can be selected on top of the page:
              <ol>
              <li> This introductory tab</li>
              <li> The tab<span style='color:",AGEblue,"'> 'Test for non-linear effects'</span>,
                   which contains the main part of the app and provides the interface for uploading
                   the data and fitting the model</li>
              <li> Once data have been uploaded, their format can be checked in the tab
                   <span style='color:",AGEblue,"'>'Data Check'</span></li>
              </ol>
              "),
               br()),
             column(6,
             h4("Instructions"),
                        bsCollapsePanel(
                          h5("Data"),
                          HTML(
                            paste("<ul>",
                                  "<li> Go to the tab<span style='color:",AGEblue,"'> 'Test for non-linear effects'
                                        </span> and click on 'Choose File' to load a dataset.
                                        Supported file types are .csv and .sav (SPSS).",
                                  "<li> When the upload is completed, go to the tab <span style='color:",AGEblue,"'>
                                        'Data Check'</span> to make sure that all variables are coded properly.
                                        If necessary, correct the coding (e.g. in SPSS) and load the new dataset.",
                                  "</ul>")
                            )
                          ),
                        bsCollapsePanel(
                          h5("Model Type & Structure"),
                          HTML(
                            paste("The <span style='color:",AGEblue,"'> model type </span> (linear, logistic, poisson,
                                  Cox proprotional hazards) can be selected in the tab
                                  <span style='color:",AGEblue,"'> 'Test for non-linear effects'</span>.
                                  <!---
                                  To fit a linear regression model for a <span style='color:",AGEblue,"'> continuous
                                  outcome </span> select 'linear'. The option 'logistic' fits a logistic model for a
                                  <span style='color:", AGEblue, "'> binary outcome </span>.
                                  For <span style='color:",AGEblue,"'> count data </span> select 'poisson' and to fit
                                  a Cox proportional hazards model for a <span style='color:", AGEblue,"'>
                                  survival outcome </span> select 'Cox'.
                                  -->",

                                  "<br>",
                                  "<ul>",
                                  "<li> To choose an <span style='color:",AGEblue,"'> outcome </span> click on the
                                        dropdown list, find the variable by scolling through the list or start to
                                        type the name, and select it by clicking on it or pressing Enter when it
                                        is highlighted.",
                                  "<li> When <span style='color:",AGEblue,"'>'Cox' regression</span> is chosen as model
                                        type, two dropdown lists appear. The <span style='color:",AGEblue,"'> Time
                                        variable</span> should contain the event (or censoring) times.
                                        The <span style='color:",AGEblue,"'>Event indicator</span> should be a dummy
                                        variable that is 1 if an event was observed and 0 if the observation was
                                        censored.",
                                  "<li> To select <span style='color:",AGEblue,"'> covariates </span> click in the
                                        white field to open a dropdown list and select one or more variables.",
                                  "<li> <span style='color:",AGEblue,"'> Non-linear effects </span> can be selected in
                                        the same manner as linear covariates.<br>",
                                  "</ul>",

                                  "<span style='color:",AGEblue,"'><b> Note: </b></span> make sure that the outcome,
                                  linear covariates and non-linear effects don't contain the same variables!")
                            )
                          ),
                        bsCollapsePanel(
                          h5("Splines"),
                          HTML(
                            paste(
                              "<ul>",
                              "<li> For each selected non-linear effect a <span style='color:",AGEblue,"'> slider
                                    </span> will appear in the section 'Splines'.
                                    This slider controls the <span style='color:",AGEblue,"'> degrees of freedom (df)
                                    </span> of each non-linear effect, i.e. how flexible the fit is.
                                    The more degrees of freedom, the more flexible the fit.",
                              "<li> Setting the degrees of freedom to 1 reduces the spline to a linear fit.",
                              "</ul>")
                            )
                          ),
                        bsCollapsePanel(
                          h5("Output"),
                          HTML(
                            paste(
                              "<ul>",
                              "<li> The panel <span style='color:",AGEblue,"'> Model Dimension </span> gives an
                                    overview of the size of the model and the data used. Observations with missing
                                    values are excluded from the model.",
                              "<li> The button <span style='color:",AGEblue,"'> Open Model Summary </span> opens
                                    a window containing the model summary. This summary should be checked in order to
                                    <span style='color:",AGEblue,"'> detect problems </span> in the model fit
                                    (e.g. parameters that are NA).",
                              "<li> Finally, the panel <span style='color:",AGEblue,"'> Results </span> contains
                                    the results from the <span style='color:",AGEblue,"'> likelihood ratio test </span>,
                                    telling the user if a non-linear effect is needed.
                                    The <span style='color:",AGEblue,"'> Null-Hypothesis </span> of the test is, that
                                    the model contining a non-linear effect for a given variable is not significantly
                                    better than a model with a linear effect.",
                              "<li> The <span style='color:",AGEblue,"'> plot </span> next to the test results shows
                                    the estimated effect and its 95% confidence band. The black stripes at the bottom
                                    of the plot mark the location of the observations.",
                              "<li> The locations of the knots can be added to the plot as dashed vertical lines by
                                    checking the (left) box at the top of the panel. For continuous outcomes, partial
                                    residuals can be displayed (by checking the right box at the top of the panel).
                                    This will change the range of the y-axis.",
                              "</ul>")
                            )
                          )
           ))
    ),
    column(2)
  )
})
