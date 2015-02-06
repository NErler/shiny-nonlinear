output$HelpTab <- renderPrint({
  fluidRow(
    column(2),
    column(8,
           wellPanel(
             h3("How this App Works"),
             h4("Aim"),
             HTML("
              This application provides a tool to <span style='color:", AGEblue,"'> test for non-linear effects </span>
              using splines. It contains implementations of the most frequently used model types in epidemiologic
              research (<span style='color:",AGEblue,"'>linear, logistic, poisson</span> and
              <span style='color:",AGEblue,"'>Cox regression</span>). The non-linear fit is presented in a graph and
              the test results (<span style='color:",AGEblue,"'>Likelihood Ratio test statistic, degrees of freedom,
              p-value</span>) are given together with the conclusion.
              <br>
              This App does not provide a tool for extensive analyses."
             ),
             br(),br(),
             h4("Structure"),
             HTML("
              This App consists of three tabs that can be selected on top of the page:
              <ol>
              <li> This introductory tab</li>
              <li> The tab<span style='color:",AGEblue,"'> 'Test for non-linear effects'</span>,
                   which contains the main part of the App and provides the interface for uploading
                   the data and fitting the model.</li>
              <li> Once data have been uploaded, their format can be checked in the tab
                   <span style='color:",AGEblue,"'>'Data Check'</span></li>
              </ol>
              "),
             br(),
             h4("Instructions"),
             bsCollapse(multiple=T, id="collapse1",
                        bsCollapsePanel(h5("Data"),
                                        HTML(paste("
                  <ul>
                  <li>  Go to the tab<span style='color:",AGEblue,"'> 'Test for non-linear effects'</span>
                        and click on 'Choose File' to load a dataset.
                        Supported file types are .txt, .csv and .sav (SPSS).
                  <li>  When the upload is completed, go to the tab
                        <span style='color:",AGEblue,"'> 'Data Check'</span> to make sure that all variables
                        are coded properly. If necessary, correct the coding (e.g. in SPSS) and load the new dataset.
                  </ul>
                  "))),
                        bsCollapsePanel(h5("Model Type"),
                                        HTML(paste("
                  <ul>
                  <li>  Then the <span style='color:",AGEblue,"'> model type </span> can be selected in the tab
                        <span style='color:",AGEblue,"'> 'Test for non-linear effects'</span>. To fit a
                        linear regression model for a <span style='color:",AGEblue,"'> continuous outcome </span>
                        select 'linear'. The option 'logistic' fits a logistic model for a <span style='color:",AGEblue,
                                                   "'> binary outcome </span>. For <span style='color:",AGEblue,"'> count data </span> select
                        'poisson' and to fit a Cox proportional hazards model for a <span style='color:",AGEblue,"'>
                        survival outcome </span> select 'Cox'.
                  </ul>
                  "))),
                        bsCollapsePanel(h5("Model Structure"),
                                        HTML(paste("
                  The next step is to select the outcome variable, linear covariates and non-linear effects.
                  <ul>
                  <li>  To choose an <span style='color:",AGEblue,"'> outcome </span> click on the
                        dropdown list, find the variable by scolling through the list or start to
                        type the name, and select it by clicking on it or pressing Enter when it
                        is highlighted.
                  <li>  When <span style='color:",AGEblue,"'>'Cox' regression</span> is chosen as model
                        type, two dropdown lists appear. The <span style='color:",AGEblue,"'>
                        Time variable</span> should contain the event (or censoring) times. The
                        <span style='color:",AGEblue,"'>Event indicator</span> should be a dummy
                        variable that is 1 if an event was observed and 0 if the observation was censored.
                  <li>  To select <span style='color:",AGEblue,"'> covariates </span> click in the
                        white field to open a dropdown list and select one or more variables. To delete
                        a variable from the selection click on it and press 'Delete'.
                        It is possible to fit a model with only non-linear effects. Then the field for
                        the covariates needs to be kept empty.
                  <li>  <span style='color:",AGEblue,"'> Non-linear effects </span> can be selected in
                        the same manner as linear covariates.<br>
                        <span style='color:",AGEblue,"'><b> Note: </b></span> make sure that the
                        outcome, linear covariates and non-linear effects don't contain the same
                        variables!
                  </ul>
                  "))),
                        bsCollapsePanel(h5("Splines"),
                                        HTML(paste("
                  <ul>
                  <li>  For each selected non-linear effect a <span style='color:",AGEblue,"'> slider </span> will
                        appear in the section 'Splines'.
                        This slider controls the <span style='color:",AGEblue,"'> degrees of freedom (df) </span> of
                        each non-linear effect, i.e.
                        how flexible the fit is. The larger the number of degrees of freedom, the more
                        flexible the fit.
                  <li>  Setting the degrees of freedom for a spline to 1 reduces it to a linear fit.
                  </ul>
                  "))))
           )
    ),
    column(2)
  )
})
