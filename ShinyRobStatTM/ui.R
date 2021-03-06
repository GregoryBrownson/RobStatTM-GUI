# ui.R
# 
# Author: Gregory Brownson
#
# Description: frontend portion of the Shiny 
#              interface for loading data and
#              selecting location and scale
#              parameters

# Command To Run:
#   shiny::runGitHub("RobStatTM-Gui", "GregoryBrownson", subdir = "ShinyRobStatTM", ref = 'dev')

library(DT)
library(shiny)
library(shinyjs)

# Some custom CSS code for formatting panels
CSS.format1 <- 
"
  /* Smaller font for preformatted text */
  pre, table.table {
    font-size: smaller;
  }

  body {
    min-height: 2000px;
  }

  .option-group {
    border: 1px solid #ccc;
    border-radius: 6px;
    padding: 0px 5px;
    margin: 5px -10px;
    background-color: #f5f5f5;
  }

  .option-header {
    color: #79d;
    text-transform: uppercase;
    margin-bottom: 5px;
  }
"

# JavaScript function to display slider input values in scientific notation
JS.log10 <-
"
  // Function to compute logarithmic scale with base 10 for slider input
  function log10 (sliderId) {
    $('#'+sliderId).data('ionRangeSlider').update({
        'prettify': function (num) { return ('1e'+num); }
      })
}"

# JavaScript code needed to call above function
JS.onCall <-
"
  $(document).ready(function() {
    // Wait for other scripts to execute
    setTimeout(function() {
      // Include call for each slider input
      log10('tolerance')
    }, 5)
  })
"

pkgs <- c("datasets", "RobStatTM", "robustbase", "PerformanceAnalytics")

# Define UI for Shiny Application
shinyUI(navbarPage("RobStatTM",
  
  # Tab to choose a data set
  
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(HTML(CSS.format1))),
        
        # Radio buttons to select data source
        radioButtons("source", "Data Source",
                     choices = c("R Package" = "library",
                                 "Upload"    = "upload"),
                     selected = "library"),
        # Create panel for uploading data
        conditionalPanel(
          condition = "input.source == 'upload'",
          # Select a file
          fileInput("file", "Choose CSV File",
                    multiple = TRUE,
                    accept   = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
  
          tags$hr(),
    
          # Checkbox for header
          checkboxInput("header", "Header", TRUE),
    
          # Radio buttons for delimiting character
          radioButtons("sep", "Separator",
                       choices  = c(Comma     = ",",
                                    Semicolon = ";",
                                    Tab       = "\t"),
                       selected = ","),
    
          # Radio buttons for type of quotes used
          radioButtons("quote", "Quote",
                       choices  = c(None           = "",
                                    "Double Quote" = '"',
                                    "Single Quote" = "'"),
                       selected = '"'),
          
          checkboxInput("data.ts", "Time Series?", FALSE)
        ),
        
        conditionalPanel(
          condition = "input.source == 'library'",
          # Selection of R packages
          selectInput("library", "Library Name",
                      choices = pkgs,
                      selected = "RobStatTM"),
          
          # Render UI given package selected
          uiOutput("select.data")
        ),
        
        # Button to display data table when pressed
        actionButton("display.table", "Load Data")
      ),
      
      # Panel for displaying the dataframe
      mainPanel(
        tags$head(tags$style(HTML(CSS.format1))),
            
        uiOutput("data.panel")
      )
    )
  ),
  
  # Tab for Location/Scale
  tabPanel("Location-Scale",
    h3("Location-Scale"),
    helpText("Calculate the robust location and scale for a single variable in
             a data set."),
    
    sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(HTML(CSS.format1))),
        tags$head(tags$script(HTML(JS.log10))),
        tags$head(tags$script(HTML(JS.onCall))),
        
        # Renders selection of univariate vectors chosen from
        # dataset
        uiOutput("locScale.select.variable"),
        
        # Selection of classic, robust, or both
        radioButtons("locScale.method", "Method",
                     choices = c("Both"      = "both",
                                 "Classical" = "classic",
                                 "Robust"    = "rob")),
        
        # Type disabled until scaleM() in RobStatTM is reviewed
        disabled(
          radioButtons("locScale.type", "Type",
                       choices = c("Both"     = "both",
                                   "Scale"    = "scale"))
        ),
        
        conditionalPanel("input['locScale.method'] != 'classic'",
                         
           selectInput("locScale.psi", "Score Function (Psi)",
                       choices = c("Mod. Opt." = "modopt",
                                   "Opt."      = "optimal",
                                   "Bisquare"  = "bisquare",
                                   "Huber"     = "huber")),
           
           # Radio buttons for desired asymptotic efficiency
           radioButtons("locScale.eff", "Asymptotic Efficiency",
                        choices  = c("0.85", "0.9", "0.95"),
                        selected = "0.9")
        ),
        
        # Button to display estimates for location and scale when pressed
        actionButton("locScale.display", "Results")
      ),
      
      mainPanel(
        tags$head(tags$style(HTML(CSS.format1))),
        # Display values for location and scale estimators
        htmlOutput("locScale.Results")
      )
    )
  ),
  
  # Tab to select from list of models
  
  navbarMenu("Models",
             
    ## Linear Regression ##
    tabPanel("Robust Linear Regression",
    h3("Robust Linear Regression"),
    helpText("Calculate the robust coefficients of several factors using any robust
             method andcompare to another robust regression or the least-squares
             equivalent."),
      tabsetPanel(id = "linear.tabs", type = "tabs",
                  
        # Model selection
        tabPanel(title = "Model", value = "linear.model",
          sidebarLayout(
            sidebarPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
              tags$head(tags$script(HTML(JS.log10))),
              tags$head(tags$script(HTML(JS.onCall))),
              
              checkboxInput("linRegress.second.method", "Add Second Method", value = FALSE),
              
              uiOutput("linRegress.options"),
        
              # Button to run selected regression
              actionButton("linRegress.display", "Results")
            ),
            
            mainPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("linRegress.results.ui")
            )
          )
        ), 
        
        # Plot selection
        tabPanel(title = "Plotting", value = "linear.plotting",
          sidebarLayout(
            sidebarPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
              h4("Plots"),
              checkboxInput("linRegress.residual.fit", "Residuals v. Fit", TRUE),
              checkboxInput("linRegress.response.fit", "Response v. Fit", TRUE),
              checkboxInput("linRegress.qq", "Residuals Normal QQ Plot", TRUE),
              checkboxInput("linRegress.resid.dist", "Std. Residuals v. Robust Distances", TRUE),
              checkboxInput("linRegress.residual.density", "Estimated Residual Density", TRUE),
              checkboxInput("linRegress.resid.index", "Std. Residuals v. Index (Time)", TRUE),
              conditionalPanel(
                condition = "input['linRegress.second.method']",
                
                checkboxInput("linRegress.overlaid.scatter", "Scatter with Overlaid Fits", TRUE)
              ),
              
              tags$hr(),
              
              h4("Options"),
              #checkboxInput("include.smooth", "Include Smooth", TRUE),
              checkboxInput("include.rugplot", "Include Rugplot", FALSE),
              checkboxInput("linRegress.qq.env", "QQ Plot Envelope", TRUE),
              checkboxInput("linRegress.qqline.robust", "Include Robust QQ Line", TRUE),
              #uiOutput("extreme.points"),
              
              # Button to run selected regression
              actionButton("linRegress.display.plots", "View Plots")
            ),
          
            mainPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("linRegress.plot.ui")
            )
          )
        )
      )
    ),
    
    tabPanel("Robust Covariance",
      h3("Robust Covariance"),
      helpText("Compute estimates for multivariate location and scale using classical and/or robust methods."),
      tabsetPanel(id = "covariance.tabs", type = "tabs",
        tabPanel(title = "Estimates", value = "covariance.est",
          sidebarLayout(
            sidebarPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("covariance.select.variables"),
              
              radioButtons("covariance.method", "Method",
                           choices = c("Both"      = "both",
                                       "Classical" = "classic",
                                       "Robust"    = "rob")),
              
              radioButtons("covariance.type", "Type",
                           choices = c("Covariances"  = "cov",
                                       "Correlations" = "corr")),
              
              conditionalPanel("input['covariance.method'] != 'classic'",
                selectInput("covariance.estimator", "Robust Covariance Estimator",
                             choices = c("Auto", "MM", "Rocke"))
              ),
              
              actionButton("covariance.display", "Results")
            ),
            
            mainPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("covariance.results.ui")
            )
          )
        ),
        
         tabPanel("Plots",
          sidebarLayout(
            sidebarPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              useShinyjs(),
              
              h4("Plots"),
              
              checkboxInput("covariance.eigen", "Eigenvalues", TRUE),
              checkboxInput("covariance.mahalanobis", "Mahalanobis Distances", TRUE),
              checkboxInput("covariance.ellipses.matrix", "Ellipses Matrix", TRUE),
              checkboxInput("covariance.chi.qqplot", "Chi-Square QQ Plot", TRUE),
              
              conditionalPanel(
                condition = "input['covariance.method'] == 'both'",
                disabled(
                  checkboxInput("covariance.image.display", "Image Display", TRUE)
                )
              ),
              
              conditionalPanel(
                condition = "input['covariance.method'] == 'both'",
                checkboxInput("covariance.dist.dist", "Distance-Distance Plot", TRUE)
              ),
              
              actionButton("covariance.display.plots", "View Plots")
            ),
            
            mainPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("covariance.plot.ui")
            )
          )
        )
      )
    ),
    
    tabPanel("Robust PCA",
      h3("Robust Principle Component Analysis"),
      helpText("Principal component analysis using classical and/or robust methods."),
      tabsetPanel(id = "pca.tabs", type = "tabs",
        tabPanel(title = "Estimates", value = "pca.est",
          sidebarLayout(
            sidebarPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("pca.select.variables"),
              
              radioButtons("pca.method", "Method",
                           choices = c("Both"      = "both",
                                       "Classical" = "classic",
                                       "Robust"    = "rob")),
              
              radioButtons("pca.type", "Type",
                           choices = c("Covariances"  = "cov",
                                       "Correlations" = "corr")),
              
              conditionalPanel("input['pca.method'] != 'classic'",
                selectInput("pca.estimator", "Robust PCA Estimator",
                            choices = c("Auto", "MM", "Rocke"))
              ),
              
              actionButton("pca.display", "Results")
            ),
            
            mainPanel(
              tags$head(tags$style(HTML(CSS.format1))),
              
              uiOutput("pca.results.ui")
            )
          )
        ),
        
        disabled(
          tabPanel("Plots",
            sidebarLayout(
              sidebarPanel(
                tags$head(tags$style(HTML(CSS.format1))),
                h4("Plots"),
                checkboxInput("pca.scatter", "Scatter Plots", TRUE),
                checkboxInput("pca.loadings", "Loadings", TRUE),
                checkboxInput("pca.scree", "Screeplot", TRUE),
                
                actionButton("pca.display.plots", "View Plots")
              ),
              
              mainPanel(
                tags$head(tags$style(HTML(CSS.format1))),
                
                uiOutput("pca.plot.ui")
              )
            )
          )
        )
      )
    )
  ),
  
  tabPanel("About",
    htmlOutput("about.text")
  ),
  
  tabPanel("Help",
    htmlOutput("help.text")
  )
))
