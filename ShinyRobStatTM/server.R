# server.R
# 
# Author: Gregory Brownson
#
# Description: Backend portion of the Shiny 
#              interface for loading data and
#              selecting location and scale
#              parameters

library(DT)
library(ggplot2)
library(RobStatTM)
library(shiny)

# Back-end implementation of Shiny server
shinyServer(function(input, output) {
  
  # Need this to store reactive objects
  values <- reactiveValues()
  
####################  
## Data Selection ##
####################
  
  # Display correct options to obtain data given
  # method selected by user
  output$select.data <- renderUI({
    # If no package is chosen, then do nothing
    if(is.null(input$library)) {
      return("")
    }
    
    # List of datasets
    list <- data(package = input$library)$results[, "Item"]
    
    # Get info on datasets
    values$data.info <- vcdExtra::datasets(input$library)
    
    # Create selection for datasets
    selectInput("dataset",
                label   = "Select Dataset",
                choices = list)
  })
  
  # On-click, load the data and return the data frame
  contents_table <- eventReactive(input$display.table, {
    if (input$source == "upload") {
      req(input$file)
      
      # Read data from file
      values$dat <- read.csv(input$file$datapath,
                      header = input$header,
                      sep    = input$sep,
                      quote  = input$quote)
      
      # If there are no headers, give data headers
      if (input$header == FALSE)
      {
        colnames(dat)[-1] <- paste0('X', 1:(ncol(dat) - 1))
      }
    } else {
      # If no dataset exists, return nothing
      if (is.null(input$dataset)) {
        return()
      }
      
      # Obtain specified dataset from specified package
      data(list = input$dataset, package = input$library)
      
      values$dat <- as.data.frame(get(input$dataset))
    }
    
    # Get variable names
    values$dat.variables <- colnames(values$dat)
      
    return (values$dat)
  })
  
  # Create data table
  output$contents.table <- DT::renderDataTable({
    contents_table()
  })
  
####################
## Location/Scale ##
####################

    # Render variable input list
  output$select.variable <- renderUI({
    # If there is no data, do nothing
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    # Render select input for variables
    selectInput("variable", "Variable",
                choices = values$dat.variables)
  })
  
  # On-click, find the estimators and create string object of results
  contents_estimators <- eventReactive(input$display.Location, {
    if (is.null(dim(values$dat))) {
      return("ERROR: No data. Please upload a data set or select one from the available libraries.")
    }
    
    # Get values for location and scale using 'MLocDis' function from
    # 'RobStatTM' package
    est <- MLocDis(x     = as.numeric(values$dat[,input$variable]),
                   psi   = input$psi,
                   eff   = input$efficiency,
                   maxit = input$max.iter,
                   tol   = input$tolerance)
    
    # Store results in string objects
    line1 <- paste0("Location (Error): ", round(est$mu, 4), " (", round(est$std.mu, 4), ")")
    line2 <- paste0("      Dispersion: ", round(est$disper, 4))

    return(paste(line1, line2, sep = "\n"))
  })
  
  # Display output
  output$results.Location <- renderText({
    contents_estimators()
  })
  
#########################
## Linear Regression I ##
#########################
  
  ## Running Regression ##
  
  values$regress.methods <- c("Least Squares", "M", "MM", "Distance Constrained")
  values$regress.models  <- list(lm, lmrobM, lmrobdetMM, lmrobdetDCML)

  output$select.dependent.LinRegress <- renderUI({
    # If there is no data, do nothing
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    # Render select input for variables
    selectInput("dependent.var.LinRegress", "Dependent",
                choices = values$dat.variables)
  })

  output$select.independent.LinRegress <- renderUI({
    # If no dependent variable is selected, do nothing
    if (is.null(input$dependent.var.LinRegress)) {
      return("")
    }
    
    ind.vars <- setdiff(values$dat.variables, input$dependent.var.LinRegress)
    
    # Render select input for variables
    selectInput("independent.var.LinRegress", "Independent",
                choices = ind.vars,
                selected = ind.vars,
                multiple = TRUE)
  })
  
  output$formula.LinRegress <- renderUI({
    if (is.null(input$dependent.var.LinRegress)) {
      return("")
    }
    
    formula.str <- paste(input$dependent.var.LinRegress, " ~ ")
    
    ind.vars <- input$independent.var.LinRegress
    
    n <- length(ind.vars)
    
    if (n > 1) {
      for (i in 1:(n - 1)) {
        formula.str <- paste(formula.str, ind.vars[i], " + ")
      }
    }
    
    formula.str <- paste(formula.str, ind.vars[n])
    
    textInput("formula.text", "Formula",
              formula.str)
  })
  
  run_regression <- eventReactive(input$display.LinRegress, {
    index <- match(input$fit.option, values$regress.methods)
    
    n <- length(index)
    
    model <- lapply(index,
                    function(i, model.list) {
                      model.list[[i]]
                    },
                    values$regress.models)
    
    fit <- vector(mode = "list", length = length(index))
    
    fit[[1]] <- model[[1]](as.formula(input$formula.text), data = values$dat)
    
    
    if (length(index) == 2) {
      fit[[2]] <- model[[2]](as.formula(input$formula.text), data = values$dat)
    }
    
    return(fit)
  })
  
  invalid_response <- eventReactive(input$display.LinRegress, {
    return(paste("ERROR: Response variable is of",
                 class(values$dat[, input$dependent.var.LinRegress]),
                 "type. Please select a response variable with numeric values"))
  })
  
  output$results.LinRegress <- renderPrint({
    if (is.numeric(values$dat[, input$dependent.var.LinRegress])) {
      fit <- run_regression()
    
      values$fit1 <- fit[[1]]
      
      values$num.fits <- length(fit)
      
      if (values$num.fits == 2) {
        values$fit2 <- fit[[2]]
      }
      
      print(summary(values$fit1))
    
      if (values$num.fits == 2) {
        print(summary(values$fit2))
      }
    } else {
      invalid_response()
    }
  })
  
  ## Plotting ##
  
  output$extreme.points <- renderUI({
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    num.obs <- nrow(values$dat)
    
    numericInput("num.extreme.points",
                 "Number of Extreme Points to Identify",
                 value = 0,
                 max = num.obs)
  })
  
  observeEvent(input$display.plots, {
    plots <- vector(mode = "list")
    
    i <- 0
    
    if (input$residual.fit == T) {
      
    }
    
    if (input$response.fit == T) {
      
    }
    
    if (input$qq == T) {
      i <- i + 1
      
      fit <- values$fit1
      
      qqnorm(fit$residuals)
      qqline(fit$residuals)
      
      dat <- as.data.frame(fit$residuals)
      
      colnames(dat) <- c("Res")
      
      # Calculate slope and intercept for qqline
      
      y     <- quantile(fit$residuals, c(0.25, 0.75), type=5)
      x     <- qnorm(c(0.25, 0.75))
      slope <- diff(y) / diff(x)
      int   <- y[1] - slope * x[1]
      
      # Normal QQ plot
      p <- ggplot(data = dat, aes(sample = Res)) +
             geom_qq() + geom_abline(slope = slope, intercept = int)
      
      plots[[i]] <- p
    }
    
    if (input$stdResidual.RobustDist == T) {
      
    }
    
    if (input$residual.density == T) {
      
    }
    
    if (input$stdResidual.Index == T) {
      
    }
    
    i <- i + 1
    
    dat <- data.frame(X = 1:6, Y = 3:8)
    
    plots[[i]] <- ggplot(dat, aes(x = X, y = Y)) +
                    geom_point()
    
    values$plots <- plots

    values$num.plots <- i

    values$active.index <- 1

    values$active.plot <- plots[[1]]
    
    output$plot.ui <- renderUI({
      fluidPage(
        wellPanel(
          plotOutput("plot.output")
        ),
      
        fluidRow(
          column(1,
                 offset = 8,
                 actionButton("next.plot",
                              "",
                              icon = icon("angle-right", "fa-2x"))
          )
        )
      )
    })
    
    output$plot.output <- renderPlot({
      values$active.plot
    })
  })
  
  output$plot.output <- renderPlot({
      values$active.plot
    })
  
  observeEvent(input$next.plot, {
    if (values$num.plots > 0) {
      values$active.index <- values$active.index %% values$num.plots + 1
    
      values$active.plot <- values$plots[[values$active.index]]
      
      output$plot.ui <- renderUI({
        if (values$active.index == 1) {
          fluidPage(
            wellPanel(
              plotOutput("plot.output")
            ),
          
            fluidRow(
              column(1,
                     offset = 8,
                     actionButton("next.plot",
                                  "",
                                  icon = icon("angle-right", "fa-2x"))
              )
            )
          )
        } else if (values$active.index == values$num.plots) {
          fluidPage(
            wellPanel(
              plotOutput("plot.output")
            ),
          
            fluidRow(
              column(1,
                     offset = 1,
                     actionButton("prev.plot",
                                  "",
                                  icon = icon("angle-left", "fa-2x"))
              )
            )
          )
        } else {
          fluidPage(
            wellPanel(
              plotOutput("plot.output")
            ),
          
            fluidRow(
              column(1,
                     offset = 1,
                     actionButton("prev.plot",
                                  "",
                                  icon = icon("angle-left", "fa-2x"))
              ),
              
              column(1,
                     offset = 8,
                     actionButton("next.plot",
                                  "",
                                  icon = icon("angle-right", "fa-2x"))
              )
            )
          )
        }
      })
    
      output$plot.output <- renderPlot({
        values$active.plot
      })
    }
  })
  
  observeEvent(input$prev.plot, {
    if (values$num.plots > 0) {
      values$active.index <- values$num.plots + (values$active.index - 1) %% -values$num.plots
    
      values$active.plot <- values$plots[[values$active.index]]
      
      output$plot.ui <- renderUI({
        if (values$active.index == 1) {
          fluidPage(
            wellPanel(
              plotOutput("plot.output")
            ),
          
            fluidRow(
              column(1,
                     offset = 8,
                     actionButton("next.plot",
                                  "",
                                  icon = icon("angle-right", "fa-2x"))
              )
            )
          )
        } else if (values$active.index == values$num.plots) {
          fluidPage(
            wellPanel(
              plotOutput("plot.output")
            ),
          
            fluidRow(
              column(1,
                     offset = 1,
                     actionButton("prev.plot",
                                  "",
                                  icon = icon("angle-left", "fa-2x"))
              )
            )
          )
        } else {
          fluidPage(
            wellPanel(
              plotOutput("plot.output")
            ),
          
            fluidRow(
              column(1,
                     offset = 1,
                     actionButton("prev.plot",
                                  "",
                                  icon = icon("angle-left", "fa-2x"))
              ),
              
              column(1,
                     offset = 8,
                     actionButton("next.plot",
                                  "",
                                  icon = icon("angle-right", "fa-2x"))
              )
            )
          )
        }
      })
    
      output$plot.output <- renderPlot({
        values$active.plot
      })
    }
  })
})