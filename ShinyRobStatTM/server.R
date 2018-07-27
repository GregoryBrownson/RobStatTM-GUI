# server.R
# 
# Author: Gregory Brownson
#
# Description: Backend portion of the Shiny 
#              interface for loading data and
#              selecting location and scale
#              parameters

library(DT)
library(fit.models)
library(ggplot2)
library(grid)
library(gridExtra)
library(PerformanceAnalytics)
library(RobStatTM)
library(robustbase)
library(shiny)
library(xts)

thm <- theme_bw() +
       theme(plot.title = element_text(hjust = 0.5))

theme_set(thm)

lm <- function(form, ...) {
  fit <- stats::lm(form, ...)
  fit$call <- form
  return(fit)
}

lmrobM <- function(form, ...) {
  fit <- RobStatTM::lmrobM(form, ...)
  fit$call <- form
  return(fit)
}

lmrobdetDCML <- function(form, ...) {
  fit <- RobStatTM::lmrobdetDCML(form, ...)
  fit$call <- form
  return(fit)
}

lmrobdetMM <- function(form, ...) {
  fit <- RobStatTM::lmrobdetMM(form, ...)
  fit$call <- form
  return(fit)
}

# Add classes to lmfm and covfm in fit.models registry
fmclass.add.class("lmfm", "lmrobM", warn = F)
fmclass.add.class("lmfm", "lmrobdetMM", warn = F)
fmclass.add.class("lmfm", "lmrobdetDCML", warn = F)

fmclass.add.class("covfm", "covClassic", warn = F)
fmclass.add.class("covfm", "covRob", warn = F)

covClassic <- function(data, data.name, ...) {
  z <- RobStatTM::covClassic(data, ...)
  z$call <- call("covClassic", data = as.name(data.name))
  
  return(z)
}

covRob <- function(data, data.name, corr = F, ...) {
  z <- RobStatTM::covRob(data, ...)
  z$corr <- corr
  z$call <- call("covRob", data = as.name(data.name))
  class(z) <- "covRob"
  
  if(corr == T) {
    D <- sqrt(diag(z$cov))
    z$cov <- t(diag(1 / D)) %*% z$cov %*% diag(1 / D)
  }
  
  return(z)
}

covRobMM <- function(data, data.name, corr = F, ...) {
  z <- RobStatTM::covRob(data, type = "MM", ...)
  z$corr <- corr
  z$call <- call("covRobMM", data = as.name(data.name))
  class(z) <- "covRob"
  
  if(corr == T) {
    D <- sqrt(diag(z$cov))
    z$cov <- t(diag(1 / D)) %*% z$cov %*% diag(1 / D)
  }
  
  return(z)
}

covRobRocke <- function(data, data.name, corr = F, ...) {
  z <- RobStatTM::covRob(data, type = "Rocke", ...)
  z$corr <- corr
  z$call <- call("covRobRocke", data = as.name(data.name))
  class(z) <- "covRob"
  
  if (corr == T) {
    D <- sqrt(diag(z$cov))
    z$cov <- t(diag(1 / D)) %*% z$cov %*% diag(1 / D)
  }
  
  return(z)
}

# Back-end implementation of Shiny server
shinyServer(function(input, output) {
  
  # Need this to store reactive objects
  values <- reactiveValues()
  
  values$regress.active <- F
  values$linRegress.plots.active <- F
  
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
    lst <- data(package = input$library)
    
    # Create selection for datasets
    selectInput("dataset",
                label   = "Select Dataset",
                choices = lst$results[, "Item"])
  })
  
  observeEvent(input$display.table, {
    output$data.panel <- renderUI({
      fluidPage(
        # Create Data table
        fluidRow(
          wellPanel(DT::dataTableOutput("contents.table"))
        ),
        
        fluidRow(
          column(2, offset = 5,
            actionLink("data.info.link", label = "More Info")
          )
        )
      )
    })
    
    output$data.info <- 
    
    output$contents.table <- DT::renderDataTable({
      contents_table()
    })
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
      
      values$dat <- get(input$dataset)
    }
    
    # Get variable names
    values$dat.variables <- colnames(values$dat)
    
    values$dat.numeric <- values$dat[, sapply(values$dat, is.numeric) | sapply(values$dat, is.integer)]
    
    values$dat.numeric.variables <- colnames(values$dat.numeric)
    
    return (as.data.frame(values$dat))
  })
  
  observeEvent(input$more.info, {
    
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
    est <- MLocDis(x     = as.numeric(values$dat[, input$variable]),
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
  
  values$linRegress.methods <- c("LS", "M", "MM", "DCML")
  values$linRegress.functions  <- c("lm", "lmrobM", "lmrobdetMM", "lmrobdetDCML")
  
  output$linRegress.options <- renderUI({
    if (input$linRegress.second.method) {
      tabPanel("",
        fluidRow(
          column(6,
            selectInput("linRegress.fit.option", "Method",
                        choices   = c("LS", "M", "MM", "DCML"),
                        selected  = "MM"),
            
            # List of dependent variables, must be selected
            uiOutput("linRegress.select.dependent"),
            
            # List of predictors to choose from
            uiOutput("linRegress.select.independent"),
            
            # String representing regression formula of form Y ~ X_0 + ... + X_n
            uiOutput("linRegress.formula"),
            
            # Interface to robust options
            uiOutput("linRegress.robust.control")
          ),
          
          column(6,
            selectInput("linRegress.fit.option2", "Method",
                        choices   = c("LS", "M", "MM", "DCML"),
                        selected  = "MM"),
            
            # List of dependent variables, must be selected
            uiOutput("linRegress.select.dependent2"),
            
            # List of predictors to choose from
            uiOutput("linRegress.select.independent2"),
            
            # String representing regression formula of form Y ~ X_0 + ... + X_n
            uiOutput("linRegress.formula2"),
            
            # Interface to robust options
            uiOutput("linRegress.robust.control2")
          )
        )
      )
    } else {
      tabPanel("",
        selectInput("linRegress.fit.option", "Method",
                    choices   = c("LS", "M", "MM", "DCML"),
                    selected  = "MM"),
        
        # List of dependent variables, must be selected
        uiOutput("linRegress.select.dependent"),
        
        # List of predictors to choose from
        uiOutput("linRegress.select.independent"),
        
        # String representing regression formula of form Y ~ X_0 + ... + X_n
        uiOutput("linRegress.formula"),
        
        # Interface to robust options
        uiOutput("linRegress.robust.control")
      )
    }
  })

  output$linRegress.select.dependent <- renderUI({
    # If there is no data, do nothing
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    # Render select input for variables
    selectInput("linRegress.dependent", "Dependent",
                choices = values$dat.numeric.variables)
  })

  output$linRegress.select.independent <- renderUI({
    # If no dependent variable is selected, do nothing
    if (is.null(input$linRegress.dependent)) {
      return("")
    }
    
    ind.vars <- setdiff(values$dat.variables, input$linRegress.dependent)
    
    # Render select input for variables
    selectInput("linRegress.independent", "Independent",
                choices  = ind.vars,
                selected = ind.vars[1],
                multiple = TRUE)
  })
  
  output$linRegress.formula <- renderUI({
    if (is.null(input$linRegress.dependent)) {
      return("")
    }
    
    formula.str <- paste(input$linRegress.dependent, " ~ ")
    
    ind.vars <- input$linRegress.independent
    
    n <- length(ind.vars)
    
    if (n > 1) {
      for (i in 1:(n - 1)) {
        formula.str <- paste(formula.str, ind.vars[i], " + ")
      }
    }
    
    formula.str <- paste(formula.str, ind.vars[n])
    
    textInput("linRegress.formula.text", "Formula",
              formula.str)
  })
  
  output$linRegress.select.dependent2 <- renderUI({
    # If there is no data, do nothing
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    # Render select input for variables
    selectInput("linRegress.dependent2", "Dependent",
                choices = values$dat.numeric.variables)
  })

  output$linRegress.select.independent2 <- renderUI({
    # If no dependent variable is selected, do nothing
    if (is.null(input$linRegress.dependent2)) {
      return("")
    }
    
    ind.vars <- setdiff(values$dat.variables, input$linRegress.dependent2)
    
    # Render select input for variables
    selectInput("linRegress.independent2", "Independent",
                choices  = ind.vars,
                selected = ind.vars[1],
                multiple = TRUE)
  })
  
  output$linRegress.formula2 <- renderUI({
    if (is.null(input$linRegress.dependent)) {
      return("")
    }
    
    formula.str <- paste(input$linRegress.dependent2, " ~ ")
    
    ind.vars <- input$linRegress.independent2
    
    n <- length(ind.vars)
    
    if (n > 1) {
      for (i in 1:(n - 1)) {
        formula.str <- paste(formula.str, ind.vars[i], " + ")
      }
    }
    
    formula.str <- paste(formula.str, ind.vars[n])
    
    textInput("linRegress.formula.text2", "Formula",
              formula.str)
  })
  
  output$linRegress.robust.control <- renderUI({
    if (input$linRegress.fit.option != "LS") {
      if (input$linRegress.second.method == TRUE) {
        req(input$linRegress.fit.option2)
        
        if(input$linRegress.fit.option2 != "LS") {
          tabPanel("",
            tags$hr(),
            
            h4("Robust Controls 1"),
            
            selectInput("linRegress.family", "Family",
                        choices = c("Bi-square" = "bisquare",
                                    "Opt."      = "optimal",
                                    "Mod. Opt." = "modified.optimal"),
                        selected = "modified.optimal"),
            
            numericInput("linRegress.eff", "Efficiency", value = 0.99, min = 0.80, max = 0.99, step = 0.01)
          )
        } else {
          tabPanel("",
            tags$hr(),
            
            h4("Robust Controls"),
            
            selectInput("linRegress.family", "Family",
                        choices = c("Bi-square" = "bisquare",
                                    "Opt."      = "optimal",
                                    "Mod. Opt." = "modified.optimal"),
                        selected = "modified.optimal"),
            
            numericInput("linRegress.eff", "Efficiency", value = 0.99, min = 0.80, max = 0.99, step = 0.01)
          )
        }
      } else {
        tabPanel("",
          tags$hr(),
          
          h4("Robust Controls"),
          
          selectInput("linRegress.family", "Family",
                      choices = c("Bi-square" = "bisquare",
                                  "Opt."      = "optimal",
                                  "Mod. Opt." = "modified.optimal"),
                      selected = "modified.optimal"),
          
          numericInput("linRegress.eff", "Efficiency", value = 0.99, min = 0.80, max = 0.99, step = 0.01)
        )
      }
    }
  })
  
  output$linRegress.robust.control2 <- renderUI({
    if (input$linRegress.fit.option2 != "LS") {
      if (input$linRegress.fit.option != "LS") {
        tabPanel("",
          tags$hr(),
          
          h4("Robust Controls 2"),
          
          selectInput("linRegress.family2", "Family",
                      choices = c("Bi-square" = "bisquare",
                                  "Opt."      = "optimal",
                                  "Mod. Opt." = "modified.optimal"),
                      selected = "modified.optimal"),
          
          numericInput("linRegress.eff2", "Efficiency", value = 0.99, min = 0.80, max = 0.99, step = 0.01)
        )
      } else {
        tabPanel("",
          tags$hr(),
          
          h4("Robust Controls"),
          
          selectInput("linRegress.family", "Family",
                      choices = c("Bi-square" = "bisquare",
                                  "Opt."      = "optimal",
                                  "Mod. Opt." = "modified.optimal"),
                      selected = "modified.optimal"),
          
          numericInput("linRegress.eff", "Efficiency", value = 0.99, min = 0.80, max = 0.99, step = 0.01)
        )
      }
    }
  })
  
  observeEvent(input$linRegress.display, {
    if (is.null(input$linRegress.independent)) {
      output$linRegress.results <- renderPrint({
        cat("ERROR: Missing dependent or independent variables")
      })
    } else if (!is.numeric(values$dat[, input$linRegress.dependent])) {
      output$linRegress.results <- renderPrint({ invalid_response() })
    }
    
    if (input$linRegress.second.method) {
      methods <- c(input$linRegress.fit.option, input$linRegress.fit.option2)
      
      index <- match(methods, values$linRegress.methods)
    
      n <- length(index)
    
      model <- sapply(index,
                      function(i, m) {
                        m[i]
                      },
                      values$linRegress.functions)
    } else {
      model <- values$linRegress.functions[match(input$linRegress.fit.option, values$linRegress.methods)]
    }
    
    fit <- vector(mode = "list", length = length(index))
    if (model[1] == "lm") {
      fit[[1]] <- do.call(model[1], list(as.formula(input$linRegress.formula.text), data = values$dat))
    } else {
      control <- lmrobdet.control(efficiency = input$linRegress.eff,
                                  family = input$linRegress.family,
                                  compute.rd = T)
    
      fit[[1]] <- do.call(model[1], list(as.formula(input$linRegress.formula.text),
                                         data    = values$dat,
                                         control = control))
    }
    
    fit[[1]]$call <- call(model[1], as.formula(input$linRegress.formula.text))
    
    if (input$linRegress.second.method) {
      if (model[2] == "lm") {
        fit[[2]] <- do.call(model[2], list(as.formula(input$linRegress.formula.text2), data = values$dat))
      } else {
        control2 <- lmrobdet.control(efficiency = input$linRegress.eff2,
                                    family = input$linRegress.family2,
                                    compute.rd = T)
      
        fit[[2]] <- do.call(model[2], list(as.formula(input$linRegress.formula.text2),
                                           data    = values$dat,
                                           control = control2))
      }
        
      fit[[2]]$call <- call(model[2], as.formula(input$linRegress.formula.text2))
      
      fm <- fit.models(fit[[1]], fit[[2]])
    } else {
      fm <- fit.models(fit[[1]])
    }

    values$regress.active <- T
    
    if (input$linRegress.second.method) {
      if (input$linRegress.fit.option == input$linRegress.fit.option2) {
        values$linRegress.models <- c(paste(input$linRegress.fit.option, "1"), paste(input$linRegress.fit.option[1], "2"))
      } else {
        values$linRegress.models <- c(input$linRegress.fit.option, input$linRegress.fit.option2)
      }
    } else {
      values$linRegress.models <- input$linRegress.fit.option
    }
    
    names(fm) <- values$linRegress.models
      
    output$linRegress.results <- renderPrint({
      values$linRegress.fm <- fm
      
      values$linRegress.fit <- fm[[1]]
      
      print(summary(values$linRegress.fm))
      
      values$linRegress.num.fits <- length(fm)
        
      if (values$linRegress.num.fits == 2) {
        values$linRegress.fit2 <- fm[[2]]
      }
    })
  })
  
  invalid_response <- eventReactive(input$linRegress.display, {
    return(paste("ERROR: Response variable is of",
                 class(values$dat[, input$linRegress.dependent]),
                 "type. Please select a response variable with numeric values"))
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
  
  observeEvent(input$linRegress.display.plots, {
    values$linRegress.plots.active <- T
    
    plots <- vector(mode = "list")
    
    i <- 0
    
    fit <- values$linRegress.fit
    
    # Residual v. fitted values
    if (input$linRegress.residual.fit == T) {
      i <- i + 1
      
      fit.vals <- fitted(fit)
      
      dat <- data.frame(X = fit.vals, Y = fit$residuals)
      
      sigma <- 1
      
      if (any(class(fit) == "lm")) {
        sigma <- sd(fit$residuals)
      } else {
        sigma <- fit$scale
      }
      
      title.name <- ifelse(any(class(fit) == "lm"), values$linRegress.models[1], paste("Robust", values$linRegress.models[1]))
      
      plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                      ggtitle(title.name) +
                      xlab("Fitted Values") +
                      ylab("Residuals") +
                      geom_point() +
                      geom_hline(yintercept = c(-2.5 * sigma, 0, 2.5 * sigma),
                                 linetype = 2)
      
      if (input$include.rugplot == T) {
        plots[[i]] <- plots[[i]] + geom_rug()
      }
    }
    
    # Response v. fitted values
    if (input$linRegress.response.fit == T) {
      i <- i + 1

      fit.vals <- fitted(fit)
      
      response <- fit$model[, 1]

      dat <- data.frame(X = fit.vals, Y = response)
      
      title.name <- ifelse(any(class(fit) == "lm"), values$linRegress.models[1], paste("Robust", values$linRegress.models[1]))

      plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                      ggtitle(title.name) +
                      xlab("Fitted Values") +
                      ylab("Response") +
                      geom_point()

      if (input$include.rugplot == T) {
        plots[[i]] <- plots[[i]] + geom_rug()
      }
    }

    # QQ Plot
    if (input$linRegress.qq == T) {
      i <- i + 1

      dat <- data.frame(Res = sort(fit$residuals))
      
      title.name <- ifelse(any(class(fit) == "lm"), values$linRegress.models[1], paste("Robust", values$linRegress.models[1]))

      # Calculate slope and intercept for qqline
      
      y     <- quantile(fit$residuals, c(0.25, 0.75), type = 5)
      x     <- qnorm(c(0.25, 0.75))
      slope <- diff(y) / diff(x)
      int   <- y[1] - slope * x[1]
      
      if (input$linRegress.qq.env == T) {
        confidence.level <- 0.95
        n <- length(fit$residuals)
        P <- ppoints(n)
        z <- qnorm(P)
        
        zz <- qnorm(1 - (1 - confidence.level) / 2)
        SE <- (slope / dnorm(z)) * sqrt(P * (1 - P) / n)
        fit.vals <- int + slope * z
        dat$z <- z
        dat$lower <- fit.vals - zz * SE
        dat$upper <- fit.vals + zz * SE
      }

      # Normal QQ plot
      
      if (input$linRegress.qq.env == T) {
        plots[[i]] <- ggplot(data = dat, aes(x = z, y = Res)) +
                        ggtitle(title.name) +
                        xlab("Normal Quantiles") +
                        ylab("Ordered Residuals") +
                        geom_point() +
                        geom_abline(slope = slope, intercept = int) +
                        geom_ribbon(aes(ymin = lower, ymax = upper),
                                    alpha = 0.2)
      } else {
        plots[[i]] <- ggplot(data = dat, aes(sample = Res)) +
                        ggtitle(title.name) +
                        xlab("Normal Quantiles") +
                        ylab("Ordered Residuals") +
                        geom_qq() +
                        geom_abline(slope = slope, intercept = int)
      }
    }
    
    # Standardized residuals vs. robust distances
    if (input$linRegress.resid.dist == T) {
      i <- i + 1
      
      title.name <- ifelse(any(class(fit) == "lm"), values$linRegress.models[1], paste("Robust", values$linRegress.models[1]))
      
      if (any(class(fit) == "lm")) {
        st.residuals <- rstandard(fit)
        
        tr <- terms(fit)
        
        mat <- model.matrix(fit)
        
        if(attr(tr, "intercept") == 1) {
          mat <- mat[, -1, drop = F]
        }
        
        MD <- sqrt(mahalanobis(x      = mat,
                               center = colMeans(mat),
                               cov    = var(mat)))
        
        chi <- sqrt(qchisq(p = 1 - 0.025, df = fit$rank))
              
        dat <- data.frame(X = MD, Y = st.residuals)

        plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                         ggtitle(title.name) +
                         xlab("Distances") +
                         ylab("Standardized Residuals") +
                         geom_point() +
                         geom_hline(yintercept = c(-2.5, 0, 2.5),
                                    linetype = 2) +
                         geom_vline(xintercept = chi,
                                    linetype = 2)
      } else {
        st.residuals <- fit$residuals / fit$scale
        
        MD <- fit$MD
        
        chi <- sqrt(qchisq(p = 1 - 0.025, df = fit$rank))
              
        dat <- data.frame(X = MD, Y = st.residuals)

        plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                         ggtitle(title.name) +
                         xlab("Robust Distances") +
                         ylab("Robustly Standardized Residuals") +
                         geom_point() +
                         geom_hline(yintercept = c(-2.5, 0, 2.5),
                                    linetype = 2) +
                         geom_vline(xintercept = chi,
                                    linetype = 2)
      }
    }
    
    # Estimated residual density
    if (input$linRegress.residual.density == T) {
      i <- i + 1
      
      dat <- data.frame(Res = fit$residuals)
      
      title.name <- ifelse(any(class(fit) == "lm"), values$linRegress.models[1], paste("Robust", values$linRegress.models[1]))
      
      plots[[i]] <- ggplot(data = dat) +
                      ggtitle(title.name) +
                      xlab("Residuals") +
                      ylab("Density") +
                      geom_histogram(aes(x = Res, y = ..density..),
                                     fill  = 'white',
                                     color = 'black',
                                     bins  = 35) +
                      geom_density(aes(x = Res),
                                   color = 'blue',
                                   fill  = 'blue',
                                   alpha = 0.1)
      
      if (input$include.rugplot == T) {
        plots[[i]] <- plots[[i]] + geom_rug()
      }
    }
    
    # Standardized residuals vs. index values
    if (input$linRegress.resid.index == T) {
      i <- i + 1
      
      title.name <- ifelse(any(class(fit) == "lm"), values$linRegress.models[1], paste("Robust", values$linRegress.models[1]))
      
      if (any(class(values$dat) == "zoo")) {
        indx <- index(values$dat)
      } else {
        indx <- 1:nrow(values$dat)
      }
      
      if (any(class(fit) == "lm")) {
        st.residuals <- rstandard(fit)
        
        dat <- data.frame(X = indx, Y = st.residuals)
      
        plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                        ggtitle(title.name) +
                        ylab("Standardized Residuals") +
                        geom_point() +
                        geom_line()
      } else {
        st.residuals <- fit$residuals / fit$scale
        
        dat <- data.frame(X = indx, Y = st.residuals)
      
        plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                        ggtitle(title.name) +
                        ylab("Robustly Standardized Residuals") +
                        geom_point() +
                        geom_line()
      }
    }
    
    ## 2 Regression Case
    
    if (values$linRegress.num.fits == 2) {
      j <- 0
      
      fit2 <- values$linRegress.fit2
      
      # Residual v. fitted values
      if (input$linRegress.residual.fit == T) {
        j <- j + 1
        
        fit.vals <- fitted(fit2)
        
        dat <- data.frame(X = fit.vals, Y = fit2$residuals)
        
        title.name <- ifelse(any(class(fit2) == "lm"), values$linRegress.models[2], paste("Robust", values$linRegress.models[2]))
        
        sigma <- 1
        
        if (any(class(fit2) == "lm")) {
          sigma <- sd(fit2$residuals)
        } else {
          sigma <- fit2$scale
        }
        
        plt <- ggplot(data = dat, aes(x = X, y = Y)) +
                 ggtitle(title.name) +
                 xlab("Fitted Values") +
                 ylab("Residuals") +
                 geom_point() +
                 geom_hline(yintercept = c(-2.5 * sigma, 0, 2.5 * sigma),
                            linetype = 2)
        
        if (input$include.rugplot == T) {
          plt <- plt + geom_rug()
        }
        
        x.min <- min(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)
        x.max <- max(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)

        y.min <- min(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        y.max <- max(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        
        plots[[j]] <- plots[[j]] + scale_y_continuous(limits = c(y.min, y.max)) +
                        scale_x_continuous(limits = c(x.min, x.max))
        plt <- plt + scale_y_continuous(limits = c(y.min, y.max)) +
                 scale_x_continuous(limits = c(x.min, x.max))
        
        plots[[j]] <- cbind(ggplotGrob(plots[[j]]), ggplotGrob(plt), size = "last")
      }
      
      # Response v. fitted values
      if (input$linRegress.response.fit == T) {
        j <- j + 1
  
        fit.vals <- fitted(fit2)
  
        dat <- data.frame(X = fit.vals, Y = fit2$model[, 1])
        
        title.name <- ifelse(any(class(fit2) == "lm"), values$linRegress.models[2], paste("Robust", values$linRegress.models[2]))
  
        plt <- ggplot(data = dat, aes(x = X, y = Y)) +
                 ggtitle(title.name) +
                 xlab("Fitted Values") +
                 ylab("Response") +
                 geom_point()
  
        if (input$include.rugplot == T) {
          plt <- plt + geom_rug()
        }
        
        x.min <- min(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)
        x.max <- max(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)

        y.min <- min(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        y.max <- max(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        
        plots[[j]]  <- plots[[j]] + scale_y_continuous(limits = c(y.min, y.max),
                                                       expand = expand_scale(mult = c(0.05, 0.05))) +
                         scale_x_continuous(limits = c(x.min, x.max))
        
        plt <- plt + scale_y_continuous(limits = c(y.min, y.max),
                                        expand = expand_scale(mult = c(0.05, 0.05))) +
                 scale_x_continuous(limits = c(x.min, x.max))
        
        plots[[j]] <- cbind(ggplotGrob(plots[[j]]), ggplotGrob(plt), size = "last")
      }
  
      # QQ Plot
      if (input$linRegress.qq == T) {
        j <- j + 1
  
        dat <- data.frame(Res = sort(fit2$residuals))
        
        title.name <- ifelse(any(class(fit2) == "lm"), values$linRegress.models[2], paste("Robust", values$linRegress.models[2]))
  
        # Calculate slope and intercept for qqline 
        y     <- quantile(fit2$residuals, c(0.25, 0.75), type=5)
        x     <- qnorm(c(0.25, 0.75))
        slope <- diff(y) / diff(x)
        int   <- y[1] - slope * x[1]
        
        if (input$linRegress.qq.env == T) {
          confidence.level <- 0.95
          n <- length(fit2$residuals)
          P <- ppoints(n)
          z <- qnorm(P)
          
          zz <- qnorm(1 - (1 - confidence.level) / 2)
          SE <- (slope / dnorm(z)) * sqrt(P * (1 - P) / n)
          fit.vals <- int + slope * z
          dat$z <- z
          dat$lower <- fit.vals - zz * SE
          dat$upper <- fit.vals + zz * SE
          
          plt <- ggplot(data = dat, aes(x = z, y = Res)) +
                   ggtitle(title.name) +
                   xlab("Normal Quantiles") +
                   ylab("Ordered Residuals") +
                   geom_point() +
                   geom_abline(slope = slope, intercept = int) +
                   geom_ribbon(aes(ymin = lower, ymax = upper),
                               alpha = 0.2)
        } else {
          plt <- ggplot(data = dat, aes(sample = Res)) +
                   ggtitle(title.name) +
                   xlab("Normal Quantiles") +
                   ylab("Ordered Residuals") +
                   geom_qq() +
                   geom_abline(slope = slope, intercept = int)
        }
        
        x.min <- min(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)
        x.max <- max(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)

        y.min <- min(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        y.max <- max(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        
        plots[[j]]  <- plots[[j]] + scale_y_continuous(limits = c(y.min, y.max),
                                                       expand = expand_scale(mult = c(0.05, 0.05))) +
                        scale_x_continuous(limits = c(x.min, x.max))
        
        plt <- plt + scale_y_continuous(limits = c(y.min, y.max),
                                        expand = expand_scale(mult = c(0.05, 0.05))) +
                 scale_x_continuous(limits = c(x.min, x.max))
        
        plots[[j]] <- cbind(ggplotGrob(plots[[j]]), ggplotGrob(plt), size = "last")
      }
      
      # Standardized residuals vs. robust distances
      if (input$linRegress.resid.dist == T) {
        j <- j + 1
        
        title.name <- ifelse(any(class(fit2) == "lm"), values$linRegress.models[2], paste("Robust", values$linRegress.models[2]))
        
        if (any(class(fit2) == "lm")) {
          st.residuals <- rstandard(fit2)
          
          tr <- terms(fit2)
        
          mat <- model.matrix(fit2)
          
          if(attr(tr, "intercept") == 1) {
            mat <- mat[, -1, drop = F]
          }
          
          MD <- sqrt(mahalanobis(x      = mat,
                                 center = colMeans(mat),
                                 cov    = var(mat)))
          
          chi <- sqrt(qchisq(p = 1 - 0.025, df = fit2$rank))
                
          dat <- data.frame(X = MD, Y = st.residuals)
  
          plt <- ggplot(data = dat, aes(x = X, y = Y)) +
                   ggtitle(title.name) +
                   xlab("Distances") +
                   ylab("Standardized Residuals") +
                   geom_point() +
                   geom_hline(yintercept = c(-2.5, 0, 2.5),
                              linetype = 2) + 
                   geom_vline(xintercept = chi,
                              linetype = 2)
        } else {
          st.residuals <- fit2$residuals / fit2$scale
          
          MD <- fit2$MD
          
          chi <- sqrt(qchisq(p = 1 - 0.025, df = fit2$rank))
                
          dat <- data.frame(X = MD, Y = st.residuals)
  
          plt <- ggplot(data = dat, aes(x = X, y = Y)) +
                   ggtitle(title.name) +
                   xlab("Robust Distances") +
                   ylab("Robustly Standardized Residuals") +
                   geom_point() +
                   geom_hline(yintercept = c(-2.5, 0, 2.5),
                              linetype = 2) + 
                   geom_vline(xintercept = chi,
                              linetype = 2)
        }
        
        x.min <- min(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)
        x.max <- max(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)

        y.min <- min(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        y.max <- max(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        
        plots[[j]] <- plots[[j]] + scale_y_continuous(limits = c(y.min, y.max),
                                                       expand = expand_scale(mult = c(0.05, 0.05))) +
                        scale_x_continuous(limits = c(x.min, x.max))
        
        plt <- plt + scale_y_continuous(limits = c(y.min, y.max),
                                        expand = expand_scale(mult = c(0.05, 0.05))) +
                 scale_x_continuous(limits = c(x.min, x.max))
        
        plots[[j]] <- cbind(ggplotGrob(plots[[j]]), ggplotGrob(plt), size = "last")
      }
      
      # Estimated residual density
      if (input$linRegress.residual.density == T) {
        j <- j + 1
        
        dat <- data.frame(Res = fit2$residuals)
        
        title.name <- ifelse(any(class(fit2) == "lm"), values$linRegress.models[2], paste("Robust", values$linRegress.models[2]))
        
        plt <- ggplot(data = dat) +
                 ggtitle(title.name) +
                 xlab("Residuals") +
                 ylab("Density") +
                 geom_histogram(aes(x = Res, y = ..density..),
                                fill  = 'white',
                                color = 'black',
                                bins  = 35) +
                 geom_density(aes(x = Res),
                              color = 'blue',
                              fill  = 'blue',
                              alpha = 0.1)
        
        if (input$include.rugplot == T) {
          plt <- plt + geom_rug()
        }
        
        x.min <- min(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)
        x.max <- max(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)

        y.min <- min(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        y.max <- max(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        
        plots[[j]]  <- plots[[j]] + scale_y_continuous(limits = c(y.min, y.max),
                                                       expand = expand_scale(mult = c(0.01, 0.05))) +
                        scale_x_continuous(limits = c(x.min, x.max))
        
        plt <- plt + scale_y_continuous(limits = c(y.min, y.max),
                                        expand = expand_scale(mult = c(0.01, 0.05))) +
                 scale_x_continuous(limits = c(x.min, x.max))
        
        plots[[j]] <- cbind(ggplotGrob(plots[[j]]), ggplotGrob(plt), size = "last")
      }
      
      # Standardized residuals vs. index values
      if (input$linRegress.resid.index == T) {
        j <- j + 1
        
        title.name <- ifelse(any(class(fit2) == "lm"), values$linRegress.models[2], paste("Robust", values$linRegress.models[2]))
        
        indx <- NULL
        
        if (any(class(values$dat) == "zoo")) {
          indx <- index(values$dat)
        } else {
          indx <- 1:nrow(values$dat)
        }
        
        if (any(class(fit2) == "lm")) {
          st.residuals <- rstandard(fit2)
          
          dat <- data.frame(X = indx, Y = st.residuals)
        
          plt <- ggplot(data = dat, aes(x = X, y = Y)) + 
                   ggtitle(title.name) +
                   ylab("Standardized Residuals") +
                   geom_point() +
                   geom_line()
        } else {
          st.residuals <- fit2$residuals / fit2$scale
          
          dat <- data.frame(X = indx, Y = st.residuals)
        
          plt <- ggplot(data = dat, aes(x = X, y = Y)) + 
                   ggtitle(title.name) +
                   ylab("Robustly Standardized Residuals") +
                   geom_point() +
                   geom_line()
        }
        
        # x.min <- min(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)
        # x.max <- max(layer_scales(plots[[j]])$x$range$range, layer_scales(plt)$x$range$range)

        y.min <- min(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        y.max <- max(layer_scales(plots[[j]])$y$range$range, layer_scales(plt)$y$range$range)
        
        plots[[j]]  <- plots[[j]] + scale_y_continuous(limits = c(y.min, y.max),
                                                       expand = expand_scale(mult = c(0.05, 0.05)))
                        # scale_x_continuous(limits = c(x.min, x.max),
                        #                               expand = c(0, 0))
        
        plt <- plt + scale_y_continuous(limits = c(y.min, y.max),
                                        expand = expand_scale(mult = c(0.05, 0.05)))
                        # scale_x_continuous(limits = c(x.min, x.max),
                        #                               expand = c(0, 0))
        
        plots[[j]] <- cbind(ggplotGrob(plots[[j]]), ggplotGrob(plt), size = "last")
      }
      
      if (input$linRegress.overlaid.scatter == T) {
        j <- j + 1
        mat <- as.data.frame(fit$model)
        
        names <- colnames(mat)
        
        colnames(mat) <- c("Y", "X")
        
        int1 <- fit$coefficients[1]
        int2 <- fit2$coefficients[1]
        
        slope1 <- fit$coefficients[2]
        slope2 <- fit2$coefficients[2]
        
        title.name <- input$linRegress.formula.text
        
        plt <- ggplot(data = mat, aes(x = X, y = Y)) +
                 ggtitle(title.name) +
                 xlab(names[2]) +
                 ylab(names[1]) +
                 geom_point() +
                 geom_abline(aes(slope = slope1, intercept = int1, color = "black")) +
                 geom_abline(aes(slope = slope2, intercept = int2, color = "blue")) +
                 scale_color_manual("Method",
                                    values = c("black", "blue"))
        
        plots[[j]] <- ggplotGrob(plt)
      }
      
      if (j == 0) {
        output$linRegress.plot.ui <- renderUI({
          fluidPage(
            fluidRow(verbatimTextOutput("no.selection"))
          )
        })
      } else {
        values$linRegress.plots        <- plots
        values$linRegress.num.plots    <- j
        values$linRegress.active.index <- 1
        values$linRegress.active.plot  <- plots[[1]]
        
        if (i > 1) {
          output$linRegress.plot.ui <- renderUI({
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          })
        } else {
          output$linRegress.plot.ui <- renderUI({
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              )
            )
          })
        }
        
        output$linRegress.plot.output <- renderPlot({
          grid.draw(values$linRegress.active.plot)
        })
      }
    } else {
      if (i == 0) {
        output$linRegress.plot.ui <- renderUI({
          fluidPage(verbatimTextOutput("no.selection"))
        })
      } else {
        values$linRegress.plots        <- plots
        values$linRegress.num.plots    <- i
        values$linRegress.active.index <- 1
        values$linRegress.active.plot  <- plots[[1]]
        
        if (i > 1) {
          output$linRegress.plot.ui <- renderUI({
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          })
        } else {
          output$linRegress.plot.ui <- renderUI({
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              )
            )
          })
        }
        
        output$linRegress.plot.output <- renderPlot({
          values$linRegress.active.plot
        })
      }
    }
  })
  
  output$no.selection <- renderPrint({
    cat("No plots selected.")
  })
  
  # On button press, show next plot(s)
  observeEvent(input$linRegress.next.plot, {
    if (values$linRegress.num.plots > 0) {
      if (values$linRegress.num.fits == 2) {
        values$linRegress.active.index <- values$linRegress.active.index %% values$linRegress.num.plots + 1
        values$linRegress.active.plot  <- values$linRegress.plots[[values$linRegress.active.index]]
        
        output$linRegress.plot.ui <- renderUI({
          if (values$linRegress.active.index == values$linRegress.num.plots) {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("linRegress.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("linRegress.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
        
        output$linRegress.plot.output <- renderPlot({
          grid.draw(values$linRegress.active.plot)
        })
      } else {
        
        values$linRegress.active.index <- values$linRegress.active.index %% values$linRegress.num.plots + 1
      
        values$linRegress.active.plot <- values$linRegress.plots[[values$linRegress.active.index]]
        
        output$linRegress.plot.ui <- renderUI({
          if (values$linRegress.active.index == values$linRegress.num.plots) {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("linRegress.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("linRegress.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
      
        output$linRegress.plot.output <- renderPlot({
          values$linRegress.active.plot
        })
      }
    }
  })
  
  # On button press, move to previous plot(s)
  observeEvent(input$linRegress.prev.plot, {
    if (values$linRegress.num.plots > 0) {
      if (values$linRegress.num.fits == 2) {
        values$linRegress.active.index <- values$linRegress.num.plots + (values$linRegress.active.index - 1) %% (-values$linRegress.num.plots)
        values$linRegress.active.plot  <- values$linRegress.plots[[values$linRegress.active.index]]
        
        output$linRegress.plot.ui <- renderUI({
          if (values$linRegress.active.index == 1) {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("linRegress.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
        
        output$linRegress.plot.output <- renderPlot({
          grid.draw(values$linRegress.active.plot)
        })
      } else {
        values$linRegress.active.index <- values$linRegress.num.plots + (values$linRegress.active.index - 1) %% (-values$linRegress.num.plots)
      
        values$linRegress.active.plot <- values$linRegress.plots[[values$linRegress.active.index]]
        
        output$linRegress.plot.ui <- renderUI({
          if (values$linRegress.active.index == 1) {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("linRegress.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("linRegress.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("linRegress.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
      
        output$linRegress.plot.output <- renderPlot({
          values$linRegress.active.plot
        })
      }
    }
  })
  
  output$covariance.select.variables <- renderUI({
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    selectInput("covariance.variables", "variables",
                choices  = values$dat.numeric.variables,
                selected = values$dat.numeric.variables,
                multiple = TRUE)
  })
  
  observeEvent(input$covariance.display, {
    if (is.null(dim(values$dat))) {
      output$covariance.results <- renderPrint({
        cat("ERROR: No Data loaded")
      })
    } else if (is.null(input$covariance.variables)) {
      output$covariance.results <- renderPrint({
        cat("ERROR: Missing variables")
      })
    } 
    
    corr <- FALSE
    
    if (input$covariance.type == "corr") {
      corr <- TRUE
    }
    
    if (input$covariance.method == "classic") {
      values$covariance.fit <- covClassic(values$dat.numeric[, input$covariance.variables],
                                          data.name = input$dataset,
                                          corr = corr)
    } else if (input$covariance.method == "rob") {
      if (input$covariance.estimator == "MM") {
        values$covariance.fit <- covRobMM.temp(values$dat.numeric[, input$covariance.variables],
                                               data.name = input$dataset,
                                               corr = corr)
      } else if (input$covariance.estimator == "Rocke") {
        values$covariance.fit <- covRobRocke.temp(values$dat.numeric[, input$covariance.variables],
                                                  data.name = input$dataset,
                                                  corr = corr)
      } else {
        values$covariance.fit <- covRob(values$dat.numeric,
                                        data.name = input$dataset,
                                        corr = corr)
      }
    } else {
      if (input$covariance.estimator == "MM") {
        values$covariance.fit <- fit.models(c(Classic = "covClassic", Robust = "covRobMM"),
                                            data = values$dat.numeric[, input$covariance.variables],
                                            data.name = input$dataset,
                                            corr = corr)
      } else if (input$covariance.estimator == "Rocke") {
        values$covariance.fit <- fit.models(c(Classic = "covClassic", Robust = "covRobRocke"),
                                            data = values$dat.numeric[, input$covariance.variables],
                                            data.name = input$dataset,
                                            corr = corr)
      } else {
        values$covariance.fit <- fit.models(c(Classic = "covClassic", Robust = "covRob"),
                                            data = values$dat.numeric[, input$covariance.variables],
                                            data.name = input$dataset,
                                            corr = corr)
      }
    }
    
    output$covariance.results <- renderPrint({
      print(summary(values$covariance.fit))
    })
  })
  
  observeEvent(input$covariance.display.plots, {
    values$covariance.plots.active <- TRUE
    
    i <- 0
    if (input$covariance.method == "Both") {
      fm <- values$covariance.fit
      
      if (input$covariance.eigen == T) {
        i <- i + 1
        
        eigen.vec1 <- eigen(fm[[1]]$cov)
        eigen.vec2 <- eigen(fm[[2]]$cov)
      }
      
      if (input$covariance.mahalanobis == T) {
        
      }
      
      if (input$covariance.ellipses.matrix == T) {
        
      }
      
      if (input$covariance.image.display == T) {
        
      }
      
      if (input$covariance.dist.dist == T) {
        
      }
      
      
    } else {
      if (input$covariance.eigen == T) {
        
      }
      
      if (input$covariance.mahalanobis == T) {
        
      }
      
      if (input$covariance.ellipses.matrix == T) {
        
      }
      
      if (input$covariance.image.display == T) {
        
      }
    }
  })
  
  observeEvent(input$covariance.next.plot, {
    if (values$covariance.num.plots > 0) {
      if (input$covariance.method == "Both") {
        values$covariance.active.index <- values$covariance.active.index %% values$covariance.num.plots + 1
        values$covariance.active.plot  <- values$covariance.plots[[values$covariance.active.index]]
        
        output$covariance.plot.ui <- renderUI({
          if (values$covariance.active.index == values$covariance.num.plots) {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("covariance.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("covariance.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("covariance.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
        
        output$covariance.plot.output <- renderPlot({
          grid.draw(values$covariance.active.plot)
        })
      } else {
        
        values$covariance.active.index <- values$covariance.active.index %% values$covariance.num.plots + 1
      
        values$covariance.active.plot <- values$covariance.plots[[values$covariance.active.index]]
        
        output$covariance.plot.ui <- renderUI({
          if (values$covariance.active.index == values$covariance.num.plots) {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("covariance.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("covariance.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("covariance.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
      
        output$covariance.plot.output <- renderPlot({
          values$covariance.active.plot
        })
      }
    }
  })
  
  # On button press, move to previous plot(s)
  observeEvent(input$covariance.prev.plot, {
    if (values$covariance.num.plots > 0) {
      if (input$covariance.method == "Both") {
        values$covariance.active.index <- values$covariance.num.plots + (values$covariance.active.index - 1) %% (-values$covariance.num.plots)
        values$covariance.active.plot  <- values$covariance.plots[[values$covariance.active.index]]
        
        output$covariance.plot.ui <- renderUI({
          if (values$covariance.active.index == 1) {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("covariance.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("covariance.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("covariance.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
        
        output$covariance.plot.output <- renderPlot({
          grid.draw(values$covariance.active.plot)
        })
      } else {
        values$covariance.active.index <- values$covariance.num.plots + (values$covariance.active.index - 1) %% (-values$covariance.num.plots)
      
        values$covariance.active.plot <- values$covariance.plots[[values$covariance.active.index]]
        
        output$covariance.plot.ui <- renderUI({
          if (values$covariance.active.index == 1) {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("covariance.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              wellPanel(
                plotOutput("covariance.plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 1,
                       actionButton("covariance.prev.plot",
                                    "",
                                    icon = icon("angle-left", "fa-2x"))
                ),
                
                column(1,
                       offset = 8,
                       actionButton("covariance.next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          }
        })
      
        output$covariance.plot.output <- renderPlot({
          values$covariance.active.plot
        })
      }
    }
  })
  
  output$pca.select.variables <- renderUI({
    if (is.null(dim(values$dat))) {
      return("")
    }
    
    selectInput("pca.variables", "variables",
                choices  = values$dat.numeric.variables,
                selected = values$dat.numeric.variables,
                multiple = TRUE)
  })
  
  # Reset all windows once new data set is loaded
  observeEvent(input$display.table, {
    if (values$regress.active) {
      output$linRegress.results <- renderPrint({ invisible() })
      
      values$regress.active <- F
    }
    
    if (values$linRegress.plots.active) {
      output$linRegress.plot.ui <- renderUI({ invisible() })
      
      values$linRegress.plots.active <- F
    }
    
    updateTabsetPanel(session  = getDefaultReactiveDomain(), "linear.tabs",
                      selected = "linear.model")
  })
  
  # Reset plotting window for linear regression
  observeEvent(input$linRegress.display, {
    if (values$linRegress.plots.active) {
      output$linRegress.plot.ui <- renderUI({ invisible() })
      
      values$linRegress.plots.active <- F
    }
  })
})