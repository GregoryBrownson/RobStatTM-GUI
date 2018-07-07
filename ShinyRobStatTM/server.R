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
library(robustbase)
library(shiny)

theme_set(theme_bw())

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

# print.sum.lm <- function(x,
#                          digits = max(3L, getOption("digits") - 3L),
#                          symbolic.cor = x$symbolic.cor,
# 	                       signif.stars = getOption("show.signif.stars"),
#                          ...) {
#   # Custom print function for lm summary method
#   resid <- x$residuals
#   df    <- x$df
#   rdf   <- df[2L]
#   cat(if (!is.null(x$weights) && diff(range(x$weights))) "Weighted ",
#       "Residuals:\n", sep = "")
#   if (rdf > 5L) {
#     nam <- c("Min", "1Q", "Median", "3Q", "Max")
#     rq <- if (length(dim(resid)) == 2L) {
#             structure(apply(t(resid), 1L, quantile),
# 		                  dimnames = list(nam, dimnames(resid)[[2L]]))
#           } else  {
#             zz <- zapsmall(quantile(resid), digits + 1L)
#             structure(zz, names = nam)
#           }
#     print(rq, digits = digits, ...)
#   } else if (rdf > 0L) {
# 	  print(resid, digits = digits, ...)
#   } else { # rdf == 0 : perfect fit!
# 	  cat("ALL", df[1L], "residuals are 0: no residual degrees of freedom!")
#     cat("\n")
#   }
#   
#   if (length(x$aliased) == 0L) {
#     cat("\nNo Coefficients\n")
#   } else {
#     if (nsingular <- df[3L] - df[1L])
#         cat("\nCoefficients: (", nsingular,
#             " not defined because of singularities)\n", sep = "")
#     else cat("\nCoefficients:\n")
#     coefs <- x$coefficients
#     if(!is.null(aliased <- x$aliased) && any(aliased)) {
#         cn <- names(aliased)
#         coefs <- matrix(NA, length(aliased), 4, dimnames=list(cn, colnames(coefs)))
#         coefs[!aliased, ] <- x$coefficients
#     }
# 
#     printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
#                  na.print = "NA", ...)
#   }
#     ##
#   cat("\nResidual standard error:",
# 	    format(signif(x$sigma, digits)),
#       "on",
#       rdf,
#       "degrees of freedom")
#   
#   cat("\n")
#   
#   if(nzchar(mess <- naprint(x$na.action))) cat("  (",mess, ")\n", sep = "")
#   
#   if (!is.null(x$fstatistic)) {
# 	  cat("Multiple R-squared: ", formatC(x$r.squared, digits = digits))
#     
# 	  cat(",\tAdjusted R-squared: ",formatC(x$adj.r.squared, digits = digits),
#   	    "\nF-statistic:", formatC(x$fstatistic[1L], digits = digits),
#   	    "on", x$fstatistic[2L], "and",
#   	    x$fstatistic[3L], "DF,  p-value:",
#   	    format.pval(pf(x$fstatistic[1L], x$fstatistic[2L],
#                        x$fstatistic[3L], lower.tail = FALSE),
#                     digits = digits))
# 	  
#     cat("\n")
#   }
#   
#   correl <- x$correlation
#   if (!is.null(correl)) {
#     p <- NCOL(correl)
#     if (p > 1L) {
#         cat("\nCorrelation of Coefficients:\n")
#         if(is.logical(symbolic.cor) && symbolic.cor) {# NULL < 1.7.0 objects
#     	  print(symnum(correl, abbr.colnames = NULL))
#       } else {
#         correl <- format(round(correl, 2), nsmall = 2, digits = digits)
#         correl[!lower.tri(correl)] <- ""
#         print(correl[-1, -p, drop=FALSE], quote = FALSE)
#       }
#     }
#   }
#   
#   cat("\n")#- not in S
#   invisible(x)
# }
# 
# print.sum.MM <- function(fit.summary) {
#   # Custom print function for lmrobdet summary method
#   # for MM estimator
# }
# 
# print.sum.M <- function(fit.summary) {
#   # Custom print function for lmrobdet summary method
#   # for M estimator
# }
# 
# print.sum.DCML <- function(fit.summary) {
#   # Custom print function for lmrobdet summary method
#   # for DCML estimator
# }

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
  
  values$regress.methods <- c("Least Squares", "M", "MM", "Distance Constrained")
  values$regress.models  <- c("lm", "lmrobM", "lmrobdetMM", "lmrobdetDCML")

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
  
  output$robust.control <- renderUI({
    if (any(input$fit.option != "Least Squares")) {
      tabPanel("",
          tags$hr(),
          
          h4("Robust Controls"),
          
          selectInput("family.regress", "Family",
                      choices = c("Bi-square" = "bisquare",
                                  "Optimal"   = "optimal",
                                  "Modified Optimal" = "modified.optimal")),
          
          selectInput("eff.regress", "Efficiency",
                      choices = c("0.85", "0.90", "0.95")),
        
          tags$hr()
      )
    }
  })
  
  run_regression <- eventReactive(input$display.LinRegress, {
    index <- match(input$fit.option, values$regress.methods)
    
    n <- length(index)
    
    model <-sapply(index,
                    function(i, m) {
                      m[i]
                    },
                    values$regress.models)
    
    fit <- vector(mode = "list", length = length(index))
    
    control <- lmrobdet.control(efficiency = as.numeric(input$eff.regress),
                                family = input$family.regress,
                                compute.rd = T)
    if (model[1] == "lm") {
      fit[[1]] <- do.call(model[1], list(as.formula(input$formula.text), data = values$dat))
    } else {
      fit[[1]] <- do.call(model[1], list(as.formula(input$formula.text),
                                         data    = values$dat,
                                         control = control))
    }
    
    
    
    if (length(index) == 2) {
      if (model[2] == "lm") {
        fit[[2]] <- do.call(model[2], list(as.formula(input$formula.text), data = values$dat))
      } else {
        fit[[2]] <- do.call(model[2], list(as.formula(input$formula.text),
                                           data    = values$dat,
                                           control = control))
      }
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
      
      sum.fit <- summary(values$fit1)
      
      print(summary(values$fit1))
      
      values$num.fits <- length(fit)
        
      if (values$num.fits == 2) {
        values$fit2 <- fit[[2]]
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
  
  output$overlaid.plots <- renderUI({
    if (values$num.fits == 2)
    {
      tabPanel("",
        tags$hr(),
                
        h4("Overlaid Plots"),
        checkboxInput("overlaid.qq", "Residuals Normal QQ", TRUE),
        checkboxInput("overlaid.residual.density", "Estimated Residual Density", FALSE)
      )
    }
  })
  
  observeEvent(input$display.plots, {
    plots <- vector(mode = "list")
    
    i <- 0
    
    fit <- values$fit1
    
    # Residual v. fitted values
    if (input$residual.fit == T) {
      i <- i + 1
      
      fit.vals <- fitted(fit)
      
      dat <- data.frame(X = fit.vals, Y = fit$residuals)
      
      sigma <- 1
      
      if (any(class(fit) == "lm")) {
        sigma <- sd(fit$residuals)
      } else {
        sigma <- fit$scale
      }
      
      plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                      ggtitle("Residual v. Fitted Values") +
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
    if (input$response.fit == T) {
      i <- i + 1

      fit.vals <- fitted(fit)

      dat <- data.frame(X = fit.vals, Y = fit$model[, 1])

      plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                      ggtitle("Response v. Fitted Values") +
                      xlab("Fitted Values") +
                      ylab("Response") +
                      geom_point()

      if (input$include.rugplot == T) {
        plots[[i]] <- plots[[i]] + geom_rug()
      }
    }

    # QQ Plot
    if (input$qq == T) {
      i <- i + 1

      qqnorm(fit$residuals)
      qqline(fit$residuals)

      dat <- data.frame(Res = sort(fit$residuals))

      # Calculate slope and intercept for qqline
      
      y     <- quantile(fit$residuals, c(0.25, 0.75), type=5)
      x     <- qnorm(c(0.25, 0.75))
      slope <- diff(y) / diff(x)
      int   <- y[1] - slope * x[1]
      
      if (input$qq.env == T) {
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
      
      
      if (input$qq.env == T) {
        plots[[i]] <- ggplot(data = dat, aes(x = z, y = Res)) +
                        ggtitle("Residual v. Normal QQ Plot") +
                        xlab("Normal Quantiles") +
                        ylab("Residual Quantiles") +
                        geom_point() +
                        geom_abline(slope = slope, intercept = int) +
                        geom_ribbon(aes(ymin = lower, ymax = upper),
                                    alpha = 0.2)
      } else {
        plots[[i]] <- ggplot(data = dat, aes(sample = Res)) +
                        ggtitle("Residual v. Normal QQ Plot") +
                        xlab("Normal Quantiles") +
                        ylab("Residual Quantiles") +
                        geom_qq() +
                        geom_abline(slope = slope, intercept = int)
      }
    }
    
    # Standardized residuals vs. robust distances
    if (input$stdResidual.RobustDist == T) {
      i <- i + 1
      
      if (any(class(fit) == "lm")) {
        st.residuals <- rstandard(fit)
      } else {
        st.residuals <- fit$residuals / fit$scale
      }
      chi <- sqrt(qchisq(p = 1 - 0.025, df = fit$rank))
      
      MD <- robMD(x         = model.frame(fit, fit$model),
                  intercept = attr(fit$terms, "intercept"),
                  wqr       = fit$qr)
              
      dat <- data.frame(X = MD, Y = st.residuals)

      plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                      ggtitle("Standardized Residuals v. Robust Distances") +
                      xlab("Robust Distances") +
                      ylab("Robust Standardized Residuals") +
                      geom_point() +
                      geom_hline(yintercept = c(-2.5, 0, 2.5),
                                 linetype = 2) + 
                      geom_vline(xintercept = chi,
                                 linetype = 2)
    }
    
    # Estimated residual density
    if (input$residual.density == T) {
      i <- i + 1
      
      dat <- data.frame(Res = fit$residuals)
      
      plots[[i]] <- ggplot(data = dat) +
                      ggtitle("Estimated Residual Density") +
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
    if (input$stdResidual.Index == T) {
      i <- i + 1
      
      if (any(class(fit) == "lm")) {
        st.residuals <- rstandard(fit)
      } else {
        st.residuals <- fit$residuals / fit$scale
      }
      
      dat <- data.frame(X = 1:length(st.residuals), Y = st.residuals)
      
      plots[[i]] <- ggplot(data = dat, aes(x = X, y = Y)) + 
                      ggtitle("Standardized Residuals v. Index") +
                      xlab("Index") +
                      ylab("Standardized Residuals") +
                      geom_point()
    }
    
    ## 2 Regression Case
    
    if (values$num.fits == 2) {
      plots2 <- vector(mode = "list")
    
      j <- 0
      
      fit2 <- values$fit2
      
      # Residual v. fitted values
      if (input$residual.fit == T) {
        j <- j + 1
        
        fit.vals <- fitted(fit2)
        
        dat <- data.frame(X = fit.vals, Y = fit$residuals)
        
        sigma <- 1
        
        if (any(class(fit2) == "lm")) {
          sigma <- sd(fit2$residuals)
        } else {
          sigma <- fit2$scale
        }
        
        plots2[[j]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                         ggtitle("Residual v. Fitted Values") +
                         xlab("Fitted Values") +
                         ylab("Residuals") +
                         geom_point() +
                         geom_hline(yintercept = c(-2.5 * sigma, 0, 2.5 * sigma),
                                    linetype = 2)
        
        if (input$include.rugplot == T) {
          plots2[[j]] <- plots2[[j]] + geom_rug()
        }
      }
      
      # Response v. fitted values
      if (input$response.fit == T) {
        j <- j + 1
  
        fit.vals <- fitted(fit2)
  
        dat <- data.frame(X = fit.vals, Y = fit2$model[, 1])
  
        plots2[[j]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                         ggtitle("Response v. Fitted Values") +
                         xlab("Fitted Values") +
                         ylab("Response") +
                         geom_point()
  
        if (input$include.rugplot == T) {
          plots2[[j]] <- plots2[[j]] + geom_rug()
        }
      }
  
      # QQ Plot
      if (input$qq == T) {
        j <- j + 1
  
        qqnorm(fit2$residuals)
        qqline(fit2$residuals)
  
        dat <- data.frame(Res = sort(fit2$residuals))
  
        # Calculate slope and intercept for qqline 
        y     <- quantile(fit2$residuals, c(0.25, 0.75), type=5)
        x     <- qnorm(c(0.25, 0.75))
        slope <- diff(y) / diff(x)
        int   <- y[1] - slope * x[1]
        
        if (input$qq.env == T) {
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
          
          plots2[[j]] <- ggplot(data = dat, aes(x = z, y = Res)) +
                           ggtitle("Residual v. Normal QQ Plot") +
                           xlab("Normal Quantiles") +
                           ylab("Residual Quantiles") +
                           geom_point() +
                           geom_abline(slope = slope, intercept = int) +
                           geom_ribbon(aes(ymin = lower, ymax = upper),
                                       alpha = 0.2)
        } else {
          plots2[[j]] <- ggplot(data = dat, aes(sample = Res)) +
                           ggtitle("Residual v. Normal QQ Plot") +
                           xlab("Normal Quantiles") +
                           ylab("Residual Quantiles") +
                           geom_qq() +
                           geom_abline(slope = slope, intercept = int)
        }
      }
      
      # Standardized residuals vs. robust distances
      if (input$stdResidual.RobustDist == T) {
        j <- j + 1
        
        if (any(class(fit2) == "lm")) {
          st.residuals <- rstandard(fit2)
        } else {
          st.residuals <- fit2$residuals / fit2$scale
        }
        chi <- sqrt(qchisq(p = 1 - 0.025, df = fit2$rank))
        
        MD <- robMD(x         = model.frame(fit2, fit2$model),
                    intercept = attr(fit2$terms, "intercept"),
                    wqr       = fit2$qr)
                
        dat <- data.frame(X = MD, Y = st.residuals)
  
        plots2[[j]] <- ggplot(data = dat, aes(x = X, y = Y)) +
                         ggtitle("Standardized Residuals v. Robust Distances") +
                         xlab("Robust Distances") +
                         ylab("Robust Standardized Residuals") +
                         geom_point() +
                         geom_hline(yintercept = c(-2.5, 0, 2.5),
                                    linetype = 2) + 
                         geom_vline(xintercept = chi,
                                    linetype = 2)
      }
      
      # Estimated residual density
      if (input$residual.density == T) {
        j <- j + 1
        
        dat <- data.frame(Res = fit2$residuals)
        
        plots2[[j]] <- ggplot(data = dat) +
                         ggtitle("Estimated Residual Density") +
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
          plots2[[j]] <- plots2[[j]] + geom_rug()
        }
      }
      
      # Standardized residuals vs. index values
      if (input$stdResidual.Index == T) {
        j <- j + 1
        
        if (any(class(fit2) == "lm")) {
          st.residuals <- rstandard(fit2)
        } else {
          st.residuals <- fit2$residuals / fit2$scale
        }
        
        dat <- data.frame(X = 1:length(st.residuals), Y = st.residuals)
        
        plots2[[j]] <- ggplot(data = dat, aes(x = X, y = Y)) + 
                         ggtitle("Standardized Residuals v. Index") +
                         xlab("Index") +
                         ylab("Standardized Residuals") +
                         geom_point()
      }
      
      if (i == 0) {
        output$plot.ui <- renderUI({
          fluidPage(verbatimTextOutput("no.selection"))
          
          output$no.selection <- renderPrint({
            print("No plots selected.")
          })
        })
      } else {
        values$plots        <- plots
        values$num.plots    <- i
        values$active.index <- 1
        values$active.plot  <- plots[[1]]
        
        values$plots2        <- plots2
        values$num.plots2    <- j
        values$active.index2 <- 1
        values$active.plot2  <- plots2[[1]]
        
        if (i > 1) {
          output$plot.ui <- renderUI({
            fluidPage(
              fluidRow(
                column(6,
                  plotOutput("plot.output")
                ),
                
                column(6,
                  plotOutput("plot.output2")
                )
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          })
        } else {
          output$plot.ui <- renderUI({
            fluidPage(
              fluidRow(
                column(6,
                       plotOutput("plot.output")
                ),
                
                column(6,
                       plotOutput("plot.output2")
                )
              )
            )
          })
        }
        
        output$plot.output <- renderPlot({
          values$active.plot
        })
        
        output$plot.output2 <- renderPlot({
          values$active.plot2
        })
      }
    } else {
      if (i == 0) {
        output$plot.ui <- renderUI({
          fluidPage(verbatimTextOutput("no.selection"))
          
          output$no.selection <- renderPrint({
            print("No plots selected.")
          })
        })
      } else {
        values$plots        <- plots
        values$num.plots    <- i
        values$active.index <- 1
        values$active.plot  <- plots[[1]]
        
        if (i > 1) {
          output$plot.ui <- renderUI({
            fluidPage(
              wellPanel(
                plotOutput("plot.output")
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          })
        } else {
          output$plot.ui <- renderUI({
            fluidPage(
              wellPanel(
                plotOutput("plot.output")
              )
            )
          })
        }
        
        output$plot.output <- renderPlot({
          values$active.plot
        })
      }
    }
  })
  
  observeEvent(input$next.plot, {
    if (values$num.plots > 0) {
      if (values$num.fits == 2) {
        values$active.index <- values$active.index %% values$num.plots + 1
        values$active.plot  <- values$plots[[values$active.index]]
        
        values$active.index2 <- values$active.index2 %% values$num.plots + 1
        values$active.plot2  <- values$plots2[[values$active.index2]]
        
        output$plot.ui <- renderUI({
          if (values$active.index == values$num.plots) {
            fluidPage(
              fluidRow(
                column(6,
                       plotOutput("plot.output")
                ),
                
                column(6,
                       plotOutput("plot.output2")
                )
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
              fluidRow(
                column(6,
                       plotOutput("plot.output")
                ),
                
                column(6,
                       plotOutput("plot.output2")
                )
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
        
        output$plot.output2 <- renderPlot({
          values$active.plot2
        })
      } else {
        
        values$active.index <- values$active.index %% values$num.plots + 1
      
        values$active.plot <- values$plots[[values$active.index]]
        
        output$plot.ui <- renderUI({
          if (values$active.index == values$num.plots) {
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
    }
  })
  
  observeEvent(input$prev.plot, {
    if (values$num.plots > 0) {
      if (values$num.fits == 2) {
        values$active.index <- values$num.plots + (values$active.index - 1) %% (-values$num.plots)
        values$active.plot  <- values$plots[[values$active.index]]
        
        values$active.index2 <- values$num.plots + (values$active.index2 - 1) %% (-values$num.plots)
        values$active.plot2  <- values$plots2[[values$active.index2]]
        
        output$plot.ui <- renderUI({
          if (values$active.index == 1) {
            fluidPage(
              fluidRow(
                column(6,
                       plotOutput("plot.output")
                ),
                
                column(6,
                       plotOutput("plot.output2")
                )
              ),
              
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
                )
              )
            )
          } else {
            fluidPage(
              fluidRow(
                column(6,
                       plotOutput("plot.output")
                ),
                
                column(6,
                       plotOutput("plot.output2")
                )
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
        
        output$plot.output2 <- renderPlot({
          values$active.plot2
        })
      } else {
        values$active.index <- values$num.plots + (values$active.index - 1) %% (-values$num.plots)
      
        values$active.plot <- values$plots[[values$active.index]]
        
        output$plot.ui <- renderUI({
          if (values$active.index == 1) {
            fluidPage(
              wellPanel(
                plotOutput("plot.output")
              ),
            
              fluidRow(
                column(1,
                       offset = 10,
                       actionButton("next.plot",
                                    "",
                                    icon = icon("angle-right", "fa-2x"))
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
    }
  })
})