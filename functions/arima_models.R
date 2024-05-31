# Loading required packages

packages <- c("forecast", "pbmcapply", "rugarch", "xts")

for (package in packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

rm(package, packages)

# Function to find and fit the best ARIMA model

fit_best_arima <- function(df,
                           max_p = 5,
                           max_q = 5,
                           max_iterations = 500) {
  
  # Create order search space (excluding (0, 0, 0))
  
  p_orders <- 0:max_p
  q_orders <- 0:max_q
  
  orders <- expand.grid(p = p_orders, q = q_orders)
  orders <- orders[!(orders$p == 0 & orders$q == 0),]
  
  # Initialize variables for best orders and best AIC
  
  best_p <- NULL
  best_q <- NULL
  best_aic <- Inf
  
  for (i in 1:nrow(orders)) {
    p <- orders$p[i]
    q <- orders$q[i]
    
    tryCatch({
      # Fitting ARIMA(p, 0, q) model
      
      model <- Arima(df, 
                     order = c(p, 0, q),
                     method = "ML",
                     optim.control = list(maxit = max_iterations))
      
      # Update best parameters if AIC is better
      
      if (model$aic < best_aic) {
        best_p <- p
        best_q <- q
        best_aic <- model$aic
      }
    }, error = function(e) {
      # Skip this iteration in case of an error
    })
  }
  
  # Fitting the best model
  
  converged <- TRUE
  
  # In case when the model fails to converge, the converged variable is set to false
  
  tryCatch({
    best_model <- Arima(df,
                        order = c(best_p, 0, best_q),
                        method = "ML",
                        optim.control = list(maxit = max_iterations))
  }, warning = function(w) {
    converged <<- FALSE
    best_model <<- Arima(df,
                         order = c(best_p, 0, best_q),
                         method = "ML",
                         optim.control = list(maxit = max_iterations))
  })
  
  output = list(best_model, converged)
  
  return(output)
}


arima_rolling_forecast <- function(prices,
                                   estimation_start_date,
                                   estimation_end_date,
                                   estimation_window_length = 1000,
                                   log = TRUE,
                                   progress_bar = TRUE,
                                   fit_best_arima_params = list(max_p = 5,
                                                                max_q = 5,
                                                                max_iterations = 500)) {
  # Prepare log file
  
  if (log) {
    log_dir <- paste0("logs/arima_", estimation_window_length, "/")
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    log_name <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_ARIMA_",
                       estimation_window_length, "_",
                       estimation_start_date, "_", estimation_end_date, ".txt")
    path <- paste0(log_dir, log_name)
    file.create(path)
    file_con <- file(path, open = "a")
  }
  
  # Initialize variables for rolling forecast
  
  estimation_start_date <- as.Date(estimation_start_date)
  initial_t <- which(index(prices) == estimation_start_date)
  estimation_end_date <- as.Date(estimation_end_date)
  end_t <- which(index(prices) == estimation_end_date)
  forecasts <- xts()
  
  # Initialize progress bar
  
  if (progress_bar) {
    pb <- progressBar(min = initial_t, max = end_t)
  }
  
  # Perform rolling forecast
  
  for (i in initial_t:end_t) {
    
    # Prepare data
    
    window <- prices[(i - estimation_window_length):(i - 1)]
    
    # Fit best ARIMA model
    
    fit_output <- fit_best_arima(window,
                                 max_p = fit_best_arima_params$max_p,
                                 max_q = fit_best_arima_params$max_q,
                                 max_iterations = fit_best_arima_params$max_iterations)
    
    model <- fit_output[[1]]
    
    # Perform one day ahead forecast
    
    arima_forecast <- forecast(model, h = 1)
    new_row <- xts(arima_forecast$mean[1], index(prices)[i])
    forecasts <- rbind(forecasts, new_row)
    
    # Log
    
    if (log) {
      log_line <- paste0(format(Sys.time(), "[%Y-%m-%dT%H:%M:%S]"),
                         " Estimation window: ", index(prices[i - estimation_window_length]), " - ", index(prices[i - 1]),
                         ", Best order: (", arimaorder(model)[1], ", 0, ", arimaorder(model)[3],
                         "), AIC: ", round(model$aic, 2),
                         ifelse(fit_output[[2]], ", Final model converged", ", Final model FAILED to converge"))
      
      writeLines(log_line, con = file_con)
    }
    
    # Update progress bar
    
    if (progress_bar) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if (progress_bar) {close(pb)}
  if (log) {close(file_con)}
  
  return(forecasts)
}


# Function to find and fit the best ARIMA-GARCH model

fit_best_arima_garch <- function(df,
                                 max_p = 5,
                                 max_q = 5,
                                 max_iterations = 100000,
                                 g_model = "sGarch",
                                 distribution = 'ged') {

  # Create order search space (excluding (0, 0, 0))
  
  p_orders <- 0:max_p
  q_orders <- 0:max_q
  
  orders <- expand.grid(p = p_orders, q = q_orders)
  orders <- orders[!(orders$p == 0 & orders$q == 0),]
  
  # Initialize variables for best orders and best AIC
  
  best_p <- NULL
  best_q <- NULL
  best_aic <- Inf
  
  for(i in 1:nrow(orders)) {
    p <- orders$p[i]
    q <- orders$q[i]
    
    tryCatch({
      # Fitting ARIMA(p, 0, q)-GARCH(1, 1) model
      
      spec <- ugarchspec(variance.model = list(model = g_model,
                                               garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(p, q),
                                           include.mean = TRUE),
                         distribution = distribution)
  
      model <- ugarchfit(spec = spec,
                         data = df,
                         solver = "hybrid",
                         solver.control = list(maxeval = max_iterations,
                                               ftol_rel = 1e-6,
                                               xtol_rel = 1e-4))
  
      # Update best parameters if AIC is better
      
      if (infocriteria(model)[1] < best_aic) {
        best_p <- p
        best_q <- q
        best_aic <- infocriteria(model)[1]
      }
    }, error = function(e) {
      # Skip this iteration in case of an error
    })
  }
  
  # Fitting the best model
  
  converged <- TRUE
  
  best_spec <- ugarchspec(variance.model = list(model = g_model,
                                                garchOrder = c(1, 1)),
                          mean.model = list(armaOrder = c(best_p, best_q),
                                            include.mean = TRUE))
  
  # In case when the model fails to converge, the converged variable is set to false
  
  tryCatch({
    best_model <- ugarchfit(spec = best_spec,
                            data = df,
                            solver = "hybrid",
                            solver.control = list(maxit = max_iterations))
  }, warning = function(w) {
    converged <<- FALSE
    best_model <<- ugarchfit(spec = best_spec,
                             data = df,
                             solver = "hybrid",
                             solver.control = list(maxit = max_iterations))
  })
  
  output = list(best_model, converged)
  
  return(output)
}


arima_garch_rolling_forecast <- function(prices,
                                         estimation_start_date,
                                         estimation_end_date,
                                         estimation_window_length = 1000,
                                         seed = 1,
                                         log = TRUE,
                                         progress_bar = TRUE,
                                         fit_best_arima_garch_params = list(max_p = 5,
                                                                            max_q = 5,
                                                                            max_iterations = 100000),
                                         ugarchboot_params = list(method = c("Partial","Full")[1],
                                                                  n.bootpred = 100,
                                                                  n.bootfit = 500),
                                         g_model = 'sGARCH',
                                         distribution = 'ged') {
  # Prepare log file
  
  if (log) {
    log_dir <- paste0("logs/arima-", tolower(g_model), "_", tolower(distribution), "_", estimation_window_length, "/")
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    log_name <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_ARIMA-", 
                       toupper(g_model), ".", toupper(distribution), "_",
                       estimation_window_length, "_",
                       estimation_start_date, "_", estimation_end_date, ".txt")
    path <- paste0(log_dir, log_name)
    file.create(path)
    file_con <- file(path, open = "a")
  }
  
  # Initialize variables for rolling forecast
  
  estimation_start_date <- as.Date(estimation_start_date)
  initial_t <- which(index(prices) == estimation_start_date)
  estimation_end_date <- as.Date(estimation_end_date)
  end_t <- which(index(prices) == estimation_end_date)
  forecasts <- xts()
  
  # Initialize progress bar
  
  if (progress_bar) {
    pb <- progressBar(min = initial_t, max = end_t)
  }
  
  # Perform rolling forecast
  
  for (i in initial_t:end_t) {
    # Prepare data
    
    window <- prices[(i - estimation_window_length):(i - 1)]
    
    # Fit best ARIMA-GARCH model
    
    fit_output <- fit_best_arima_garch(window,
                                       max_p = fit_best_arima_garch_params$max_p,
                                       max_q = fit_best_arima_garch_params$max_q,
                                       max_iterations = fit_best_arima_garch_params$max_iterations,
                                       g_model = g_model,
                                       distribution = distribution
                                       )
    model <- fit_output[[1]]
    
    # Perform one day ahead forecast
    
    arima_garch_forecast <- ugarchboot(model,
                                       method = ugarchboot_params$method,
                                       n.ahead = 1,
                                       n.bootpred = ugarchboot_params$n.bootpred,
                                       n.bootfit = ugarchboot_params$n.bootfit,
                                       rseed = seq(seed + i, 
                                                   seed + i +
                                                     ugarchboot_params$n.bootpred +
                                                     ugarchboot_params$n.bootfit)) # seed must be a vector of length n.bootpred + n.bootfit
    new_row <- xts(arima_garch_forecast@forc@forecast$seriesFor, index(prices)[i])
    forecasts <- rbind(forecasts, new_row)
    
    # Log
    
    if (log) {
      log_line <- paste0(format(Sys.time(), "[%Y-%m-%dT%H:%M:%S]"),
                         " Estimation window: ", index(prices[i - estimation_window_length]), " - ", index(prices[i - 1]),
                         ", Best order: (", model@model$modelinc['ar'], ", 0, ", model@model$modelinc['ma'],
                         ")-(1, 1), AIC: ", infocriteria(model)[1],
                         ifelse(fit_output[[2]], ", Final model converged", ", Final model FAILED to converge"))
      
      writeLines(log_line, con = file_con)
    }
    
    # Update progress bar
    
    if (progress_bar) {
      setTxtProgressBar(pb, i)
    }
  }
  
  if (progress_bar) {close(pb)}
  if (log) {close(file_con)}
  
  return(forecasts)
}