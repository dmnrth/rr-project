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
  p_orders <- 0:max_p
  q_orders <- 0:max_q
  
  # Create order search space (excluding (0, 1, 0))
  
  orders <- expand.grid(p = p_orders, q = q_orders)
  orders <- orders[!(orders$p == 0 & orders$q == 0),]
  
  # Initialize variables for best orders and best AIC
  
  best_p <- NULL
  best_q <- NULL
  best_aic <- Inf
  
  for (i in 1:nrow(orders)) {
    p <- orders$p[i]
    q <- orders$q[i]
    
    # Fitting ARIMA(p, 1, q) model
    
    model <- Arima(df, 
                   order = c(p, 1, q),
                   method = "ML",
                   optim.control = list(maxit = max_iterations))
    
    # Update best parameters if AIC is better
    
    if (model$aic < best_aic) {
      best_p <- p
      best_q <- q
      best_aic <- model$aic
    }
  }
  
  # Fitting the best model
  
  best_model <- Arima(df,
                      order = c(best_p, 1, best_q),
                      method = "ML",
                      optim.control = list(maxit = max_iterations))

  return(best_model)
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
    log_name <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_ARIMA.txt")
    path <- paste0("logs/", log_name)
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
    
    model <- fit_best_arima(window,
                            max_p = fit_best_arima_params$max_p,
                            max_q = fit_best_arima_params$max_q,
                            max_iterations = fit_best_arima_params$max_iterations)
    
    # Perform one day ahead forecast
    
    arima_forecast <- forecast(model, h = 1)
    new_row <- xts(arima_forecast$mean[1], index(prices)[i])
    forecasts <- rbind(forecasts, new_row)
    
    # Log
    
    if (log) {
      log_line <- paste0(format(Sys.time(), "[%Y-%m-%dT%H:%M:%S]"),
                         " Estimation window: ", index(prices[i - estimation_window_length]), " - ", index(prices[i - 1]),
                         ", Best order: (", arimaorder(model)[1], ", 1, ", arimaorder(model)[3],
                         "), AIC: ", round(model$aic, 2))
      
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

# TODO: Add ARIMA-GARCH functions