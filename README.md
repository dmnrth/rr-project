# Reproducible Research - Final Project

Final project for the Reproducible Research course in 2023/2024 spring semester, group members:

-   Katarzyna Dybcio
-   Quoc An Frejter
-   Kamil Kashif
-   Damian Ślusarczyk

## :beginner: About

We based our research on the article Applying Hybrid ARIMA-SGARCH in Algorithmic Investment Strategies on S&P500 Index by Nguyen Vo and Robert Slepaczuk.

Our study aims to evaluate and compare the effectiveness of the ARIMA model and its hybrid combinations with GARCH family models (specifically SGARCH and EGARCH) in forecasting the log returns of the S&P500 index. The research utilizes daily data from Yahoo Finance, spanning from January 1, 2000, to December 31, 2019. Through a rolling window approach, the study investigates whether these hybrid models can better capture the time-series characteristics and offer superior predictive power compared to the standalone ARIMA model. The models are assessed based on their forecasting accuracy (using metrics such as MAE, MAPE, RMSE) and performance (including annualized return, annualized standard deviation, maximum drawdown, information ratio, and adjusted information ratio). The findings indicate that the hybrid models significantly outperform the ARIMA model and the Buy&Hold strategy over the long term, with results remaining robust across different window sizes, distribution types, and GARCH model variations.

## :file_folder: File structure

```         
├── functions
│   └── arima_models.R
├── input
│   └── sp500_log_returns
│   └── sp500_prices.rds
│   └── sp500_simple_returns
├── logs
│   └── arima_100
│   └── arima-egarch_ged_1000
│   └── arima-sgarch_ged_500
│   └── arima-sgarch_ged_1000
├── output
│   └── arima_base_case
│   └── arima-egarch_ged_1000
│   └── arima-garch_base_case
├── 01_download_data.qmd
├── 02_estimate_arima.qmd
├── 03_sensitivity_analysis.qmd
├── 04_backtesting.ipynb
├── 05_summary_report.qmd
└── README.md
```

| No  | File name                   | Details                                                                                                                                               |
|-----|-----------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1   | 01_download_data.qmd        | Downloading S&P500 price data, calculating descriptive statistics.                                                                                    |
| 2   | 02_estimate_arima.qmd       | Performing forecasts for the base case scenario.                                                                                                      |
| 3   | 03_sensitivity_analysis.qmd | Performing forecasts with varying GARCH models, distributions and window-length                                                                       |
| 4   | 04_backtesting.ipynb        | Calculating equity line and performance metrics for base case and sensitivity analysis.                                                               |
| 5   | 05_summary_report.qmd       | Final summary and comparison with the results presented in article Applying Hybrid ARIMA-SGARCH in Algorithmic Investment Strategies on S&P500 Index. |
