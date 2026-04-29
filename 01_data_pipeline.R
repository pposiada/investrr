# 01_data_pipeline.R
# Systematic Financial Data Acquisition and Transformation

library(tidyverse)
library(tidyquant)
library(timetk)

# Define our portfolio of assets
tickers <- c("AAPL", "MSFT", "GOOG", "AMZN")

# 1. Data Acquisition
# We use tq_get as the one-stop-shop wrapper, relying on Yahoo Finance
cat("Fetching daily stock prices from Yahoo Finance...\n")
portfolio_data <- tq_get(tickers,
                         get  = "stock.prices",
                         from = "2015-01-01",
                         to   = "2023-12-31")

# 2. Tidy Transformation: Calculating Monthly Returns
# We group by symbol and use tq_transmute to aggregate from daily to monthly periodicity
# and calculate the monthly return using the adjusted price
cat("Calculating monthly returns...\n")
portfolio_monthly_returns <- portfolio_data %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "monthly.returns")

# 3. Conversion to xts
# Many econometric and optimization packages require wide xts objects
# We use tk_xts from the timetk package to pivot our tidy data to an xts object
cat("Converting to wide xts format for modeling...\n")
portfolio_returns_xts <- portfolio_monthly_returns %>%
  pivot_wider(names_from = symbol, values_from = monthly.returns) %>%
  tk_xts(date_var = date)

# Let's preview the data
print(head(portfolio_returns_xts))

# Save the xts object for subsequent scripts
saveRDS(portfolio_returns_xts, "portfolio_returns_xts.rds")
cat("Data pipeline complete. Output saved to portfolio_returns_xts.rds\n")
