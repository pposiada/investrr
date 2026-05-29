# 11_rolling_5yr_indicator_combinations.R
# Advanced Reversion Strategy - 5-Year Rolling Parameter Search (2000-2025)

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyquant)
library(purrr)

# 1. Universe Definition
tickers <- c(
  # I. Financials & Insurance
  "PKO.WA", "PEO.WA", "PZU.WA", "ALR.WA", "SPL.WA", "ING.WA", "MBK.WA", "MIL.WA", "BHW.WA", "KRU.WA", "GPW.WA", "XTB.WA",
  # II. Energy, Power & Utilities
  "PGE.WA", "TPE.WA", "ENA.WA", "PEP.WA", "ZEP.WA",
  # III. Mining, Oil & Gas
  "PKN.WA", "KGH.WA", "JSW.WA", "LWB.WA",
  # IV. Chemicals, Heavy Industry & Defense
  "ATT.WA", "PCE.WA", "PUW.WA", "KTY.WA", "BRS.WA", "STP.WA", "COG.WA", "GEA.WA", "CRI.WA",
  # V. Infrastructure, Construction & Logistics
  "PKP.WA", "BDX.WA", "TOR.WA", "PXM.WA", "MRB.WA", "CAR.WA", "APR.WA",
  # VI. Telecommunications, IT & Media
  "ACP.WA", "OPL.WA", "CPS.WA", "WPL.WA", "ASB.WA",
  # VII. Food Security, Retail & Healthcare
  "DNP.WA", "ZAB.WA", "ALE.WA", "LPP.WA", "EUR.WA", "NEU.WA", "CDR.WA", "DOM.WA"
)

# 2. Parameter Grid
entry_rsi_params <- seq(10, 40, by = 5)
armed_rsi_params <- seq(30, 70, by = 5)
stop_loss_params <- seq(0.01, 0.10, by = 0.01)
sell_pct_params <- c(0.25, 0.50, 0.75, 1.00)

param_grid <- expand.grid(
  Entry_RSI = entry_rsi_params,
  Armed_RSI = armed_rsi_params,
  Stop_Loss = stop_loss_params,
  Sell_Pct = sell_pct_params
)

cat(sprintf("Total parameter combinations to test: %d\n", nrow(param_grid)))

# 3. Data Wrangling
cat("Fetching long-term history (1999-2026) for 100 tickers...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = "1999-01-01", to = "2026-12-31")

cat("Calculating Indicators globally (this may take a moment)...\n")
df_global <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  drop_na(high, low, close) %>%
  mutate(
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200),
    RSI14 = RSI(close, n = 14),
    L10 = runMin(low, n = 10),
    H10 = runMax(high, n = 10),
    fastK = (close - L10) / (H10 - L10),
    fastK = ifelse(is.nan(fastK) | is.infinite(fastK), NA, fastK),
    fastK = zoo::na.locf(fastK, na.rm = FALSE),
    fastK = ifelse(is.na(fastK), 0.5, fastK),
    fastD = SMA(fastK, n = 5),
    stoch_bull = fastK > fastD
  ) %>%
  select(-L10, -H10, -fastK, -fastD) %>%
  ungroup()

# 4. Define 5-year Periods
periods <- list(
  list(name = "2000 - 2005 (Dot-Com Bust)", start = "2000-01-01", end = "2005-12-31"),
  list(name = "2004 - 2009 (GFC)", start = "2004-01-01", end = "2009-12-31"),
  list(name = "2008 - 2013 (Recovery)", start = "2008-01-01", end = "2013-12-31"),
  list(name = "2012 - 2017 (Bull Market)", start = "2012-01-01", end = "2017-12-31"),
  list(name = "2016 - 2021 (COVID Crash)", start = "2016-01-01", end = "2021-12-31"),
  list(name = "2020 - 2025 (Modern Era)", start = "2020-01-01", end = "2025-12-31")
)

# 5. Backtester Wrapper Function
run_period_backtest <- function(df_period, p_name, entry_thresh, armed_thresh, sl_pct, sell_pct) {
  dates <- sort(unique(df_period$date))
  if(length(dates) < 100) return(NA)
  
  make_wide <- function(col_name) {
    df_period %>% select(date, symbol, all_of(col_name)) %>% 
      pivot_wider(names_from = symbol, values_from = all_of(col_name)) %>% 
      arrange(date) %>% select(-date) %>% as.matrix()
  }
  
  O_mat <- make_wide("open")
  H_mat <- make_wide("high")
  L_mat <- make_wide("low")
  C_mat <- make_wide("close")
  SMA10_mat <- make_wide("SMA10")
  SMA200_mat <- make_wide("SMA200")
  
  RSI_mat <- make_wide("RSI14")
  stoch_mat <- make_wide("stoch_bull")
  
  symbols <- colnames(C_mat)
  n_symbols <- length(symbols)
  
  in_rsi_zone <- rep(FALSE, n_symbols)
  setup_armed <- rep(FALSE, n_symbols)
  recent_high <- rep(NA_real_, n_symbols)
  swing_high  <- rep(NA_real_, n_symbols)
  lowest_low  <- rep(NA_real_, n_symbols)
  
  position    <- rep(0, n_symbols)
  entry_price <- rep(NA_real_, n_symbols)
  stop_loss   <- rep(NA_real_, n_symbols)
  target1     <- rep(NA_real_, n_symbols)
  shares      <- rep(0, n_symbols)
  
  global_cash <- 100000
  
  for (i in 1:length(dates)) {
    O_today <- O_mat[i, ]
    H_today <- H_mat[i, ]
    L_today <- L_mat[i, ]
    C_today <- C_mat[i, ]
    rsi <- RSI_mat[i, ]
    sma10 <- SMA10_mat[i, ]
    sma200 <- SMA200_mat[i, ]
    stoch_today <- stoch_mat[i, ]
    
    active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
    triggered_indices <- integer(0)
    
    for (j in active_idx) {
      if (rsi[j] >= armed_thresh && !in_rsi_zone[j]) {
        if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) recent_high[j] <- H_today[j]
      }
      if (rsi[j] < armed_thresh) {
        if (!in_rsi_zone[j]) {
          in_rsi_zone[j] <- TRUE
          swing_high[j] <- recent_high[j]
          lowest_low[j] <- L_today[j]
          setup_armed[j] <- FALSE
          recent_high[j] <- NA
        } else {
          if (L_today[j] < lowest_low[j]) lowest_low[j] <- L_today[j]
        }
      }
      if (rsi[j] >= entry_thresh && in_rsi_zone[j]) {
        in_rsi_zone[j] <- FALSE
        setup_armed[j] <- TRUE
        stop_loss[j] <- lowest_low[j] * (1 - sl_pct)
      }
      
      if (position[j] > 0) {
        exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
        if (L_today[j] <= stop_loss[j]) {
          global_cash <- global_cash + (shares[j] * exec_price_stop)
          shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
        } else {
          if (position[j] == 1 && H_today[j] >= target1[j]) {
            exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
            sell_shares <- floor(shares[j] * sell_pct)
            global_cash <- global_cash + (sell_shares * exec_price_t1)
            shares[j] <- shares[j] - sell_shares
            position[j] <- ifelse(shares[j] > 0, 0.5, 0)
            stop_loss[j] <- entry_price[j]
          }
          if (position[j] == 0.5 && H_today[j] >= sma200[j]) {
            exec_price_t2 <- ifelse(O_today[j] > sma200[j], O_today[j], sma200[j])
            global_cash <- global_cash + (shares[j] * exec_price_t2)
            shares[j] <- 0; position[j] <- 0
          }
        }
      } else if (position[j] == 0 && setup_armed[j]) {
        if (L_today[j] <= stop_loss[j]) {
          setup_armed[j] <- FALSE
        } else if (C_today[j] > sma10[j] && C_today[j] < sma200[j]) {
          risk_pct <- (C_today[j] - stop_loss[j]) / C_today[j]
          if (risk_pct <= 0.15 && !is.na(swing_high[j]) && swing_high[j] > C_today[j]) {
            if (!is.na(stoch_today[j]) && stoch_today[j]) {
              triggered_indices <- c(triggered_indices, j)
            }
          } else {
            setup_armed[j] <- FALSE
          }
        }
      }
    }
    
    if (global_cash > 0 && length(triggered_indices) > 0) {
      cash_per_trade <- global_cash / length(triggered_indices)
      for (j in triggered_indices) {
        entry_px <- C_today[j]
        alloc_shares <- floor(cash_per_trade / entry_px)
        if (alloc_shares > 0) {
          global_cash <- global_cash - (alloc_shares * entry_px)
          shares[j] <- alloc_shares
          position[j] <- 1; entry_price[j] <- entry_px; target1[j] <- swing_high[j]
          setup_armed[j] <- FALSE
        }
      }
    }
  }
  
  portfolio_value <- global_cash
  for (j in 1:n_symbols) {
    if (shares[j] > 0) {
      last_price <- C_mat[nrow(C_mat), j]
      if (!is.na(last_price)) {
         portfolio_value <- portfolio_value + (shares[j] * last_price)
      }
    }
  }
  return(portfolio_value)
}

# 6. Execute Grid Search
all_results <- list()

for (p in periods) {
  cat("\n============================================\n")
  cat("Running backtests for period:", p$name, "...\n")
  
  df_period <- df_global %>% filter(date >= as.Date(p$start) & date <= as.Date(p$end))
  
  for (r in 1:nrow(param_grid)) {
    params <- param_grid[r, ]
    final_eq <- run_period_backtest(df_period, p$name, params$Entry_RSI, params$Armed_RSI, params$Stop_Loss, params$Sell_Pct)
    
    all_results[[length(all_results) + 1]] <- data.frame(
      Period = p$name,
      Entry_RSI = params$Entry_RSI,
      Armed_RSI = params$Armed_RSI,
      Stop_Loss = params$Stop_Loss,
      Sell_Pct = params$Sell_Pct,
      Final_Equity = final_eq,
      stringsAsFactors = FALSE
    )
    
    if (r %% 100 == 0) cat(sprintf("  Completed %d/%d combinations for this period...\n", r, nrow(param_grid)))
  }
}

# 7. Analyze Results
results_df <- bind_rows(all_results)

summary_df <- results_df %>%
  group_by(Entry_RSI, Armed_RSI, Stop_Loss, Sell_Pct) %>%
  summarize(
    Avg_Final_Equity = mean(Final_Equity, na.rm = TRUE),
    Median_Final_Equity = median(Final_Equity, na.rm = TRUE),
    Win_Rate = mean(Final_Equity > 100000, na.rm = TRUE) * 100,
    Min_Final_Equity = min(Final_Equity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Avg_Final_Equity))

cat("\n============================================\n")
cat("Top 10 Indicator Combinations (by Avg Final Equity):\n")
print(head(summary_df, 10))
cat("============================================\n")

if(!dir.exists("outputs")) dir.create("outputs")
write.csv(summary_df, "outputs/indicator_combinations_summary_pl.csv", row.names = FALSE)
write.csv(results_df, "outputs/indicator_combinations_raw_pl.csv", row.names = FALSE)
cat("Full results saved to outputs/indicator_combinations_summary_pl.csv and _raw_pl.csv\n")
