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
  "LMT", "BA", "GD", "RTX", "NOC", "LHX", "HII", "TXT", "LDOS", "BAH",
  "JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "STT", "PNC", "USB", "COF", "TFC",
  "XOM", "CVX", "COP", "NEE", "DUK", "SO", "D", "AEP", "EXC", "SRE", "PCG", "KMI", "WMB", "SLB", "HAL", "BKR",
  "MSFT", "AAPL", "GOOGL", "AMZN", "META", "INTC", "NVDA", "AMD", "QCOM", "TXN", "AVGO", "MU", "AMAT", "IBM", "ORCL", "CSCO", "PANW", "CRWD",
  "T", "VZ", "TMUS", "CMCSA", "CHTR",
  "UNP", "CSX", "NSC", "FDX", "UPS", "DAL", "UAL", "AAL", "LUV",
  "JNJ", "PFE", "MRK", "ABBV", "LLY", "UNH", "CVS", "ELV", "MCK", "COR", "CAH", "MRNA",
  "ADM", "BG", "DE", "CTVA", "TSN", "GIS", "K", "CF", "MOS",
  "F", "GM", "CAT", "CMI", "PCAR",
  "DOW", "DD", "NUE", "FCX"
)

# 2. Parameter Grid
rsi_params <- c(10, 14, 16, 20)
stoch_k_params <- c(5, 7, 10, 14)
stoch_d_params <- c(3, 5, 7)

param_grid <- expand.grid(
  RSI_n = rsi_params,
  Stoch_K_n = stoch_k_params,
  Stoch_D_n = stoch_d_params
)

cat(sprintf("Total parameter combinations to test: %d\n", nrow(param_grid)))

# 3. Data Wrangling
cat("Fetching long-term history (1999-2026) for 100 tickers...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = "1999-01-01", to = "2026-12-31")

cat("Calculating Indicators globally (this may take a moment)...\n")
df_global <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200)
  )

# Dynamically calculate RSI variations
for (r in rsi_params) {
  col_name <- paste0("RSI", r)
  df_global <- df_global %>% mutate(!!sym(col_name) := RSI(close, n = r))
}

# Dynamically calculate Stoch variations
for (k in stoch_k_params) {
  l_col <- paste0("L", k)
  h_col <- paste0("H", k)
  fastk_col <- paste0("fastK", k)
  
  df_global <- df_global %>% mutate(
    !!sym(l_col) := runMin(low, n = k),
    !!sym(h_col) := runMax(high, n = k),
    !!sym(fastk_col) := (close - !!sym(l_col)) / (!!sym(h_col) - !!sym(l_col))
  )
  
  df_global[[fastk_col]] <- ifelse(is.nan(df_global[[fastk_col]]) | is.infinite(df_global[[fastk_col]]), NA, df_global[[fastk_col]])
  df_global[[fastk_col]] <- zoo::na.locf(df_global[[fastk_col]], na.rm = FALSE)
  df_global[[fastk_col]] <- ifelse(is.na(df_global[[fastk_col]]), 0.5, df_global[[fastk_col]])
  
  for (d in stoch_d_params) {
    fastd_col <- paste0("fastD", k, "_", d)
    stochbull_col <- paste0("stochbull_", k, "_", d)
    
    df_global <- df_global %>% mutate(
      !!sym(fastd_col) := SMA(!!sym(fastk_col), n = d),
      !!sym(stochbull_col) := !!sym(fastk_col) > !!sym(fastd_col)
    )
  }
}

df_global <- df_global %>% ungroup()

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
run_period_backtest <- function(df_period, p_name, rsi_n, stoch_k, stoch_d) {
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
  
  rsi_col <- paste0("RSI", rsi_n)
  RSI_mat <- make_wide(rsi_col)
  
  stochbull_col <- paste0("stochbull_", stoch_k, "_", stoch_d)
  stoch_mat <- make_wide(stochbull_col)
  
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
      if (rsi[j] >= 30 && !in_rsi_zone[j]) {
        if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) recent_high[j] <- H_today[j]
      }
      if (rsi[j] < 30) {
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
      if (rsi[j] >= 30 && in_rsi_zone[j]) {
        in_rsi_zone[j] <- FALSE
        setup_armed[j] <- TRUE
        stop_loss[j] <- lowest_low[j] * 0.98
      }
      
      if (position[j] > 0) {
        exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
        if (L_today[j] <= stop_loss[j]) {
          global_cash <- global_cash + (shares[j] * exec_price_stop)
          shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
        } else {
          if (position[j] == 1 && H_today[j] >= target1[j]) {
            exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
            sell_shares <- floor(shares[j] / 2)
            global_cash <- global_cash + (sell_shares * exec_price_t1)
            shares[j] <- shares[j] - sell_shares
            position[j] <- 0.5; stop_loss[j] <- entry_price[j]
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
    final_eq <- run_period_backtest(df_period, p$name, params$RSI_n, params$Stoch_K_n, params$Stoch_D_n)
    
    all_results[[length(all_results) + 1]] <- data.frame(
      Period = p$name,
      RSI_n = params$RSI_n,
      Stoch_K_n = params$Stoch_K_n,
      Stoch_D_n = params$Stoch_D_n,
      Final_Equity = final_eq,
      stringsAsFactors = FALSE
    )
    
    if (r %% 12 == 0) cat(sprintf("  Completed %d/%d combinations for this period...\n", r, nrow(param_grid)))
  }
}

# 7. Analyze Results
results_df <- bind_rows(all_results)

summary_df <- results_df %>%
  group_by(RSI_n, Stoch_K_n, Stoch_D_n) %>%
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
write.csv(summary_df, "outputs/indicator_combinations_summary.csv", row.names = FALSE)
write.csv(results_df, "outputs/indicator_combinations_raw.csv", row.names = FALSE)
cat("Full results saved to outputs/indicator_combinations_summary.csv and _raw.csv\n")
