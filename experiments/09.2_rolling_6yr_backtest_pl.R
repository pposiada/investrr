# 09_rolling_6yr_backtest.R
# Advanced Reversion Strategy - 6-Year Rolling Periods Backtest (2000-2026)

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyquant)
library(purrr)

# 1. Universe Definition
  # I. Financials & Insurance (Systemic Institutions)
  tickers <- c("PKO.WA", "PEO.WA", "PZU.WA", "ALR.WA", "SPL.WA", "ING.WA", "MBK.WA", "MIL.WA", "BHW.WA", "KRU.WA", "GPW.WA", "XTB.WA",
  # II. Energy, Power & Utilities (Critical Infrastructure)
  "PGE.WA", "TPE.WA", "ENA.WA", "PEP.WA", "ZEP.WA",
  # III. Mining, Oil & Gas (Resource Security)
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

# 2. Data Wrangling (Fetch long history)
cat("Fetching long-term history (2000-2026) for 50 Polish tickers...\n")
# Fetch starting from 1999 to give room for the 200-day SMA in year 2000
raw_data <- tq_get(tickers, get = "stock.prices", from = "1999-01-01", to = "2026-12-31")

cat("Calculating Indicators globally...\n")
df_global <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  drop_na(high, low, close) %>%
  mutate(
    SMA20 = SMA(close, n = 20),
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

# 3. Define Periods
periods <- list(
  list(name = "2000 - 2005 (Dot-Com Bust)", start = "2000-01-01", end = "2005-12-31"),
  list(name = "2004 - 2009 (GFC)", start = "2004-01-01", end = "2009-12-31"),
  list(name = "2008 - 2013 (Recovery)", start = "2008-01-01", end = "2013-12-31"),
  list(name = "2012 - 2017 (Bull Market)", start = "2012-01-01", end = "2017-12-31"),
  list(name = "2016 - 2021 (COVID Crash)", start = "2016-01-01", end = "2021-12-31"),
  list(name = "2021 - 2026 (Modern Era)", start = "2021-01-01", end = "2026-12-31")
)

all_results <- list()

# 4. Backtester Wrapper Function
run_period_backtest <- function(p_name, p_start, p_end) {
  df <- df_global %>% filter(date >= as.Date(p_start) & date <= as.Date(p_end))
  dates <- sort(unique(df$date))
  
  if(length(dates) < 100) return(NULL) # Skip if barely any data
  
  make_wide <- function(col_name) {
    df %>% select(date, symbol, all_of(col_name)) %>% 
      pivot_wider(names_from = symbol, values_from = all_of(col_name)) %>% 
      arrange(date) %>% select(-date) %>% as.matrix()
  }
  
  O_mat <- make_wide("open")
  H_mat <- make_wide("high")
  L_mat <- make_wide("low")
  C_mat <- make_wide("close")
  SMA20_mat <- make_wide("SMA20")
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
  equity_curve <- numeric(length(dates))
  
  # Start at day 1 (because SMA200 is already pre-calculated from 1999!)
  for (i in 1:length(dates)) {
    O_today <- O_mat[i, ]
    H_today <- H_mat[i, ]
    L_today <- L_mat[i, ]
    C_today <- C_mat[i, ]
    rsi <- RSI_mat[i, ]
    sma20 <- SMA20_mat[i, ]
    sma200 <- SMA200_mat[i, ]
    stoch_today <- stoch_mat[i, ]
    
    active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
    triggered_indices <- integer(0)
    
    for (j in active_idx) {
      if (rsi[j] >= 50 && !in_rsi_zone[j]) {
        if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) recent_high[j] <- H_today[j]
      }
      if (rsi[j] < 50) {
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
        stop_loss[j] <- lowest_low[j] * 0.93
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
            position[j] <- ifelse(shares[j] > 0, 0.5, 0); stop_loss[j] <- entry_price[j]
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
        } else if (C_today[j] > sma20[j] && C_today[j] < sma200[j]) {
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
    
    portfolio_value <- global_cash
    for (j in active_idx) {
      if (shares[j] > 0) portfolio_value <- portfolio_value + (shares[j] * C_today[j])
    }
    equity_curve[i] <- portfolio_value
  }
  
  # Return data normalized by trading day index so we can plot them on the same X axis
  res <- data.frame(
    TradingDay = 1:length(dates),
    RealDate = dates,
    Equity = equity_curve,
    Period = p_name,
    stringsAsFactors = FALSE
  )
  return(res)
}

# 5. Execute Loop
for (p in periods) {
  cat("Running backtest for period:", p$name, "...\n")
  res <- run_period_backtest(p$name, p$start, p$end)
  if (!is.null(res)) {
    all_results[[p$name]] <- res
  }
}

plot_data <- bind_rows(all_results) %>% filter(!is.na(Equity) & Equity > 0)

# Calculate final equities
final_equities <- plot_data %>%
  group_by(Period) %>%
  slice(n()) %>%
  select(Period, Final_Equity = Equity)

cat("\n============================================\n")
cat("Final Equities by 6-Year Period:\n")
print(final_equities)
cat("============================================\n")

# 6. Plotting
cat("Generating Multi-Period Rolling Chart...\n")
p <- ggplot(plot_data, aes(x = TradingDay, y = Equity, color = Period)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(title = "Rolling 6-Year Performance Analysis - Polish Market",
       subtitle = "50 Companies (Optimized: Armed50, Entry30, SL-7%, T1_Sell=50%)",
       y = "Portfolio Equity (PLN)",
       x = "Trading Days Elapsed (0 = Year 1, 1500 = Year 6)",
       color = "Market Environment") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = scales::dollar_format())

print(p)
# Save to outputs folder since we reorganized the workspace
ggsave("outputs/portfolio_rolling_6yr_pl.png", plot = p, width = 12, height = 7)
cat("Done! Chart saved to outputs/portfolio_rolling_6yr_pl.png\n")
