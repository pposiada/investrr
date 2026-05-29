# 05.4_advanced_strategy.R
# Multi-Asset Advanced Reversion Strategy (Portfolio-Level Backtest) - Polish Market

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyquant)
library(purrr)

# 1. Universe Definition
tickers <- c(
  # I. Financials & Insurance (Systemic Institutions)
  "PKO.WA", "PEO.WA", "PZU.WA", "ALR.WA", "SPL.WA", "ING.WA", "MBK.WA", "MIL.WA", "BHW.WA", "KRU.WA", "GPW.WA", "XTB.WA",
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

# 2. Data Wrangling
cat("Fetching daily data for 50 Polish tickers (This may take 1-2 minutes)...\n")
# Using tq_get to fetch everything efficiently
raw_data <- tq_get(tickers, get = "stock.prices", from = "2018-01-01", to = "2026-12-31")

cat("Fetching WIG20 data for benchmark comparison...\n")
benchmark_data <- tq_get("WIG20.WA", get = "stock.prices", from = "2018-01-01", to = "2026-12-31") %>%
  select(date, Benchmark_Close = close) %>%
  mutate(Benchmark_Return = Benchmark_Close / lag(Benchmark_Close) - 1)

cat("Calculating Indicators...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  drop_na(high, low, close) %>%
  mutate(
    # Use close prices for consistency with OHLC rules
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

# To make the event loop fast, we pivot the required columns to wide format
dates <- sort(unique(df$date))

cat("Pivoting data to wide format for fast time-series iteration...\n")
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

# 3. State Management
symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# Arrays to hold state for all symbols simultaneously
in_rsi_zone <- rep(FALSE, n_symbols)
setup_armed <- rep(FALSE, n_symbols)
recent_high <- rep(NA_real_, n_symbols)
swing_high  <- rep(NA_real_, n_symbols)
lowest_low  <- rep(NA_real_, n_symbols)

position    <- rep(0, n_symbols)       # 0, 0.5, or 1
entry_price <- rep(NA_real_, n_symbols)
stop_loss   <- rep(NA_real_, n_symbols)
target1     <- rep(NA_real_, n_symbols)
shares      <- rep(0, n_symbols)

global_cash <- 100000
trade_log <- list()
equity_curve <- numeric(length(dates))

# 4. Multi-Asset Event Loop
cat("Running Portfolio Event Loop over time...\n")

# Start loop at day 201 to ensure SMA200 is populated
for (i in 201:length(dates)) {
  current_date <- dates[i]
  
  # Current day vectors (one element per symbol)
  O_today <- O_mat[i, ]
  H_today <- H_mat[i, ]
  L_today <- L_mat[i, ]
  C_today <- C_mat[i, ]
  rsi <- RSI_mat[i, ]
  sma20 <- SMA20_mat[i, ]
  sma200 <- SMA200_mat[i, ]
  stoch_today <- stoch_mat[i, ]
  
  # Active symbols today (avoid NAs for recently listed/delisted stocks)
  active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
  
  triggered_indices <- integer(0)
  
  for (j in active_idx) {
    sym <- symbols[j]
    
    # ---------------------------------------------------------
    # PHASE A: Track Support & Resistance State
    # ---------------------------------------------------------
    # Track the highest price before dropping into the RSI zone
    if (rsi[j] >= 50 && !in_rsi_zone[j]) {
      if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) {
        recent_high[j] <- H_today[j]
      }
    }
    
    if (rsi[j] < 50) {
      if (!in_rsi_zone[j]) {
        # Just entered zone
        in_rsi_zone[j] <- TRUE
        swing_high[j] <- recent_high[j] # Lock Target 1
        lowest_low[j] <- L_today[j]
        setup_armed[j] <- FALSE
        recent_high[j] <- NA
      } else {
        # Dynamic Support Tracking
        if (L_today[j] < lowest_low[j]) lowest_low[j] <- L_today[j]
      }
    }
    
    if (rsi[j] >= 30 && in_rsi_zone[j]) {
      # Trigger: RSI broke > 30
      in_rsi_zone[j] <- FALSE
      setup_armed[j] <- TRUE
      stop_loss[j] <- lowest_low[j] * 0.93
    }
    
    # ---------------------------------------------------------
    # PHASE B: Exits & Risk Management
    # ---------------------------------------------------------
    if (position[j] > 0) {
      exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
      
      if (L_today[j] <= stop_loss[j]) {
        # Stop Loss Hit
        global_cash <- global_cash + (shares[j] * exec_price_stop)
        trade_log[[length(trade_log) + 1]] <- data.frame(Date = current_date, Symbol = sym, Action = "STOP_LOSS", Price = exec_price_stop, Shares = -shares[j])
        shares[j] <- 0
        position[j] <- 0
        setup_armed[j] <- FALSE
      } else {
        # Check Target 1 (50% position sell)
        if (position[j] == 1 && H_today[j] >= target1[j]) {
          exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
          sell_shares <- floor(shares[j] / 2)
          global_cash <- global_cash + (sell_shares * exec_price_t1)
          shares[j] <- shares[j] - sell_shares
          position[j] <- ifelse(shares[j] > 0, 0.5, 0)
          stop_loss[j] <- entry_price[j] # RISK FREE TRAIL
          trade_log[[length(trade_log) + 1]] <- data.frame(Date = current_date, Symbol = sym, Action = "TARGET_1_HIT", Price = exec_price_t1, Shares = -sell_shares)
        }
        # Check Target 2 (SMA 200 Reversion)
        if (position[j] == 0.5 && H_today[j] >= sma200[j]) {
          exec_price_t2 <- ifelse(O_today[j] > sma200[j], O_today[j], sma200[j])
          global_cash <- global_cash + (shares[j] * exec_price_t2)
          trade_log[[length(trade_log) + 1]] <- data.frame(Date = current_date, Symbol = sym, Action = "TARGET_2_HIT", Price = exec_price_t2, Shares = -shares[j])
          shares[j] <- 0
          position[j] <- 0
        }
      }
    } 
    # ---------------------------------------------------------
    # PHASE C: Entry Check (Add to Trigger Queue)
    # ---------------------------------------------------------
    else if (position[j] == 0 && setup_armed[j]) {
      if (L_today[j] <= stop_loss[j]) {
        setup_armed[j] <- FALSE # Support broke, invalidate
      } else if (C_today[j] > sma20[j] && C_today[j] < sma200[j]) {
        risk_pct <- (C_today[j] - stop_loss[j]) / C_today[j]
        if (risk_pct <= 0.15) {
           if (!is.na(swing_high[j]) && swing_high[j] > C_today[j]) {
              if (!is.na(stoch_today[j]) && stoch_today[j]) {
                # Valid entry trigger
                triggered_indices <- c(triggered_indices, j)
              }
           }
        } else {
          setup_armed[j] <- FALSE # Rejected (Risk > 15%)
        }
      }
    }
  }
  
  # ---------------------------------------------------------
  # PHASE D: Allocate Available Cash to New Setups
  # ---------------------------------------------------------
  if (global_cash > 0 && length(triggered_indices) > 0) {
    # Distribute cash equally among all triggers today
    cash_per_trade <- global_cash / length(triggered_indices)
    
    for (j in triggered_indices) {
      sym <- symbols[j]
      entry_px <- C_today[j]
      alloc_shares <- floor(cash_per_trade / entry_px)
      
      if (alloc_shares > 0) {
        global_cash <- global_cash - (alloc_shares * entry_px)
        shares[j] <- alloc_shares
        position[j] <- 1
        entry_price[j] <- entry_px
        target1[j] <- swing_high[j]
        setup_armed[j] <- FALSE
        
        trade_log[[length(trade_log) + 1]] <- data.frame(Date = current_date, Symbol = sym, Action = "ENTRY", Price = entry_px, Shares = alloc_shares)
      }
    }
  }
  
  # ---------------------------------------------------------
  # Mark-to-Market Portfolio Equity
  # ---------------------------------------------------------
  portfolio_value <- global_cash
  for (j in active_idx) {
    if (shares[j] > 0) {
      portfolio_value <- portfolio_value + (shares[j] * C_today[j])
    }
  }
  equity_curve[i] <- portfolio_value
}

# 5. Reporting
trade_df <- bind_rows(trade_log)

cat("\n============================================\n")
cat("Total Trades Executed:", nrow(trade_df), "\n")
cat("Final Portfolio Value: ", round(equity_curve[length(equity_curve)], 2), " PLN\n")

# Calculate Strategy Returns and Correlation
plot_data <- data.frame(Date = dates, Equity = equity_curve) %>% filter(!is.na(Equity) & Equity > 0)
plot_data <- plot_data %>% 
  mutate(Strategy_Return = Equity / lag(Equity) - 1)

if (nrow(benchmark_data) > 0 && "Benchmark_Return" %in% colnames(benchmark_data)) {
  plot_data <- plot_data %>%
    left_join(benchmark_data, by = c("Date" = "date"))
  
  # Check if we have enough complete pairs for correlation
  complete_pairs <- sum(complete.cases(plot_data$Strategy_Return, plot_data$Benchmark_Return))
  if (complete_pairs > 2) {
    strat_cor <- cor(plot_data$Strategy_Return, plot_data$Benchmark_Return, use = "complete.obs")
    cat("Strategy vs WIG20 Correlation:", round(strat_cor, 4), "\n")
  } else {
    cat("Strategy vs WIG20 Correlation: Not enough valid benchmark data points.\n")
  }
} else {
  cat("Strategy vs WIG20 Correlation: Benchmark data unavailable.\n")
}
cat("============================================\n\n")

if (nrow(trade_df) > 0) {
  print(head(trade_df, 15))
}

# Plot
plot_data <- data.frame(Date = dates, Equity = equity_curve) %>% filter(!is.na(Equity) & Equity > 0)
p <- ggplot(plot_data, aes(x = Date, y = Equity)) +
  geom_line(color = "purple") +
  theme_minimal() +
  labs(title = "Portfolio Advanced Reversal Strategy - Polish Market",
       subtitle = "50 Companies (Optimized: Armed50, Entry30, SL-7%, T1_Sell=50%)",
       y = "Portfolio Equity (PLN)",
       x = "Date")

print(p)
ggsave("outputs/portfolio_advanced_equity_pl.png", plot = p, width = 10, height = 6)
cat("Multi-asset backtest complete! Results saved to outputs/portfolio_advanced_equity_pl.png\n")
