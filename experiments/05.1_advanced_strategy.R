# 05.1_advanced_strategy.R
# Multi-Asset Advanced Reversion Strategy (100% Exit on Target 1)

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyquant)
library(purrr)

# 1. Universe Definition
tickers <- c(
  # Defense & Aerospace
  "LMT", "BA", "GD", "RTX", "NOC", "LHX", "HII", "TXT", "LDOS", "BAH",
  # Systemically Important Financial Institutions
  "JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "STT", "PNC", "USB", "COF", "TFC",
  # Energy & Power
  "XOM", "CVX", "COP", "NEE", "DUK", "SO", "D", "AEP", "EXC", "SRE", "PCG", "KMI", "WMB", "SLB", "HAL", "BKR",
  # Technology & Semiconductors
  "MSFT", "AAPL", "GOOGL", "AMZN", "META", "INTC", "NVDA", "AMD", "QCOM", "TXN", "AVGO", "MU", "AMAT", "IBM", "ORCL", "CSCO", "PANW", "CRWD",
  # Telecommunications
  "T", "VZ", "TMUS", "CMCSA", "CHTR",
  # Transportation & Logistics
  "UNP", "CSX", "NSC", "FDX", "UPS", "DAL", "UAL", "AAL", "LUV",
  # Healthcare & Pharmaceuticals
  "JNJ", "PFE", "MRK", "ABBV", "LLY", "UNH", "CVS", "ELV", "MCK", "COR", "CAH", "MRNA",
  # Agriculture & Food Supply
  "ADM", "BG", "DE", "CTVA", "TSN", "GIS", "K", "CF", "MOS",
  # Automotive & Heavy Manufacturing
  "F", "GM", "CAT", "CMI", "PCAR",
  # Materials & Chemicals
  "DOW", "DD", "NUE", "FCX"
)

# 2. Data Wrangling
cat("Fetching daily data for 100 tickers (This may take 1-2 minutes)...\n")
# Using tq_get to fetch everything efficiently
raw_data <- tq_get(tickers, get = "stock.prices", from = "2018-01-01", to = "2026-12-31")

cat("Calculating Indicators...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    # Use close prices for consistency with OHLC rules
    SMA20 = SMA(close, n = 20),
    SMA200 = SMA(close, n = 200),
    RSI14 = RSI(close, n = 14)
  ) %>%
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

# 3. State Management
symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# Arrays to hold state for all symbols simultaneously
in_rsi_zone <- rep(FALSE, n_symbols)
setup_armed <- rep(FALSE, n_symbols)
recent_high <- rep(NA_real_, n_symbols)
swing_high  <- rep(NA_real_, n_symbols)
lowest_low  <- rep(NA_real_, n_symbols)

position    <- rep(0, n_symbols)       # 0 or 1
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
  
  # Active symbols today (avoid NAs for recently listed/delisted stocks)
  active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
  
  triggered_indices <- integer(0)
  
  for (j in active_idx) {
    sym <- symbols[j]
    
    # ---------------------------------------------------------
    # PHASE A: Track Support & Resistance State
    # ---------------------------------------------------------
    # Track the highest price before dropping into the RSI zone
    if (rsi[j] >= 30 && !in_rsi_zone[j]) {
      if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) {
        recent_high[j] <- H_today[j]
      }
    }
    
    if (rsi[j] < 30) {
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
      stop_loss[j] <- lowest_low[j] * 0.99
    }
    
    # ---------------------------------------------------------
    # PHASE B: Exits & Risk Management
    # ---------------------------------------------------------
    if (position[j] == 1) {
      exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
      
      if (L_today[j] <= stop_loss[j]) {
        # Stop Loss Hit
        global_cash <- global_cash + (shares[j] * exec_price_stop)
        trade_log[[length(trade_log) + 1]] <- data.frame(Date = current_date, Symbol = sym, Action = "STOP_LOSS", Price = exec_price_stop, Shares = -shares[j])
        shares[j] <- 0
        position[j] <- 0
        setup_armed[j] <- FALSE
      } else {
        # Check Target 1 (100% position sell instead of 50%)
        if (H_today[j] >= target1[j]) {
          exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
          global_cash <- global_cash + (shares[j] * exec_price_t1)
          trade_log[[length(trade_log) + 1]] <- data.frame(Date = current_date, Symbol = sym, Action = "TARGET_HIT", Price = exec_price_t1, Shares = -shares[j])
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
              # Valid entry trigger
              triggered_indices <- c(triggered_indices, j)
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
cat("Final Portfolio Value: $", round(equity_curve[length(equity_curve)], 2), "\n")
cat("============================================\n\n")

if (nrow(trade_df) > 0) {
  print(head(trade_df, 15))
}

# Plot
plot_data <- data.frame(Date = dates, Equity = equity_curve) %>% filter(!is.na(Equity) & Equity > 0)
p <- ggplot(plot_data, aes(x = Date, y = Equity)) +
  geom_line(color = "darkgreen") +
  theme_minimal() +
  labs(title = "Portfolio Advanced Reversal Strategy (100% Exit on Target 1)",
       subtitle = "100 Systemically Important Companies (100% Cash Deployment, Equal Split)",
       y = "Portfolio Equity ($)",
       x = "Date")

print(p)
ggsave("portfolio_advanced_equity_100_exit.png", plot = p, width = 10, height = 6)
cat("Multi-asset backtest complete! Results saved to portfolio_advanced_equity_100_exit.png\n")
