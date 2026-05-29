# Analyze Current Situation for BIOMX (PHGE) based on 05 Strategy
library(quantmod)
library(dplyr)
library(tidyr)
library(tidyquant)
library(zoo)

# 1. Fetch Data
ticker <- "PHGE"
cat("Fetching daily data for", ticker, "...\n")
# Fetch enough data to calculate 200 SMA
start_date <- Sys.Date() - 400
raw_data <- tq_get(ticker, get = "stock.prices", from = start_date)

if (nrow(raw_data) == 0) {
  stop("No data fetched for ", ticker)
}

# 2. Calculate Indicators
cat("Calculating Indicators...\n")
df <- raw_data %>%
  arrange(date) %>%
  drop_na(high, low, close) %>%
  mutate(
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200),
    RSI14 = RSI(close, n = 14),
    L14 = runMin(low, n = 14),
    H14 = runMax(high, n = 14),
    fastK = (close - L14) / (H14 - L14),
    fastK = ifelse(is.nan(fastK) | is.infinite(fastK), NA, fastK),
    fastK = zoo::na.locf(fastK, na.rm = FALSE),
    fastK = ifelse(is.na(fastK), 0.5, fastK),
    fastD = SMA(fastK, n = 7),
    stoch_bull = fastK > fastD
  ) %>%
  select(-L14, -H14, -fastK, -fastD)

df <- df %>% drop_na(SMA200, RSI14)

if (nrow(df) == 0) {
  stop("Not enough data to calculate indicators (need at least 200 days).")
}

# 3. Simulate Strategy State
in_rsi_zone <- FALSE
setup_armed <- FALSE
recent_high <- NA_real_
swing_high  <- NA_real_
lowest_low  <- NA_real_
stop_loss   <- NA_real_

cat("Running State Machine...\n")

for (i in 1:nrow(df)) {
  O_today <- df$open[i]
  H_today <- df$high[i]
  L_today <- df$low[i]
  C_today <- df$close[i]
  rsi <- df$RSI14[i]
  sma10 <- df$SMA10[i]
  sma200 <- df$SMA200[i]
  stoch_today <- df$stoch_bull[i]
  
  # Tracking Phase A
  if (rsi >= 30 && !in_rsi_zone) {
    if (is.na(recent_high) || H_today > recent_high) {
      recent_high <- H_today
    }
  }
  
  if (rsi < 30) {
    if (!in_rsi_zone) {
      in_rsi_zone <- TRUE
      swing_high <- recent_high
      lowest_low <- L_today
      setup_armed <- FALSE
      recent_high <- NA
    } else {
      if (L_today < lowest_low) lowest_low <- L_today
    }
  }
  
  if (rsi >= 30 && in_rsi_zone) {
    in_rsi_zone <- FALSE
    setup_armed <- TRUE
    stop_loss <- lowest_low * 0.98
  }
  
  # Setup invalidation
  if (setup_armed) {
    if (L_today <= stop_loss) {
      setup_armed <- FALSE
    }
  }
}

# Check if entry triggered today
last_row <- df[nrow(df), ]
entry_triggered <- FALSE
risk_pct <- NA

if (setup_armed && last_row$low > stop_loss) {
  if (last_row$close > last_row$SMA10 && last_row$close < last_row$SMA200) {
    risk_pct <- (last_row$close - stop_loss) / last_row$close
    if (risk_pct <= 0.15) {
      if (!is.na(swing_high) && swing_high > last_row$close) {
        if (!is.na(last_row$stoch_bull) && last_row$stoch_bull) {
          entry_triggered <- TRUE
        }
      }
    }
  }
}

# 4. Print Report
cat("\n============================================\n")
cat("          05 STRATEGY ANALYSIS FOR", ticker, "\n")
cat("============================================\n")
cat("Date:               ", as.character(last_row$date), "\n")
cat("Close Price:        ", last_row$close, "\n")
cat("SMA10:              ", round(last_row$SMA10, 2), "\n")
cat("SMA200:             ", round(last_row$SMA200, 2), "\n")
cat("RSI14:              ", round(last_row$RSI14, 2), "\n")
cat("Stochastic Bull:    ", last_row$stoch_bull, "\n")
cat("--------------------------------------------\n")
cat("State:\n")
cat("In RSI Zone (<30):  ", in_rsi_zone, "\n")
cat("Setup Armed:        ", setup_armed, "\n")
cat("Recent High:        ", ifelse(is.na(recent_high), "N/A", recent_high), "\n")
cat("Swing High (T1):    ", ifelse(is.na(swing_high), "N/A", swing_high), "\n")
cat("Lowest Low:         ", ifelse(is.na(lowest_low), "N/A", lowest_low), "\n")
cat("Stop Loss Level:    ", ifelse(is.na(stop_loss) || (!setup_armed && !in_rsi_zone), "N/A", stop_loss), "\n")
if (!is.na(risk_pct)) {
cat("Risk %:             ", round(risk_pct * 100, 2), "%\n")
}
cat("--------------------------------------------\n")

if (entry_triggered) {
  cat(">>> STATUS: ENTRY TRIGGERED TODAY! <<<\n")
} else if (setup_armed) {
  cat(">>> STATUS: SETUP ARMED - WAITING FOR ENTRY CONDITIONS <<<\n")
  cat("Need: Close > SMA10, Close < SMA200, Close < Swing High, Stoch Bullish, Risk <= 15%\n")
} else if (in_rsi_zone) {
  cat(">>> STATUS: IN RSI ZONE - WAITING FOR BREAKOUT >= 30 <<<\n")
} else {
  cat(">>> STATUS: WAITING FOR SETUP (RSI to drop < 30) <<<\n")
}
cat("============================================\n")
