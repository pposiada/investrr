# 08_trade_calculator.R
# Function to calculate stop losses, targets, and risk for a specific entry price
# based on the advanced mean-reversion strategy.

library(quantmod)
library(dplyr)
library(tidyr)
library(tidyquant)

#' Calculate Trade Parameters for a specific entry
#'
#' This function runs the strategy state machine up to the specified date 
#' to find the relevant lowest low (for stop loss) and swing high (for target 1), 
#' and calculates the exact risk metrics given a specific entry price.
#'
#' @param symbol Character. The ticker symbol (e.g., "PKO.WA" or "AAPL").
#' @param entry_price Numeric. The price at which you plan to enter (or have entered).
#' @param entry_date Character or Date. The date of the entry. Defaults to Sys.Date().
#'
#' @return A list containing calculated trade parameters.
#' @export
calculate_trade_parameters <- function(symbol, entry_price, entry_date = Sys.Date()) {
  # Convert to Date
  entry_date <- as.Date(entry_date)
  
  # Fetch data (need enough history for SMA200 and state machine)
  start_date <- entry_date - 1000
  
  cat("Fetching data for", symbol, "up to", as.character(entry_date), "...\n")
  raw_data <- suppressMessages(tq_get(symbol, get = "stock.prices", from = start_date, to = entry_date))
  
  if (nrow(raw_data) == 0) {
    stop("No data found for symbol ", symbol)
  }
  
  # Calculate indicators
  df <- raw_data %>%
    arrange(date) %>%
    fill(open, high, low, close, .direction = "down") %>%
    mutate(
      SMA20 = SMA(close, n = 20),
      SMA200 = SMA(close, n = 200),
      RSI14 = RSI(close, n = 14)
    )
  
  if (nrow(df) < 201) {
    stop("Not enough data to calculate indicators (need at least 200 days).")
  }
  
  # Initialize State Machine
  in_rsi_zone <- FALSE
  setup_armed <- FALSE
  recent_high <- NA
  swing_high <- NA
  lowest_low <- NA
  
  # Run state machine up to the entry date
  for (i in 201:nrow(df)) {
    H <- df$high[i]
    L <- df$low[i]
    rsi <- df$RSI14[i]
    
    if(is.na(rsi)) next
    
    # Tracking recent highs when outside the RSI < 30 zone
    if (rsi >= 30 && !in_rsi_zone) {
      if (is.na(recent_high) || H > recent_high) recent_high <- H
    }
    
    # Entering or inside the RSI < 30 zone
    if (rsi < 30) {
      if (!in_rsi_zone) {
        in_rsi_zone <- TRUE
        swing_high <- recent_high   # Lock in the highest point before dropping
        lowest_low <- L
        setup_armed <- FALSE        
        recent_high <- NA           # Reset recent high tracker
      } else {
        if (L < lowest_low) lowest_low <- L
      }
    }
    
    # Exiting the RSI < 30 zone
    if (rsi >= 30 && in_rsi_zone) {
      in_rsi_zone <- FALSE
      setup_armed <- TRUE
    }
    
    # Setup fails if price drops below stop loss
    if (setup_armed && L <= (lowest_low * 0.99)) {
      setup_armed <- FALSE
    }
  }
  
  # Check Final Day Status
  last_row <- df[nrow(df), ]
  
  if (is.na(lowest_low) || is.na(swing_high)) {
    stop("No valid setup history found for this symbol within the lookback period.")
  }
  
  # Calculate Strategy Levels
  stop_loss <- lowest_low * 0.99
  target1 <- swing_high
  target2 <- last_row$SMA200
  
  # Calculate Risk/Reward based on the *custom* entry price provided by user
  risk_amount <- entry_price - stop_loss
  risk_pct <- risk_amount / entry_price
  
  reward1_amount <- target1 - entry_price
  reward2_amount <- target2 - entry_price
  
  rr1 <- ifelse(risk_amount > 0, reward1_amount / risk_amount, NA)
  rr2 <- ifelse(risk_amount > 0, reward2_amount / risk_amount, NA)
  
  # Output Results
  cat("\n=================================================\n")
  cat("           TRADE PARAMETER CALCULATOR            \n")
  cat("=================================================\n")
  cat("Symbol      :", symbol, "\n")
  cat("Entry Date  :", as.character(entry_date), "\n")
  cat("Entry Price :", round(entry_price, 2), "\n")
  cat("-------------------------------------------------\n")
  cat("Stop Loss   :", round(stop_loss, 2), "\n")
  cat("Target 1    :", round(target1, 2), "(Swing High)\n")
  cat("Target 2    :", round(target2, 2), "(SMA 200)\n")
  cat("-------------------------------------------------\n")
  cat("Risk        :", round(risk_pct * 100, 2), "%\n")
  cat("R/R to T1   :", round(rr1, 2), "\n")
  cat("R/R to T2   :", round(rr2, 2), "\n")
  cat("=================================================\n")
  
  # Strategy Validations
  cat(">>> STRATEGY VALIDATION <<<\n")
  
  if (!setup_armed) {
    cat("[!] WARNING: Setup is NOT currently armed. (It may have been invalidated by breaking the stop loss).\n")
  } else {
    cat("[OK] Setup is currently ARMED based on RSI rules.\n")
  }
  
  if (entry_price < last_row$SMA20) {
    cat("[!] WARNING: Entry price is below SMA20 (", round(last_row$SMA20, 2), "). Strategy requires Close > SMA20.\n", sep="")
  } else {
    cat("[OK] Entry price is above SMA20.\n")
  }
  
  if (entry_price > last_row$SMA200) {
    cat("[!] WARNING: Entry price is above SMA200 (", round(last_row$SMA200, 2), "). Strategy requires Close < SMA200.\n", sep="")
  } else {
    cat("[OK] Entry price is below SMA200.\n")
  }
  
  if (risk_pct > 0.15) {
    cat("[!] WARNING: Risk is", round(risk_pct * 100, 2), "%, which exceeds the strategy maximum of 15%.\n")
  } else if (risk_amount <= 0) {
    cat("[!] WARNING: Stop loss is above or equal to your entry price. This is invalid.\n")
  } else {
    cat("[OK] Risk is within acceptable limits (<= 15%).\n")
  }
  
  if (target1 <= entry_price) {
    cat("[!] WARNING: Target 1 is below your entry price. You are entering too late.\n")
  } else {
    cat("[OK] Target 1 provides positive reward.\n")
  }
  
  cat("=================================================\n\n")
  
  # Return a named list of the values for programmatic use
  invisible(list(
    Symbol = symbol,
    EntryPrice = entry_price,
    StopLoss = stop_loss,
    Target1 = target1,
    Target2 = target2,
    RiskPct = risk_pct,
    RewardRisk1 = rr1,
    RewardRisk2 = rr2,
    SetupArmed = setup_armed
  ))
}

# Example Usage:
# calculate_trade_parameters("PKO.WA", entry_price = 58.50)
