# 06.1_strategy_scanner.R
# Real-Time Market Scanner for Advanced Reversal Strategy - Polish Market

library(quantmod)
library(dplyr)
library(tidyr)
library(tidyquant)

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
# We only need the last 3 years of data to establish the SMA200 and swing highs
start_date <- Sys.Date() - 1000
end_date <- Sys.Date()

cat("Fetching recent data for 50 Polish tickers to scan current market state...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = start_date, to = end_date)

cat("Calculating Indicators...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  fill(open, high, low, close, .direction = "down") %>%
  mutate(
    SMA20 = SMA(close, n = 20),
    SMA200 = SMA(close, n = 200),
    RSI14 = RSI(close, n = 14)
  ) %>%
  ungroup()

# 3. Market Scanner Loop
cat("Scanning for current setups...\n\n")

results <- list()

for (sym in tickers) {
  sym_data <- df %>% filter(symbol == sym) %>% arrange(date)
  
  if (nrow(sym_data) < 201) next # Skip if not enough data for SMA200
  
  # Initialize State
  in_rsi_zone <- FALSE
  setup_armed <- FALSE
  recent_high <- NA
  swing_high <- NA
  lowest_low <- NA
  
  # Run state machine up to the present day
  for (i in 201:nrow(sym_data)) {
    O <- sym_data$open[i]
    H <- sym_data$high[i]
    L <- sym_data$low[i]
    C <- sym_data$close[i]
    rsi <- sym_data$RSI14[i]
    
    # Missing data check
    if(is.na(rsi)) next
    
    # Track the highest price before dropping into the RSI zone
    if (rsi >= 30 && !in_rsi_zone) {
      if (is.na(recent_high) || H > recent_high) {
        recent_high <- H
      }
    }
    
    # Phase 1: RSI drops below 30
    if (rsi < 30) {
      if (!in_rsi_zone) {
        in_rsi_zone <- TRUE
        swing_high <- recent_high   # Lock in Target 1
        lowest_low <- L
        setup_armed <- FALSE        # Thesis rebuilding
        recent_high <- NA
      } else {
        # Dynamic Support tracking
        if (L < lowest_low) lowest_low <- L
      }
    }
    
    # Phase 1 Trigger: RSI crosses back above 30
    if (rsi >= 30 && in_rsi_zone) {
      in_rsi_zone <- FALSE
      setup_armed <- TRUE
    }
    
    # Check invalidation if armed
    if (setup_armed && L <= (lowest_low * 0.99)) {
      setup_armed <- FALSE
    }
  }
  
  # 4. Evaluate Final Day Status
  last_row <- sym_data[nrow(sym_data), ]
  status <- "NONE"
  details <- ""
  
  if (in_rsi_zone) {
    status <- "WATCH (Forming Support)"
    details <- paste("RSI < 30. Current Lowest Low:", round(lowest_low, 2))
  } else if (setup_armed) {
    # Check if entry conditions are met today
    if (last_row$close > last_row$SMA20 && last_row$close < last_row$SMA200) {
      stop_loss <- lowest_low * 0.99
      risk_pct <- (last_row$close - stop_loss) / last_row$close
      
      if (risk_pct <= 0.15) {
        if (!is.na(swing_high) && swing_high > last_row$close) {
          status <- "ACTION (ENTRY TRIGGERED)"
          details <- paste("Entry:", round(last_row$close, 2), 
                           "| Stop:", round(stop_loss, 2), 
                           "| Target 1:", round(swing_high, 2), 
                           "| Risk:", round(risk_pct * 100, 1), "%")
        } else {
          status <- "WATCH (Invalid Target 1)"
          details <- paste("Target 1 (", round(swing_high, 2), ") is below current price.")
        }
      } else {
        status <- "WATCH (Risk > 15%)"
        details <- paste("Current Risk:", round(risk_pct * 100, 1), "%")
      }
    } else {
      status <- "WATCH (Armed & Waiting)"
      details <- paste("Waiting for Close > SMA20. Close:", round(last_row$close, 2), "SMA20:", round(last_row$SMA20, 2))
    }
  }
  
  if (status != "NONE") {
    results[[sym]] <- data.frame(
      Symbol = sym, 
      Status = status, 
      Details = details,
      stringsAsFactors = FALSE
    )
  }
}

# 5. Print Report
if (length(results) > 0) {
  report_df <- bind_rows(results)
  
  cat("=================================================================\n")
  cat("                    STRATEGY SCANNER REPORT                      \n")
  cat("Date:", as.character(Sys.Date()), "\n")
  cat("=================================================================\n\n")
  
  # Print Actions First
  actions <- report_df %>% filter(grepl("ACTION", Status))
  if (nrow(actions) > 0) {
    cat(">>> TRADE SETUPS TRIGGERED TODAY <<<\n")
    print(actions, row.names = FALSE, right = FALSE)
    cat("\n")
  }
  
  # Print Watches Second
  watches <- report_df %>% filter(grepl("WATCH", Status)) %>% arrange(Status)
  if (nrow(watches) > 0) {
    cat(">>> STOCKS ON WATCHLIST <<<\n")
    print(watches, row.names = FALSE, right = FALSE)
    cat("\n")
  }
  
} else {
  cat("No stocks currently meet the watch or entry criteria.\n")
}
