# 07.1_portfolio_manager.R
# Live Portfolio Manager & Strategy Scanner (Multiple Entries & 3% Stop-Loss) - Polish Market

library(quantmod)
library(dplyr)
library(tidyr)
library(tidyquant)
library(readr)

# =========================================================================
# 1. PORTFOLIO HELPER FUNCTIONS
# Run these in your console to update your active_positions.csv file
# =========================================================================

CSV_PATH <- "active_positions.csv"

# Explicit column types to prevent read_csv from guessing 'character' on empty files
PTF_COLS <- cols(
  Symbol = col_character(),
  EntryDate = col_character(),
  EntryPrice = col_double(),
  Shares = col_double(),
  StopLoss = col_double()
)

#' Initialize Portfolio Tracking CSV
#'
#' Checks if the active_positions.csv file exists in the current directory. 
#' If it does not exist, it creates a new empty CSV file with the required column headers 
#' to track active trades.
#'
#' @return NULL (Writes a file to disk)
#' @export
init_portfolio <- function() {
  if (!file.exists(CSV_PATH)) {
    df <- data.frame(
      Symbol = character(),
      EntryDate = character(),
      EntryPrice = numeric(),
      Shares = numeric(),
      StopLoss = numeric(),
      stringsAsFactors = FALSE
    )
    write_csv(df, CSV_PATH)
    cat("Created new active_positions.csv file.\n")
  }
}

#' Reset Portfolio
#'
#' Deletes the active_positions.csv file to completely wipe your portfolio history.
#' Use with caution as this action cannot be undone.
#'
#' @return NULL
#' @export
reset_portfolio <- function() {
  if (file.exists(CSV_PATH)) {
    file.remove(CSV_PATH)
    cat("Portfolio reset successfully. All active positions deleted.\n")
  } else {
    cat("Portfolio already empty. No file to delete.\n")
  }
}

#' Log a New Buy Transaction
#'
#' Adds a new active trade or adds to an existing trade in the portfolio tracker.
#' Automatically calculates and updates the average entry price and the 3% stop-loss.
#'
#' @param symbol Character. The ticker symbol of the company (e.g., "PKO.WA").
#' @param shares Numeric. The total number of shares purchased.
#' @param entry_price Numeric. The exact execution price of the buy order.
#'
#' @return NULL (Updates the active_positions.csv file)
#' @export
buy_stock <- function(symbol, shares, entry_price) {
  stopifnot(is.character(symbol), length(symbol) == 1)
  stopifnot(is.numeric(shares), shares > 0)
  stopifnot(is.numeric(entry_price), entry_price > 0)
  
  init_portfolio()
  df <- read_csv(CSV_PATH, col_types = PTF_COLS)
  
  if (symbol %in% df$Symbol) {
    idx <- which(df$Symbol == symbol)
    existing_shares <- df$Shares[idx]
    existing_entry <- df$EntryPrice[idx]
    
    new_shares <- existing_shares + shares
    new_entry <- ((existing_shares * existing_entry) + (shares * entry_price)) / new_shares
    
    df$Shares[idx] <- new_shares
    df$EntryPrice[idx] <- new_entry
    df$StopLoss[idx] <- new_entry * 0.97 # Update stop-loss to 3% below average entry
    df$EntryDate[idx] <- as.character(Sys.Date()) # Update date to latest addition
    
    write_csv(df, CSV_PATH)
    cat("Successfully added to existing position for", symbol, ".\n")
    cat("New Average Price:", round(new_entry, 2), "| Total Shares:", new_shares, "| New Stop Loss:", round(new_entry * 0.97, 2), "\n")
    return()
  }
  
  new_trade <- data.frame(
    Symbol = symbol,
    EntryDate = as.character(Sys.Date()),
    EntryPrice = entry_price,
    Shares = shares,
    StopLoss = entry_price * 0.97
  )
  
  df <- bind_rows(df, new_trade)
  write_csv(df, CSV_PATH)
  cat("Successfully added", symbol, "to portfolio. Stop Loss (3%):", round(entry_price * 0.97, 2), "\n")
}

#' Log a Complete Position Closure
#'
#' Removes a stock from the active portfolio tracker. Use this function when the 
#' position is completely closed.
#'
#' @param symbol Character. The ticker symbol of the company to remove.
#' @param reason Character. Optional. The reason for closing the trade (e.g., "Stop Loss Hit"). Defaults to "Closed".
#'
#' @return NULL (Updates the active_positions.csv file)
#' @export
sell_stock <- function(symbol, reason = "Closed") {
  stopifnot(is.character(symbol), length(symbol) == 1)
  stopifnot(is.character(reason), length(reason) == 1)
  
  df <- read_csv(CSV_PATH, col_types = PTF_COLS)
  if (symbol %in% df$Symbol) {
    df <- df %>% filter(Symbol != symbol)
    write_csv(df, CSV_PATH)
    cat("Successfully removed", symbol, "from portfolio. Reason:", reason, "\n")
  } else {
    cat("Error: Cannot find", symbol, "in portfolio.\n")
  }
}

# =========================================================================
# 2. DAILY PORTFOLIO EVALUATOR & MARKET SCANNER
# Run this entire block every day after market close
# =========================================================================

# The Master Universe
all_tickers <- c(
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

init_portfolio()
portfolio <- read_csv(CSV_PATH, col_types = PTF_COLS)
active_symbols <- portfolio$Symbol

cat("\n=================================================================\n")
cat("                LIVE PORTFOLIO & STRATEGY DASHBOARD              \n")
cat("Date:", as.character(Sys.Date()), "\n")
cat("=================================================================\n\n")

# Fetch recent data for everyone
cat("Fetching market data for 50 Polish tickers...\n")
start_date <- Sys.Date() - 1000
raw_data <- tq_get(all_tickers, get = "stock.prices", from = start_date, to = Sys.Date())

cat("Calculating Indicators...\n\n")
df <- raw_data %>%
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

# -------------------------------------------------------------------------
# SECTION A: EVALUATE ACTIVE POSITIONS
# -------------------------------------------------------------------------
cat(">>> ACTIVE POSITIONS EVALUATION <<<\n")

if (nrow(portfolio) == 0) {
  cat("Your portfolio is currently empty. 100% Cash.\n\n")
} else {
  total_cost <- 0
  total_value <- 0
  
  for (i in 1:nrow(portfolio)) {
    sym <- portfolio$Symbol[i]
    trade <- portfolio[i, ]
    
    sym_data <- df %>% filter(symbol == sym) %>% arrange(date)
    last_row <- sym_data[nrow(sym_data), ]
    
    C <- last_row$close
    H <- last_row$high
    L <- last_row$low
    
    position_cost <- trade$EntryPrice * trade$Shares
    position_value <- C * trade$Shares
    profit_val <- position_value - position_cost
    profit_pct <- (profit_val / position_cost) * 100
    
    total_cost <- total_cost + position_cost
    total_value <- total_value + position_value
    
    cat("---", sym, "---\n")
    cat("Shares Owned:", trade$Shares, "\n")
    cat("Current Close:", round(C, 2), "| Entry Price:", round(trade$EntryPrice, 2), "| Stop Loss (3%):", round(trade$StopLoss, 2), "\n")
    
    sign_char <- if (profit_val >= 0) "+" else ""
    cat("Position Profit/Loss: ", sign_char, round(profit_val, 2), " PLN (", sign_char, round(profit_pct, 2), "%)\n", sep="")
    
    if (L <= trade$StopLoss) {
      cat(">> ACTION REQUIRED: SELL ALL. Stop Loss Hit!\n")
      cat(">> Run in console: sell_stock('", sym, "', reason='Stop Loss')\n", sep="")
    } else {
      cat(">> STATUS: HOLD. Current price is above stop loss.\n")
    }
    cat("\n")
  }
  
  total_profit_val <- total_value - total_cost
  total_profit_pct <- if (total_cost > 0) (total_profit_val / total_cost) * 100 else 0
  total_sign <- if (total_profit_val >= 0) "+" else ""
  
  cat("=================================================================\n")
  cat("                OVERALL PORTFOLIO SUMMARY (WALLET)               \n")
  cat("=================================================================\n")
  cat("Total Portfolio Cost:   ", round(total_cost, 2), " PLN\n", sep="")
  cat("Total Current Value:    ", round(total_value, 2), " PLN\n", sep="")
  cat("Total Unrealized Profit: ", total_sign, round(total_profit_val, 2), " PLN (", total_sign, round(total_profit_pct, 2), "%)\n", sep="")
  cat("=================================================================\n\n")
}

# -------------------------------------------------------------------------
# SECTION B: SCAN FOR NEW OPPORTUNITIES
# -------------------------------------------------------------------------
cat(">>> SCANNING WATCHLIST FOR NEW SETUPS <<<\n")

results <- list()
# We scan all tickers (including active ones) to support Multiple Entries additions
scan_tickers <- all_tickers

for (sym in scan_tickers) {
  sym_data <- df %>% filter(symbol == sym) %>% arrange(date)
  if (nrow(sym_data) < 201) next
  
  # Initialize State
  in_rsi_zone <- FALSE
  setup_armed <- FALSE
  recent_high <- NA
  swing_high <- NA
  lowest_low <- NA
  
  # Run state machine up to the present day
  for (i in 201:nrow(sym_data)) {
    H <- sym_data$high[i]
    L <- sym_data$low[i]
    rsi <- sym_data$RSI14[i]
    if(is.na(rsi)) next
    
    if (rsi >= 50 && !in_rsi_zone) {
      if (is.na(recent_high) || H > recent_high) recent_high <- H
    }
    
    if (rsi < 50) {
      if (!in_rsi_zone) {
        in_rsi_zone <- TRUE
        swing_high <- recent_high   
        lowest_low <- L
        setup_armed <- FALSE        
        recent_high <- NA
      } else {
        if (L < lowest_low) lowest_low <- L
      }
    }
    
    if (rsi >= 30 && in_rsi_zone) {
      in_rsi_zone <- FALSE
      setup_armed <- TRUE
    }
    
    if (setup_armed && L <= (lowest_low * 0.93)) setup_armed <- FALSE
  }
  
  # Check Final Day Status
  last_row <- sym_data[nrow(sym_data), ]
  status <- "NONE"
  details <- ""
  
  if (setup_armed) {
    if (last_row$close > last_row$SMA20 && last_row$close < last_row$SMA200) {
      setup_stop_loss <- lowest_low * 0.93
      risk_pct <- (last_row$close - setup_stop_loss) / last_row$close
      
      if (risk_pct <= 0.15 && !is.na(swing_high) && swing_high > last_row$close) {
        if (!is.na(last_row$stoch_bull) && last_row$stoch_bull) {
          status <- "ACTION (ENTRY TRIGGERED)"
          stop_loss_val <- last_row$close * 0.97 # 3% Stop-Loss
          details <- paste("Buy Price:", round(last_row$close, 2), 
                           "| Stop Loss (3%):", round(stop_loss_val, 2))
          
          owned_tag <- if (sym %in% active_symbols) " (ADD TO POSITION)" else ""
          details <- paste0(details, owned_tag, "\n   Run: buy_stock('", sym, "', shares=100, entry_price=", 
                            round(last_row$close, 2), ")")
        }
      }
    }
  }
  
  if (status != "NONE") {
    results[[sym]] <- data.frame(Symbol = sym, Status = status, Details = details, stringsAsFactors = FALSE)
  }
}

if (length(results) > 0) {
  report_df <- bind_rows(results)
  actions <- report_df %>% filter(grepl("ACTION", Status))
  
  if (nrow(actions) > 0) {
    for (i in 1:nrow(actions)) {
      cat("--- NEW SETUP:", actions$Symbol[i], "---\n")
      cat(actions$Details[i], "\n\n")
    }
  } else {
    cat("No new entry setups triggered today.\n")
  }
} else {
  cat("No new entry setups triggered today.\n")
}
cat("=================================================================\n")
