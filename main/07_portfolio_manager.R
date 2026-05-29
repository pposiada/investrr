# 07_portfolio_manager.R
# Live Portfolio Manager & Strategy Scanner

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
  StopLoss = col_double(),
  Target1 = col_double(),
  Phase = col_double()
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
      Target1 = numeric(),
      Phase = numeric(), # Phase 1 = Full Position, Phase 2 = Half Position (Target 1 Hit)
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
#' Adds a new active trade to the portfolio tracker. Use this function immediately 
#' after executing a buy order in your real brokerage account.
#'
#' @param symbol Character. The ticker symbol of the company (e.g., "AAPL").
#' @param shares Numeric. The total number of shares purchased.
#' @param entry_price Numeric. The exact execution price of the buy order.
#' @param stop_loss Numeric. The structural stop loss price calculated by the strategy.
#' @param target1 Numeric. The Target 1 (swing high) price calculated by the strategy.
#'
#' @return NULL (Updates the active_positions.csv file)
#' @export
buy_stock <- function(symbol, shares, entry_price, stop_loss, target1) {
  stopifnot(is.character(symbol), length(symbol) == 1)
  stopifnot(is.numeric(shares), shares > 0)
  stopifnot(is.numeric(entry_price), entry_price > 0)
  stopifnot(is.numeric(stop_loss), stop_loss > 0)
  stopifnot(is.numeric(target1), target1 > 0)
  
  init_portfolio()
  df <- read_csv(CSV_PATH, col_types = PTF_COLS)
  
  if (symbol %in% df$Symbol) {
    cat("Warning:", symbol, "is already in the portfolio. Please use sell_stock first if this is a new trade.\n")
    return()
  }
  
  new_trade <- data.frame(
    Symbol = symbol,
    EntryDate = as.character(Sys.Date()),
    EntryPrice = entry_price,
    Shares = shares,
    StopLoss = stop_loss,
    Target1 = target1,
    Phase = 1
  )
  
  df <- bind_rows(df, new_trade)
  write_csv(df, CSV_PATH)
  cat("Successfully added", symbol, "to portfolio.\n")
}

#' Log a Target 1 (50%) Partial Exit
#'
#' Updates an active trade in the portfolio tracker to reflect that 50% of the 
#' position has been sold at Target 1. This function automatically halves the 
#' share count and moves the stop loss up to the original entry price (break-even).
#'
#' @param symbol Character. The ticker symbol of the company (e.g., "AAPL").
#'
#' @return NULL (Updates the active_positions.csv file)
#' @export
log_partial_exit <- function(symbol) {
  stopifnot(is.character(symbol), length(symbol) == 1)
  
  df <- read_csv(CSV_PATH, col_types = PTF_COLS)
  if (symbol %in% df$Symbol) {
    idx <- which(df$Symbol == symbol)
    df$Shares[idx] <- floor(df$Shares[idx] / 2)
    df$StopLoss[idx] <- df$EntryPrice[idx] # Move stop to breakeven!
    df$Phase[idx] <- 2
    write_csv(df, CSV_PATH)
    cat("Successfully logged Target 1 exit for", symbol, ". Stop loss moved to breakeven.\n")
  } else {
    cat("Error: Cannot find", symbol, "in portfolio.\n")
  }
}

#' Log a Complete Position Closure
#'
#' Removes a stock from the active portfolio tracker. Use this function when the 
#' position is completely closed (either by hitting the stop loss or reaching Target 2).
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

init_portfolio()
portfolio <- read_csv(CSV_PATH, col_types = PTF_COLS)
active_symbols <- portfolio$Symbol

cat("\n=================================================================\n")
cat("                LIVE PORTFOLIO & STRATEGY DASHBOARD              \n")
cat("Date:", as.character(Sys.Date()), "\n")
cat("=================================================================\n\n")

# Fetch recent data for everyone
cat("Fetching market data...\n")
start_date <- Sys.Date() - 1000
raw_data <- tq_get(all_tickers, get = "stock.prices", from = start_date, to = Sys.Date())

cat("Calculating Indicators...\n\n")
df <- raw_data %>%
  group_by(symbol) %>%
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
  select(-L14, -H14, -fastK, -fastD) %>%
  ungroup()


# -------------------------------------------------------------------------
# SECTION A: EVALUATE ACTIVE POSITIONS
# -------------------------------------------------------------------------
cat(">>> ACTIVE POSITIONS EVALUATION <<<\n")

if (nrow(portfolio) == 0) {
  cat("Your portfolio is currently empty. 100% Cash.\n\n")
} else {
  for (i in 1:nrow(portfolio)) {
    sym <- portfolio$Symbol[i]
    trade <- portfolio[i, ]
    
    sym_data <- df %>% filter(symbol == sym) %>% arrange(date)
    last_row <- sym_data[nrow(sym_data), ]
    
    C <- last_row$close
    H <- last_row$high
    L <- last_row$low
    sma200 <- last_row$SMA200
    
    cat("---", sym, "---\n")
    cat("Current Price:", round(C, 2), "| Stop Loss:", round(trade$StopLoss, 2))
    
    if (trade$Phase == 1) {
      cat(" | Target 1:", round(trade$Target1, 2), "\n")
      
      if (L <= trade$StopLoss) {
        cat(">> ACTION REQUIRED: SELL ALL. Stop Loss Hit!\n")
        cat(">> Run in console: sell_stock('", sym, "', reason='Stop Loss')\n", sep="")
      } else if (H >= trade$Target1) {
        cat(">> ACTION REQUIRED: SELL 50%. Target 1 Hit!\n")
        cat(">> Run in console: log_partial_exit('", sym, "')\n", sep="")
      } else {
        cat(">> STATUS: HOLD 100%. Target 1 not reached.\n")
      }
      
    } else if (trade$Phase == 2) {
      cat(" | Target 2 (SMA200):", round(sma200, 2), "\n")
      
      if (L <= trade$StopLoss) { # Stop loss is now breakeven
        cat(">> ACTION REQUIRED: SELL REMAINING. Breakeven Stop Hit!\n")
        cat(">> Run in console: sell_stock('", sym, "', reason='Breakeven Stop')\n", sep="")
      } else if (H >= sma200) {
        cat(">> ACTION REQUIRED: SELL REMAINING. Target 2 (SMA 200) Hit!\n")
        cat(">> Run in console: sell_stock('", sym, "', reason='Target 2 Hit')\n", sep="")
      } else {
        cat(">> STATUS: HOLD REMAINING 50%. Free rolling to Target 2.\n")
      }
    }
    cat("\n")
  }
}

# -------------------------------------------------------------------------
# SECTION B: SCAN FOR NEW OPPORTUNITIES
# -------------------------------------------------------------------------
cat(">>> SCANNING WATCHLIST FOR NEW SETUPS <<<\n")

results <- list()
scan_tickers <- setdiff(all_tickers, active_symbols) # Don't scan things we already own

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
    
    if (rsi >= 30 && !in_rsi_zone) {
      if (is.na(recent_high) || H > recent_high) recent_high <- H
    }
    
    if (rsi < 30) {
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
    
    if (setup_armed && L <= (lowest_low * 0.98)) setup_armed <- FALSE
  }
  
  # Check Final Day Status
  last_row <- sym_data[nrow(sym_data), ]
  status <- "NONE"
  details <- ""
  
  if (setup_armed) {
    if (last_row$close > last_row$SMA10 && last_row$close < last_row$SMA200) {
      stop_loss <- lowest_low * 0.98
      risk_pct <- (last_row$close - stop_loss) / last_row$close
      
      if (risk_pct <= 0.15 && !is.na(swing_high) && swing_high > last_row$close) {
        if (!is.na(last_row$stoch_bull) && last_row$stoch_bull) {
          status <- "ACTION (ENTRY TRIGGERED)"
          details <- paste("Buy Price:", round(last_row$close, 2), 
                           "| Stop Loss:", round(stop_loss, 2), 
                           "| Target 1:", round(swing_high, 2))
          
          # We provide the helper command so the user can easily log it
          details <- paste0(details, "\n   Run: buy_stock('", sym, "', shares=100, entry_price=", 
                            round(last_row$close, 2), ", stop_loss=", round(stop_loss, 2), 
                            ", target1=", round(swing_high, 2), ")")
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
