# 17_savings_10yr_stop_7pct_pl.R
# Advanced Reversion Strategy - 10-Year Savings with permanent -7% Stop Loss (2016-2026) - Polish Market

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
cat("Fetching 10-year history (2015-2026) for 50 Polish tickers...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = "2015-01-01", to = "2026-12-31")

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

# Filter for the last 10 years (2016-2026)
df <- df_global %>% filter(date >= as.Date("2016-01-01") & date <= as.Date("2026-12-31"))
dates <- sort(unique(df$date))

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

# Backtest Function
run_savings_backtest <- function(strategy_type) {
  in_rsi_zone <- rep(FALSE, n_symbols)
  setup_armed <- rep(FALSE, n_symbols)
  recent_high <- rep(NA_real_, n_symbols)
  swing_high  <- rep(NA_real_, n_symbols)
  lowest_low  <- rep(NA_real_, n_symbols)
  
  position    <- rep(0, n_symbols)
  shares      <- rep(0, n_symbols)
  entry_price <- rep(NA_real_, n_symbols)
  stop_loss   <- rep(NA_real_, n_symbols)
  setup_stop_loss <- rep(NA_real_, n_symbols)
  
  global_cash <- 0
  total_savings <- 0
  equity_curve <- numeric(length(dates))
  
  for (i in 1:length(dates)) {
    # Monthly cash addition (2000 PLN)
    current_month <- format(dates[i], "%Y-%m")
    prev_month <- if (i > 1) format(dates[i-1], "%Y-%m") else ""
    if (current_month != prev_month) {
      global_cash <- global_cash + 2000
      total_savings <- total_savings + 2000
    }
    
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
    
    # ----------------------------------------------------
    # State tracking & exits
    # ----------------------------------------------------
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
        setup_stop_loss[j] <- lowest_low[j] * 0.93
      }
      
      # Exit logic for active positions
      if (shares[j] > 0 && strategy_type %in% c("Single_Entry", "Multiple_Entries")) {
        if (L_today[j] <= stop_loss[j]) {
          exec_price <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
          global_cash <- global_cash + (shares[j] * exec_price)
          shares[j] <- 0
          position[j] <- 0
          entry_price[j] <- NA
          stop_loss[j] <- NA
        }
      }
    }
    
    # ----------------------------------------------------
    # Strategy implementation
    # ----------------------------------------------------
    if (strategy_type == "Blind_DCA_Monthly") {
      # Benchmark DCA: Invest the 2000 PLN savings equally among all active stocks on the first day of each month
      if (current_month != prev_month && length(active_idx) > 0) {
        cash_per_stock <- 2000 / length(active_idx)
        for (j in active_idx) {
          entry_px <- C_today[j]
          alloc_shares <- floor(cash_per_stock / entry_px)
          if (alloc_shares > 0) {
            global_cash <- global_cash - (alloc_shares * entry_px)
            shares[j] <- shares[j] + alloc_shares
          }
        }
      }
    } else if (strategy_type == "WIG20_DCA_Monthly") {
      # WIG20 DCA: Invest the 2000 PLN savings equally among the 4 core blue chips
      if (current_month != prev_month) {
        wig_stocks <- c("PEO.WA", "KGH.WA", "PKN.WA", "OPL.WA")
        cash_per_stock <- 2000 / length(wig_stocks)
        for (sym in wig_stocks) {
          j <- which(symbols == sym)
          if (length(j) > 0) {
            entry_px <- C_today[j]
            if (!is.na(entry_px)) {
              alloc_shares <- floor(cash_per_stock / entry_px)
              if (alloc_shares > 0) {
                global_cash <- global_cash - (alloc_shares * entry_px)
                shares[j] <- shares[j] + alloc_shares
              }
            }
          }
        }
      }
    } else {
      # Indicator-based entry
      for (j in active_idx) {
        if (setup_armed[j]) {
          if (L_today[j] <= setup_stop_loss[j]) {
            setup_armed[j] <- FALSE
          } else if (C_today[j] > sma20[j] && C_today[j] < sma200[j]) {
            risk_pct <- (C_today[j] - setup_stop_loss[j]) / C_today[j]
            if (risk_pct <= 0.15 && !is.na(swing_high[j]) && swing_high[j] > C_today[j]) {
              if (!is.na(stoch_today[j]) && stoch_today[j]) {
                if (strategy_type == "Single_Entry") {
                  if (shares[j] == 0) triggered_indices <- c(triggered_indices, j)
                } else if (strategy_type == "Multiple_Entries") {
                  triggered_indices <- c(triggered_indices, j)
                }
              }
            } else {
              setup_armed[j] <- FALSE
            }
          }
        }
      }
      
      # Allocate cash to triggers
      if (global_cash > 0 && length(triggered_indices) > 0) {
        cash_per_trade <- global_cash / length(triggered_indices)
        for (j in triggered_indices) {
          entry_px <- C_today[j]
          alloc_shares <- floor(cash_per_trade / entry_px)
          if (alloc_shares > 0) {
            global_cash <- global_cash - (alloc_shares * entry_px)
            
            # Update entry price and active stop loss (-7% from entry point)
            if (shares[j] == 0) {
              entry_price[j] <- entry_px
            } else {
              entry_price[j] <- (shares[j] * entry_price[j] + alloc_shares * entry_px) / (shares[j] + alloc_shares)
            }
            stop_loss[j] <- entry_price[j] * 0.93
            
            shares[j] <- shares[j] + alloc_shares
            setup_armed[j] <- FALSE
          }
        }
      }
    }
    
    # Portfolio equity
    portfolio_value <- global_cash
    for (j in 1:n_symbols) {
      if (shares[j] > 0) {
        val <- C_today[j]
        if (is.na(val)) {
          non_na_idx <- which(!is.na(C_mat[1:i, j]))
          val <- if (length(non_na_idx) > 0) C_mat[max(non_na_idx), j] else 0
        }
        portfolio_value <- portfolio_value + (shares[j] * val)
      }
    }
    equity_curve[i] <- portfolio_value
  }
  
  return(data.frame(
    Date = dates,
    Equity = equity_curve,
    Total_Savings = total_savings,
    Strategy = strategy_type,
    stringsAsFactors = FALSE
  ))
}

# Run all four
cat("Running Blind DCA Benchmark...\n")
res_dca <- run_savings_backtest("Blind_DCA_Monthly")

cat("Running WIG20 DCA Benchmark...\n")
res_wig20 <- run_savings_backtest("WIG20_DCA_Monthly")

cat("Running Single Entry Reversion (with -7% Stop)...\n")
res_single <- run_savings_backtest("Single_Entry")

cat("Running Multiple Entries Reversion (with -7% Stop)...\n")
res_mult <- run_savings_backtest("Multiple_Entries")

plot_data <- bind_rows(res_dca, res_wig20, res_single, res_mult)

# Calculate final stats
stats <- plot_data %>%
  group_by(Strategy) %>%
  slice(n()) %>%
  mutate(
    Net_Profit = Equity - Total_Savings,
    ROI_pct = (Equity / Total_Savings - 1) * 100
  ) %>%
  select(Strategy, Total_Saved = Total_Savings, Final_Equity = Equity, Net_Profit, ROI_pct)

cat("\n========================================================================\n")
cat("10-Year Savings Backtest Results with -7% Stop Loss (2016-2026):\n")
print(as.data.frame(stats))
cat("========================================================================\n")

# Plotting
cat("Generating Savings Backtest Chart...\n")
p <- ggplot(plot_data, aes(x = Date, y = Equity, color = Strategy)) +
  geom_line(linewidth = 1) +
  geom_line(aes(y = Total_Savings), color = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = min(dates) + 500, y = 30000, label = "Total Savings Contribution", color = "black", angle = 12) +
  theme_minimal() +
  labs(title = "10-Year Savings Performance with permanent -7% Stop - Polish Market",
       subtitle = "Saving 2,000 PLN/month starting with 0 PLN. Exit at -7% stop from entry price.",
       y = "Portfolio Equity (PLN)",
       x = "Date",
       color = "Strategy Type") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " PLN"))

if(!dir.exists("outputs")) dir.create("outputs")
ggsave("outputs/portfolio_savings_10yr_stop_7pct_pl.png", plot = p, width = 12, height = 7)
cat("Done! Chart saved to outputs/portfolio_savings_10yr_stop_7pct_pl.png\n")
