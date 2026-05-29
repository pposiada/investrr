# 13_exit_strategies_comparison_10yr_pl.R
# Advanced Reversion Strategy - Exit Strategy Comparison (2016-2026) - Polish Market

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
    stoch_bull = fastK > fastD,
    # Volatility indicator for Chandelier Exit: 22-day ATR
    ATR22 = ATR(cbind(high, low, close), n = 22)[, "atr"]
  ) %>%
  select(-L10, -H10, -fastK, -fastD) %>%
  ungroup()

# Filter for backtest window (2016-2026)
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
ATR_mat <- make_wide("ATR22")

symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# Backtest Function
run_exit_backtest <- function(strategy_type) {
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
  
  # For Chandelier Exit: track highest high seen since entry
  highest_high_held <- rep(NA_real_, n_symbols)
  
  global_cash <- 100000
  equity_curve <- numeric(length(dates))
  
  trade_log <- list()
  
  for (i in 1:length(dates)) {
    O_today <- O_mat[i, ]
    H_today <- H_mat[i, ]
    L_today <- L_mat[i, ]
    C_today <- C_mat[i, ]
    rsi <- RSI_mat[i, ]
    sma20 <- SMA20_mat[i, ]
    sma200 <- SMA200_mat[i, ]
    stoch_today <- stoch_mat[i, ]
    atr_today <- ATR_mat[i, ]
    
    active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
    triggered_indices <- integer(0)
    
    for (j in active_idx) {
      # ----------------------------------------------------
      # Track RSI zone
      # ----------------------------------------------------
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
      
      # ----------------------------------------------------
      # Check exits
      # ----------------------------------------------------
      if (position[j] > 0) {
        # Update highest high seen during trade
        if (H_today[j] > highest_high_held[j]) highest_high_held[j] <- H_today[j]
        
        # Check catastrophic stop loss (always active)
        initial_stop <- stop_loss[j]
        exec_price_stop <- ifelse(O_today[j] < initial_stop, O_today[j], initial_stop)
        
        if (L_today[j] <= initial_stop) {
          # Liquidate all
          global_cash <- global_cash + (shares[j] * exec_price_stop)
          trade_log[[length(trade_log) + 1]] <- data.frame(
            Symbol = symbols[j], Entry = entry_price[j], Exit = exec_price_stop, Return = (exec_price_stop / entry_price[j] - 1),
            stringsAsFactors = FALSE
          )
          shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
        } else {
          # Strategy-specific exit rules
          if (strategy_type == "Baseline") {
            # Target 1 (50% position sell)
            if (position[j] == 1 && H_today[j] >= target1[j]) {
              exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
              sell_shares <- floor(shares[j] / 2)
              global_cash <- global_cash + (sell_shares * exec_price_t1)
              shares[j] <- shares[j] - sell_shares
              position[j] <- 0.5
              stop_loss[j] <- entry_price[j] # Move stop to breakeven
            }
            # Target 2 (SMA 200 Reversion)
            if (position[j] == 0.5 && H_today[j] >= sma200[j]) {
              exec_price_t2 <- ifelse(O_today[j] > sma200[j], O_today[j], sma200[j])
              global_cash <- global_cash + (shares[j] * exec_price_t2)
              trade_log[[length(trade_log) + 1]] <- data.frame(
                Symbol = symbols[j], Entry = entry_price[j], Exit = exec_price_t2, Return = (exec_price_t2 / entry_price[j] - 1),
                stringsAsFactors = FALSE
              )
              shares[j] <- 0; position[j] <- 0
            }
          } else if (strategy_type == "QS_Exit") {
            # QS Exit: Exit at close if Close_today > High_yesterday
            if (i > 1 && C_today[j] > H_mat[i-1, j]) {
              exec_price_qs <- C_today[j]
              global_cash <- global_cash + (shares[j] * exec_price_qs)
              trade_log[[length(trade_log) + 1]] <- data.frame(
                Symbol = symbols[j], Entry = entry_price[j], Exit = exec_price_qs, Return = (exec_price_qs / entry_price[j] - 1),
                stringsAsFactors = FALSE
              )
              shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
            }
          } else if (strategy_type == "Chandelier") {
            # Chandelier Exit: Trail at Highest High - 3 * ATR
            if (!is.na(atr_today[j])) {
              chandelier_stop <- highest_high_held[j] - 3.0 * atr_today[j]
              # Only active if it's higher than the initial stop
              effective_stop <- max(initial_stop, chandelier_stop)
              
              if (L_today[j] <= effective_stop) {
                exec_price_chan <- ifelse(O_today[j] < effective_stop, O_today[j], effective_stop)
                global_cash <- global_cash + (shares[j] * exec_price_chan)
                trade_log[[length(trade_log) + 1]] <- data.frame(
                  Symbol = symbols[j], Entry = entry_price[j], Exit = exec_price_chan, Return = (exec_price_chan / entry_price[j] - 1),
                  stringsAsFactors = FALSE
                )
                shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
              }
            }
          } else if (strategy_type == "Fixed_Target_20") {
            # Fixed Target 20%: Exit 100% when high >= entry * 1.20
            target_px <- entry_price[j] * 1.20
            if (H_today[j] >= target_px) {
              exec_price_t <- ifelse(O_today[j] > target_px, O_today[j], target_px)
              global_cash <- global_cash + (shares[j] * exec_price_t)
              trade_log[[length(trade_log) + 1]] <- data.frame(
                Symbol = symbols[j], Entry = entry_price[j], Exit = exec_price_t, Return = (exec_price_t / entry_price[j] - 1),
                stringsAsFactors = FALSE
              )
              shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
            }
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
    
    # Position allocation
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
          highest_high_held[j] <- entry_px
        }
      }
    }
    
    # Portfolio equity mark-to-market
    portfolio_value <- global_cash
    for (j in active_idx) {
      if (shares[j] > 0) portfolio_value <- portfolio_value + (shares[j] * C_today[j])
    }
    equity_curve[i] <- portfolio_value
  }
  
  # Calculate daily returns and Sortino ratio
  daily_returns <- diff(equity_curve) / equity_curve[-length(equity_curve)]
  downside_rets <- daily_returns[daily_returns < 0]
  downside_dev <- if(length(downside_rets) > 0) sqrt(mean(downside_rets^2)) else 0
  sortino <- if(downside_dev > 0) (mean(daily_returns) / downside_dev) * sqrt(252) else NA
  
  # Trade log stats
  trade_df <- bind_rows(trade_log)
  trade_count <- nrow(trade_df)
  win_rate <- if (trade_count > 0) mean(trade_df$Return > 0) * 100 else NA
  
  res_stats <- data.frame(
    Strategy = strategy_type,
    Final_Equity = equity_curve[length(equity_curve)],
    ROI_pct = (equity_curve[length(equity_curve)] / 100000 - 1) * 100,
    Trade_Count = trade_count,
    Win_Rate_pct = win_rate,
    Sortino = sortino,
    stringsAsFactors = FALSE
  )
  
  return(list(
    stats = res_stats,
    equity = data.frame(
      Date = dates,
      Equity = equity_curve,
      Strategy = strategy_type,
      stringsAsFactors = FALSE
    )
  ))
}

# Run all four
cat("Running Baseline exit backtest...\n")
run_base <- run_exit_backtest("Baseline")

cat("Running QS Exit backtest...\n")
run_qs <- run_exit_backtest("QS_Exit")

cat("Running Chandelier exit backtest...\n")
run_chan <- run_exit_backtest("Chandelier")

cat("Running Fixed Target 20% exit backtest...\n")
run_fixed <- run_exit_backtest("Fixed_Target_20")

# Aggregate results
stats_comparison <- bind_rows(run_base$stats, run_qs$stats, run_chan$stats, run_fixed$stats)
equity_curves <- bind_rows(run_base$equity, run_qs$equity, run_chan$equity, run_fixed$equity)

cat("\n========================================================================\n")
cat("10-Year Exit Strategy Comparison Results (2016-2026) - Polish Market:\n")
print(as.data.frame(stats_comparison))
cat("========================================================================\n")

# Plotting
cat("Generating Exit Strategy Comparison Chart...\n")
p <- ggplot(equity_curves, aes(x = Date, y = Equity, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  labs(title = "10-Year Exit Strategy Comparison - Polish Market (2016-2026)",
       subtitle = "Using exact same entry setup (RSI cycle, SMA20/200, Stochastic)",
       y = "Portfolio Equity (PLN)",
       x = "Date",
       color = "Exit Rule Type") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " PLN"))

if(!dir.exists("outputs")) dir.create("outputs")
ggsave("outputs/portfolio_exits_comparison_10yr_pl.png", plot = p, width = 12, height = 7)
cat("Done! Chart saved to outputs/portfolio_exits_comparison_10yr_pl.png\n")
