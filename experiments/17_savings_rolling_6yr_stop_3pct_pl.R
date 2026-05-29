# 17_savings_rolling_6yr_stop_3pct_pl.R
# Advanced Reversion Strategy - 6-Year Rolling Periods Savings Backtest with 3% & 7% Stop Loss (Multiple Entries Only)

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

# 2. Data Wrangling (Fetch long history)
cat("Fetching long-term history (2000-2026) for 50 Polish tickers...\n")
# Fetch starting from 1999 to give room for calculations
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

# 4. Backtester Wrapper Function
run_period_savings_backtest <- function(p_name, p_start, p_end, stop_loss_pct = NULL) {
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
      if (!is.null(stop_loss_pct) && shares[j] > 0) {
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
    for (j in active_idx) {
      if (setup_armed[j]) {
        if (L_today[j] <= setup_stop_loss[j]) {
          setup_armed[j] <- FALSE
        } else if (C_today[j] > sma20[j] && C_today[j] < sma200[j]) {
          risk_pct <- (C_today[j] - setup_stop_loss[j]) / C_today[j]
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
    
    # Allocate cash to triggers
    if (global_cash > 0 && length(triggered_indices) > 0) {
      cash_per_trade <- global_cash / length(triggered_indices)
      for (j in triggered_indices) {
        entry_px <- C_today[j]
        alloc_shares <- floor(cash_per_trade / entry_px)
        if (alloc_shares > 0) {
          global_cash <- global_cash - (alloc_shares * entry_px)
          
          # Update entry price and active stop loss
          if (shares[j] == 0) {
            entry_price[j] <- entry_px
          } else {
            entry_price[j] <- (shares[j] * entry_price[j] + alloc_shares * entry_px) / (shares[j] + alloc_shares)
          }
          
          if (!is.null(stop_loss_pct)) {
            stop_loss[j] <- entry_price[j] * (1 - stop_loss_pct)
          }
          
          shares[j] <- shares[j] + alloc_shares
          setup_armed[j] <- FALSE
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
  
  label <- if(is.null(stop_loss_pct)) "Baseline_No_Exit" else paste0("Stop_", stop_loss_pct * 100, "pct")
  res <- data.frame(
    TradingDay = 1:length(dates),
    RealDate = dates,
    Equity = equity_curve,
    Total_Savings = total_savings,
    Period = p_name,
    Strategy = label,
    stringsAsFactors = FALSE
  )
  return(res)
}

# 5. Execute Loop
all_results <- list()

for (p in periods) {
  # Run baseline (No Exit)
  cat("Running Baseline Multiple Entries for period:", p$name, "...\n")
  res_base <- run_period_savings_backtest(p$name, p$start, p$end, stop_loss_pct = NULL)
  if (!is.null(res_base)) {
    all_results[[paste(p$name, "Baseline", sep = "_")]] <- res_base
  }
  
  # Run 3% Stop Loss
  cat("Running 3% Stop-Loss Multiple Entries for period:", p$name, "...\n")
  res_stop3 <- run_period_savings_backtest(p$name, p$start, p$end, stop_loss_pct = 0.03)
  if (!is.null(res_stop3)) {
    all_results[[paste(p$name, "Stop3pct", sep = "_")]] <- res_stop3
  }
  
  # Run 7% Stop Loss
  cat("Running 7% Stop-Loss Multiple Entries for period:", p$name, "...\n")
  res_stop7 <- run_period_savings_backtest(p$name, p$start, p$end, stop_loss_pct = 0.07)
  if (!is.null(res_stop7)) {
    all_results[[paste(p$name, "Stop7pct", sep = "_")]] <- res_stop7
  }
}

plot_data <- bind_rows(all_results) %>% filter(!is.na(Equity) & Equity > 0)

# Calculate final equities and savings
final_stats <- plot_data %>%
  group_by(Period, Strategy) %>%
  slice(n()) %>%
  select(Period, Strategy, Total_Saved = Total_Savings, Final_Equity = Equity) %>%
  ungroup()

# Display summary in wide format
final_equities_wide <- final_stats %>%
  select(-Total_Saved) %>%
  pivot_wider(names_from = Strategy, values_from = Final_Equity)

total_saved_wide <- final_stats %>%
  select(Period, Total_Saved) %>%
  distinct()

summary_table <- total_saved_wide %>%
  left_join(final_equities_wide, by = "Period")

cat("\n========================================================================\n")
cat("Final Equities & Contributions (PLN) by 6-Year Rolling Period (Multiple Entries):\n")
print(as.data.frame(summary_table))
cat("========================================================================\n")

if(!dir.exists("outputs")) dir.create("outputs")
write.csv(summary_table, "outputs/portfolio_rolling_6yr_savings_stop_comparison_summary.csv", row.names = FALSE)

# 6. Plotting
cat("Generating Rolling Savings Strategy Comparison Chart...\n")
p <- ggplot(plot_data, aes(x = TradingDay, y = Equity, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  geom_line(aes(y = Total_Savings), color = "black", linetype = "dashed", linewidth = 0.5) +
  facet_wrap(~ Period, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(title = "Rolling 6-Year Savings Backtest with Stop-Loss - Polish Market",
       subtitle = "Multiple Entries Reversion Strategy (2,000 PLN/month). Comparing 3% & 7% Stop-Loss vs. Baseline (No Exit).",
       y = "Portfolio Equity (PLN)",
       x = "Trading Days Elapsed",
       color = "Strategy Type") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#f0f0f0", color = NA),
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " PLN"))

ggsave("outputs/portfolio_rolling_6yr_savings_stop_comparison_pl.png", plot = p, width = 14, height = 9)
cat("Done! Chart saved to outputs/portfolio_rolling_6yr_savings_stop_comparison_pl.png\n")
