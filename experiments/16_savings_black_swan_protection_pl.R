# 16_savings_black_swan_protection_pl.R
# Advanced Reversion Strategy - 26-Year Savings with Black Swan Protection (Multiple Entries Only) - Polish Market

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
cat("Fetching historical data (1999-2026) for 50 Polish tickers...\n")
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

# Filter for backtest window: 2000-01-01 to 2026-12-31
df <- df_global %>% filter(date >= as.Date("2000-01-01") & date <= as.Date("2026-12-31"))
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

# Pre-calculate Global Indicators for Black Swan Models
# 1. Market Index (Average price of active tickers) and its 200-day SMA
market_index <- rowMeans(C_mat, na.rm = TRUE)
market_index_sma200 <- SMA(market_index, n = 200)

# 2. Market Breadth (Percentage of tickers above their 200-day SMA)
above_sma200_mat <- C_mat > SMA200_mat
breadth_ratio <- rowMeans(above_sma200_mat, na.rm = TRUE)

# Backtest Function with Black Swan Protection (Multiple Entries Only)
run_savings_backtest <- function(strategy_type = "Multiple_Entries", protection_model = "None") {
  in_rsi_zone <- rep(FALSE, n_symbols)
  setup_armed <- rep(FALSE, n_symbols)
  recent_high <- rep(NA_real_, n_symbols)
  swing_high  <- rep(NA_real_, n_symbols)
  lowest_low  <- rep(NA_real_, n_symbols)
  
  position    <- rep(0, n_symbols)
  shares      <- rep(0, n_symbols)
  stop_loss   <- rep(NA_real_, n_symbols)
  
  global_cash <- 0
  total_savings <- 0
  equity_curve <- numeric(length(dates))
  
  # Protection states
  is_in_crash <- FALSE
  peak_equity <- 0
  
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
    # Calculate portfolio value (before any transaction)
    # ----------------------------------------------------
    portfolio_value <- global_cash
    for (j in 1:n_symbols) {
      if (shares[j] > 0) {
        val <- C_today[j]
        if (is.na(val)) {
          # Use last non-NA close price
          non_na_idx <- which(!is.na(C_mat[1:i, j]))
          val <- if (length(non_na_idx) > 0) C_mat[max(non_na_idx), j] else 0
        }
        portfolio_value <- portfolio_value + (shares[j] * val)
      }
    }
    
    if (portfolio_value > peak_equity) {
      peak_equity <- portfolio_value
    }
    
    # ----------------------------------------------------
    # Black Swan Protection Logic
    # ----------------------------------------------------
    if (protection_model == "Model_A_Drawdown") {
      breadth_today <- breadth_ratio[i]
      if (is_in_crash) {
        # Recovery condition: Breadth back above 40%
        if (!is.na(breadth_today) && breadth_today >= 0.40) {
          is_in_crash <- FALSE
          peak_equity <- portfolio_value # Reset peak
        }
      } else {
        # Drawdown trigger: Portfolio drops > 15% from its peak
        drawdown <- (portfolio_value - peak_equity) / peak_equity
        if (portfolio_value > 0 && !is.nan(drawdown) && drawdown <= -0.15) {
          is_in_crash <- TRUE
          # Liquidate all positions
          global_cash <- portfolio_value
          shares <- rep(0, n_symbols)
          position <- rep(0, n_symbols)
          in_rsi_zone <- rep(FALSE, n_symbols)
          setup_armed <- rep(FALSE, n_symbols)
          recent_high <- rep(NA_real_, n_symbols)
          swing_high  <- rep(NA_real_, n_symbols)
          lowest_low  <- rep(NA_real_, n_symbols)
          peak_equity <- global_cash
        }
      }
    } else if (protection_model == "Model_B_Index_SMA") {
      proxy_today <- market_index[i]
      sma_today <- market_index_sma200[i]
      if (!is.na(proxy_today) && !is.na(sma_today)) {
        if (proxy_today < sma_today) {
          if (!is_in_crash) {
            is_in_crash <- TRUE
            # Liquidate all positions
            global_cash <- portfolio_value
            shares <- rep(0, n_symbols)
            position <- rep(0, n_symbols)
            in_rsi_zone <- rep(FALSE, n_symbols)
            setup_armed <- rep(FALSE, n_symbols)
            recent_high <- rep(NA_real_, n_symbols)
            swing_high  <- rep(NA_real_, n_symbols)
            lowest_low  <- rep(NA_real_, n_symbols)
          }
        } else {
          is_in_crash <- FALSE
        }
      }
    } else if (protection_model == "Model_C_Breadth") {
      breadth_today <- breadth_ratio[i]
      if (is_in_crash) {
        # Recovery condition: Breadth back above 40%
        if (!is.na(breadth_today) && breadth_today >= 0.40) {
          is_in_crash <- FALSE
        }
      } else {
        # Crash trigger: Breadth ratio drops below 20%
        if (!is.na(breadth_today) && breadth_today < 0.20) {
          is_in_crash <- TRUE
          # Liquidate all positions
          global_cash <- portfolio_value
          shares <- rep(0, n_symbols)
          position <- rep(0, n_symbols)
          in_rsi_zone <- rep(FALSE, n_symbols)
          setup_armed <- rep(FALSE, n_symbols)
          recent_high <- rep(NA_real_, n_symbols)
          swing_high  <- rep(NA_real_, n_symbols)
          lowest_low  <- rep(NA_real_, n_symbols)
        }
      }
    }
    
    # ----------------------------------------------------
    # State tracking for setup (Only if not in crash)
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
        stop_loss[j] <- lowest_low[j] * 0.93
      }
    }
    
    # ----------------------------------------------------
    # Strategy implementation (Multiple Entries Reversion)
    # ----------------------------------------------------
    # Indicator-based entry (Only if NOT in crash mode)
    if (!is_in_crash) {
      for (j in active_idx) {
        if (setup_armed[j]) {
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
      
      # Allocate cash to triggers
      if (global_cash > 0 && length(triggered_indices) > 0) {
        cash_per_trade <- global_cash / length(triggered_indices)
        for (j in triggered_indices) {
          entry_px <- C_today[j]
          alloc_shares <- floor(cash_per_trade / entry_px)
          if (alloc_shares > 0) {
            global_cash <- global_cash - (alloc_shares * entry_px)
            shares[j] <- shares[j] + alloc_shares
            setup_armed[j] <- FALSE
          }
        }
      }
    }
    
    # Recalculate portfolio value at the end of the day after transactions
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
  
  label <- if (protection_model == "None") "Baseline" else protection_model
  return(data.frame(
    Date = dates,
    Equity = equity_curve,
    Total_Savings = total_savings,
    Strategy = label,
    stringsAsFactors = FALSE
  ))
}

# 6. Execute Simulations
cat("Running Baseline Multiple Entries (No Protection)...\n")
res_base <- run_savings_backtest("Multiple_Entries", "None")

cat("Running Multiple Entries with Model A (Portfolio Drawdown Protection)...\n")
res_model_a <- run_savings_backtest("Multiple_Entries", "Model_A_Drawdown")

cat("Running Multiple Entries with Model B (Index SMA Filter)...\n")
res_model_b <- run_savings_backtest("Multiple_Entries", "Model_B_Index_SMA")

cat("Running Multiple Entries with Model C (Market Breadth Filter)...\n")
res_model_c <- run_savings_backtest("Multiple_Entries", "Model_C_Breadth")

plot_data <- bind_rows(res_base, res_model_a, res_model_b, res_model_c)

# Calculate final stats and Maximum Drawdown (MDD)
max_drawdown <- function(equity_curve) {
  cum_max <- cummax(equity_curve)
  drawdowns <- (equity_curve - cum_max) / cum_max
  drawdowns[is.nan(drawdowns)] <- 0
  return(min(drawdowns) * 100)
}

stats <- plot_data %>%
  group_by(Strategy) %>%
  summarise(
    Total_Saved = last(Total_Savings),
    Final_Equity = last(Equity),
    Net_Profit = last(Equity) - last(Total_Savings),
    ROI_pct = (last(Equity) / last(Total_Savings) - 1) * 100,
    Max_Drawdown_pct = max_drawdown(Equity),
    .groups = "drop"
  )

cat("\n========================================================================\n")
cat("26-Year Savings Backtest with Black Swan Protection (Multiple Entries):\n")
print(as.data.frame(stats %>% arrange(desc(Final_Equity))))
cat("========================================================================\n")

if(!dir.exists("outputs")) dir.create("outputs")
write.csv(stats, "outputs/portfolio_savings_black_swan_protection_summary.csv", row.names = FALSE)

# 7. Plotting
cat("Generating Black Swan Protection Comparison Chart...\n")
p <- ggplot(plot_data, aes(x = Date, y = Equity, color = Strategy)) +
  geom_line(linewidth = 1) +
  geom_line(aes(y = Total_Savings), color = "black", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = min(dates) + 2000, y = 100000, label = "Total Savings Contribution", color = "black", angle = 22) +
  theme_minimal() +
  labs(title = "Black Swan Protection - Multiple Entries Savings Backtest (2000-2026)",
       subtitle = "Comparing Portfolio Drawdown, Index SMA, and Breadth Liquidation Filters",
       y = "Portfolio Equity (PLN)",
       x = "Date",
       color = "Strategy Variant") +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14)) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " PLN"))

ggsave("outputs/portfolio_savings_black_swan_protection_pl.png", plot = p, width = 13, height = 8)
cat("Done! Chart saved to outputs/portfolio_savings_black_swan_protection_pl.png\n")
