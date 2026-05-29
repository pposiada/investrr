# 14_savings_10yr_ath_stop_grid_pl.R
# Advanced Reversion Strategy - 10-Year Savings post-ATH Stop Loss Grid Search - Polish Market

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
    # Cumulative maximum of high up to the PREVIOUS day (the ATH to break)
    ATH = lag(cummax(high))
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
ATH_mat <- make_wide("ATH")

symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# Backtest Function
run_savings_backtest <- function(strategy_type, stop_loss_pct) {
  in_rsi_zone <- rep(FALSE, n_symbols)
  setup_armed <- rep(FALSE, n_symbols)
  recent_high <- rep(NA_real_, n_symbols)
  swing_high  <- rep(NA_real_, n_symbols)
  lowest_low  <- rep(NA_real_, n_symbols)
  
  position    <- rep(0, n_symbols)
  shares      <- rep(0, n_symbols)
  entry_price <- rep(NA_real_, n_symbols)
  stop_loss   <- rep(NA_real_, n_symbols)
  
  # Trailing stop after ATH activation state
  stop_loss_active <- rep(FALSE, n_symbols)
  highest_price_since_activation <- rep(NA_real_, n_symbols)
  
  global_cash <- 0
  total_savings <- 0
  
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
    ath_today <- ATH_mat[i, ]
    
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
        stop_loss[j] <- lowest_low[j] * 0.93
      }
      
      # Exit logic for active positions
      if (shares[j] > 0) {
        ath_val <- ath_today[j]
        
        if (stop_loss_active[j]) {
          # Update the highest price seen since activation
          if (H_today[j] > highest_price_since_activation[j]) {
            highest_price_since_activation[j] <- H_today[j]
          }
          current_stop <- highest_price_since_activation[j] * (1 - stop_loss_pct)
          
          # Check trailing stop loss exit
          if (L_today[j] <= current_stop) {
            exec_price <- ifelse(O_today[j] < current_stop, O_today[j], current_stop)
            global_cash <- global_cash + (shares[j] * exec_price)
            shares[j] <- 0
            position[j] <- 0
            stop_loss_active[j] <- FALSE
            highest_price_since_activation[j] <- NA
          }
        } else {
          # Check if we hit the all-time high
          if (!is.na(ath_val) && H_today[j] >= ath_val) {
            stop_loss_active[j] <- TRUE
            highest_price_since_activation[j] <- max(H_today[j], ath_val)
            current_stop <- highest_price_since_activation[j] * (1 - stop_loss_pct)
            
            # Check if stopped out on the activation day itself
            if (L_today[j] <= current_stop) {
              exec_price <- ifelse(O_today[j] < current_stop, O_today[j], current_stop)
              global_cash <- global_cash + (shares[j] * exec_price)
              shares[j] <- 0
              position[j] <- 0
              stop_loss_active[j] <- FALSE
              highest_price_since_activation[j] <- NA
            }
          }
        }
      }
    }
    
    # ----------------------------------------------------
    # Strategy implementation (Indicator entries)
    # ----------------------------------------------------
    for (j in active_idx) {
      if (setup_armed[j]) {
        if (L_today[j] <= stop_loss[j]) {
          setup_armed[j] <- FALSE
        } else if (C_today[j] > sma20[j] && C_today[j] < sma200[j]) {
          risk_pct <- (C_today[j] - stop_loss[j]) / C_today[j]
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
          shares[j] <- shares[j] + alloc_shares
          setup_armed[j] <- FALSE
        }
      }
    }
  }
  
  # Calculate final equity value
  portfolio_value <- global_cash
  for (j in 1:n_symbols) {
    if (shares[j] > 0) {
      last_price <- C_mat[nrow(C_mat), j]
      if (!is.na(last_price)) {
        portfolio_value <- portfolio_value + (shares[j] * last_price)
      }
    }
  }
  
  return(portfolio_value)
}

# 3. Grid Search Execution
stop_pct_vals <- c(0.01, 0.02, 0.03, 0.05, 0.07, 0.10, 0.15, 0.20)
strategies <- c("Single_Entry", "Multiple_Entries")

grid_results <- list()

for (strat in strategies) {
  for (stop_pct in stop_pct_vals) {
    cat(sprintf("Running grid backtest for Strategy: %s | Stop-Loss: %.1f%% ...\n", strat, stop_pct * 100))
    final_eq <- run_savings_backtest(strat, stop_pct)
    
    grid_results[[length(grid_results) + 1]] <- data.frame(
      Strategy = strat,
      Stop_Loss_Pct = stop_pct * 100,
      Total_Savings = 250000,
      Final_Equity = final_eq,
      Net_Profit = final_eq - 250000,
      ROI_pct = (final_eq / 250000 - 1) * 100,
      stringsAsFactors = FALSE
    )
  }
}

summary_df <- bind_rows(grid_results)

cat("\n========================================================================\n")
cat("Grid Search Results: Post-ATH Trailing Stop Optimization (10-Year PL):\n")
print(as.data.frame(summary_df %>% arrange(desc(Final_Equity))))
cat("========================================================================\n")

if(!dir.exists("outputs")) dir.create("outputs")
write.csv(summary_df, "outputs/portfolio_savings_ath_stop_grid_summary.csv", row.names = FALSE)

# 4. Plotting
cat("Generating Grid Search Plot...\n")
p <- ggplot(summary_df, aes(x = factor(Stop_Loss_Pct), y = Final_Equity, fill = Strategy)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  geom_hline(yintercept = 250000, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = 1.5, y = 265000, label = "Principal Contribution (250k)", color = "red") +
  theme_minimal() +
  labs(title = "Post-ATH Trailing Stop Optimization - 10-Year Savings",
       subtitle = "Comparing Stop Pct (1% to 20%) in Single & Multiple Entry Models",
       y = "Final Portfolio Equity (PLN)",
       x = "Trailing Stop Loss Percentage (%)",
       fill = "Strategy Type") +
  theme(legend.position = "bottom") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "", suffix = " PLN"))

ggsave("outputs/portfolio_savings_ath_stop_grid_pl.png", plot = p, width = 12, height = 7)
cat("Done! Chart saved to outputs/portfolio_savings_ath_stop_grid_pl.png\n")
