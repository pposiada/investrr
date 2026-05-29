# 05.5_advanced_strategy_stoch_opt.R
# Parameter Optimization for Stochastic Filter (Multi-Asset Backtest)

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyquant)
library(purrr)

# 1. Universe Definition
tickers <- c(
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

# 2. Data Wrangling
cat("Fetching daily data for 100 tickers...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = "2018-01-01", to = "2026-12-31")

cat("Calculating grid of Stochastic Indicators...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    # Core Strategy Indicators
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200),
    RSI16 = RSI(close, n = 16),
    
    # Grid of Stochastic Variations (fastK > fastD)
    stoch_5_3 = stoch(cbind(high, low, close), nFastK = 5, nFastD = 3)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 5, nFastD = 3)[,"fastD"],
    stoch_5_5 = stoch(cbind(high, low, close), nFastK = 5, nFastD = 5)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 5, nFastD = 5)[,"fastD"],
    stoch_5_7 = stoch(cbind(high, low, close), nFastK = 5, nFastD = 7)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 5, nFastD = 7)[,"fastD"],
    
    stoch_7_3 = stoch(cbind(high, low, close), nFastK = 7, nFastD = 3)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 7, nFastD = 3)[,"fastD"],
    stoch_7_5 = stoch(cbind(high, low, close), nFastK = 7, nFastD = 5)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 7, nFastD = 5)[,"fastD"],
    stoch_7_7 = stoch(cbind(high, low, close), nFastK = 7, nFastD = 7)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 7, nFastD = 7)[,"fastD"],
    
    stoch_10_3 = stoch(cbind(high, low, close), nFastK = 10, nFastD = 3)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 10, nFastD = 3)[,"fastD"],
    stoch_10_5 = stoch(cbind(high, low, close), nFastK = 10, nFastD = 5)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 10, nFastD = 5)[,"fastD"],
    stoch_10_7 = stoch(cbind(high, low, close), nFastK = 10, nFastD = 7)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 10, nFastD = 7)[,"fastD"],
    
    stoch_14_3 = stoch(cbind(high, low, close), nFastK = 14, nFastD = 3)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 14, nFastD = 3)[,"fastD"],
    stoch_14_5 = stoch(cbind(high, low, close), nFastK = 14, nFastD = 5)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 14, nFastD = 5)[,"fastD"],
    stoch_14_7 = stoch(cbind(high, low, close), nFastK = 14, nFastD = 7)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 14, nFastD = 7)[,"fastD"],
    
    stoch_21_3 = stoch(cbind(high, low, close), nFastK = 21, nFastD = 3)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 21, nFastD = 3)[,"fastD"],
    stoch_21_5 = stoch(cbind(high, low, close), nFastK = 21, nFastD = 5)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 21, nFastD = 5)[,"fastD"],
    stoch_21_7 = stoch(cbind(high, low, close), nFastK = 21, nFastD = 7)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 21, nFastD = 7)[,"fastD"],
    
    stoch_28_3 = stoch(cbind(high, low, close), nFastK = 28, nFastD = 3)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 28, nFastD = 3)[,"fastD"],
    stoch_28_5 = stoch(cbind(high, low, close), nFastK = 28, nFastD = 5)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 28, nFastD = 5)[,"fastD"],
    stoch_28_7 = stoch(cbind(high, low, close), nFastK = 28, nFastD = 7)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 28, nFastD = 7)[,"fastD"]
  ) %>%
  ungroup()

dates <- sort(unique(df$date))

make_wide <- function(col_name) {
  df %>% select(date, symbol, all_of(col_name)) %>% 
    pivot_wider(names_from = symbol, values_from = all_of(col_name)) %>% 
    arrange(date) %>% select(-date) %>% as.matrix()
}

cat("Pivoting data to wide format...\n")
O_mat <- make_wide("open")
H_mat <- make_wide("high")
L_mat <- make_wide("low")
C_mat <- make_wide("close")
SMA10_mat <- make_wide("SMA10")
SMA200_mat <- make_wide("SMA200")
RSI_mat <- make_wide("RSI16")

param_names <- c(
  "stoch_5_3", "stoch_5_5", "stoch_5_7",
  "stoch_7_3", "stoch_7_5", "stoch_7_7",
  "stoch_10_3", "stoch_10_5", "stoch_10_7",
  "stoch_14_3", "stoch_14_5", "stoch_14_7",
  "stoch_21_3", "stoch_21_5", "stoch_21_7",
  "stoch_28_3", "stoch_28_5", "stoch_28_7"
)

indicator_matrices <- list()
for (ind in param_names) {
  indicator_matrices[[ind]] <- make_wide(ind)
}

symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# 3. Encapsulated Backtest Function
run_backtest <- function(ind_name = "Baseline") {
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
  
  global_cash <- 100000
  equity_curve <- numeric(length(dates))
  
  if (ind_name != "Baseline") {
    ind_mat <- indicator_matrices[[ind_name]]
  }
  
  for (i in 201:length(dates)) {
    O_today <- O_mat[i, ]
    H_today <- H_mat[i, ]
    L_today <- L_mat[i, ]
    C_today <- C_mat[i, ]
    rsi <- RSI_mat[i, ]
    sma10 <- SMA10_mat[i, ]
    sma200 <- SMA200_mat[i, ]
    
    active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
    triggered_indices <- integer(0)
    
    for (j in active_idx) {
      # PHASE A: Track Support & Resistance
      if (rsi[j] >= 30 && !in_rsi_zone[j]) {
        if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) recent_high[j] <- H_today[j]
      }
      
      if (rsi[j] < 30) {
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
        stop_loss[j] <- lowest_low[j] * 0.98
      }
      
      # PHASE B: Exits
      if (position[j] > 0) {
        exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
        
        if (L_today[j] <= stop_loss[j]) {
          global_cash <- global_cash + (shares[j] * exec_price_stop)
          shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
        } else {
          if (position[j] == 1 && H_today[j] >= target1[j]) {
            exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
            sell_shares <- floor(shares[j] / 2)
            global_cash <- global_cash + (sell_shares * exec_price_t1)
            shares[j] <- shares[j] - sell_shares
            position[j] <- 0.5
            stop_loss[j] <- entry_price[j]
          }
          if (position[j] == 0.5 && H_today[j] >= sma200[j]) {
            exec_price_t2 <- ifelse(O_today[j] > sma200[j], O_today[j], sma200[j])
            global_cash <- global_cash + (shares[j] * exec_price_t2)
            shares[j] <- 0; position[j] <- 0
          }
        }
      } 
      # PHASE C: Entry Check
      else if (position[j] == 0 && setup_armed[j]) {
        if (L_today[j] <= stop_loss[j]) {
          setup_armed[j] <- FALSE
        } else if (C_today[j] > sma10[j] && C_today[j] < sma200[j]) {
          risk_pct <- (C_today[j] - stop_loss[j]) / C_today[j]
          if (risk_pct <= 0.15) {
             if (!is.na(swing_high[j]) && swing_high[j] > C_today[j]) {
                
                passed_filter <- TRUE
                if (ind_name != "Baseline") {
                  ind_val <- ind_mat[i, j]
                  if (is.na(ind_val) || !ind_val) passed_filter <- FALSE
                }
                
                if (passed_filter) triggered_indices <- c(triggered_indices, j)
             }
          } else {
            setup_armed[j] <- FALSE
          }
        }
      }
    }
    
    # PHASE D: Allocate Cash
    if (global_cash > 0 && length(triggered_indices) > 0) {
      cash_per_trade <- global_cash / length(triggered_indices)
      for (j in triggered_indices) {
        entry_px <- C_today[j]
        alloc_shares <- floor(cash_per_trade / entry_px)
        if (alloc_shares > 0) {
          global_cash <- global_cash - (alloc_shares * entry_px)
          shares[j] <- alloc_shares
          position[j] <- 1; entry_price[j] <- entry_px
          target1[j] <- swing_high[j]; setup_armed[j] <- FALSE
        }
      }
    }
    
    # Mark to Market
    portfolio_value <- global_cash
    for (j in active_idx) {
      if (shares[j] > 0) portfolio_value <- portfolio_value + (shares[j] * C_today[j])
    }
    equity_curve[i] <- portfolio_value
  }
  
  return(data.frame(Date = dates, Equity = equity_curve, Indicator = ind_name, stringsAsFactors = FALSE))
}

# 4. Running Optimization Loop
all_results <- list()
cat("\nRunning Baseline Strategy...\n")
all_results[["Baseline"]] <- run_backtest("Baseline")

for (ind in param_names) {
  cat(sprintf("Running Optimization: %s...\n", ind))
  all_results[[ind]] <- run_backtest(ind)
}

plot_data <- bind_rows(all_results) %>% filter(!is.na(Equity) & Equity > 0)

# Identify the absolute best performing parameter set
final_equity <- plot_data %>%
  group_by(Indicator) %>%
  slice(n()) %>%
  arrange(desc(Equity))

best_indicator <- final_equity$Indicator[1]

cat("\n============================================\n")
cat("Optimization Results (Final Equity):\n")
print(final_equity %>% select(Indicator, Final_Equity = Equity), n = 20)
cat("============================================\n\n")

# 5. Visualization Preparation
plot_data <- plot_data %>%
  mutate(Plot_Role = case_when(
    Indicator == "Baseline" ~ "Baseline (No Stoch)",
    Indicator == best_indicator ~ paste("Optimal:", best_indicator),
    Indicator == "stoch_14_3" & best_indicator != "stoch_14_3" ~ "Default (stoch_14_3)",
    TRUE ~ "Other Variations"
  ))

# Order roles to make sure lines overlay properly
role_levels <- c("Other Variations", "Baseline (No Stoch)")
if ("Default (stoch_14_3)" %in% plot_data$Plot_Role) role_levels <- c(role_levels, "Default (stoch_14_3)")
role_levels <- c(role_levels, paste("Optimal:", best_indicator))

plot_data$Plot_Role <- factor(plot_data$Plot_Role, levels = role_levels)

# Create color palette dynamically based on present roles
colors_map <- c(
  "Other Variations" = "grey80",
  "Baseline (No Stoch)" = "black"
)
if ("Default (stoch_14_3)" %in% plot_data$Plot_Role) colors_map["Default (stoch_14_3)"] <- "dodgerblue"
colors_map[paste("Optimal:", best_indicator)] <- "goldenrod2"

size_map <- c(
  "Other Variations" = 0.5,
  "Baseline (No Stoch)" = 1.0
)
if ("Default (stoch_14_3)" %in% plot_data$Plot_Role) size_map["Default (stoch_14_3)"] <- 1.2
size_map[paste("Optimal:", best_indicator)] <- 1.5

cat("Generating optimization plot...\n")
p <- ggplot(plot_data, aes(x = Date, y = Equity, group = Indicator, color = Plot_Role, linewidth = Plot_Role)) +
  geom_line() +
  scale_color_manual(values = colors_map) +
  scale_linewidth_manual(values = size_map) +
  theme_minimal() +
  labs(title = "Stochastic Oscillator Parameter Optimization",
       subtitle = "Finding the best combination of FastK and FastD for entry confirmations.",
       y = "Portfolio Equity ($)",
       x = "Date",
       color = "Strategy Profile",
       linewidth = "Strategy Profile") +
  theme(legend.position = "bottom")

print(p)
ggsave("portfolio_stoch_optimization.png", plot = p, width = 12, height = 7)
cat("Optimization complete! Results saved to portfolio_stoch_optimization.png\n")
