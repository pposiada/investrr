# 05.7_parameter_optimization.R
# Multi-Asset Strategy Parameter Grid Search

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyquant)
library(purrr)

# 1. Universe Definition
tickers <- c(
  # Defense & Aerospace
  "LMT", "BA", "GD", "RTX", "NOC", "LHX", "HII", "TXT", "LDOS", "BAH",
  # Systemically Important Financial Institutions
  "JPM", "BAC", "C", "WFC", "GS", "MS", "BK", "STT", "PNC", "USB", "COF", "TFC",
  # Energy & Power
  "XOM", "CVX", "COP", "NEE", "DUK", "SO", "D", "AEP", "EXC", "SRE", "PCG", "KMI", "WMB", "SLB", "HAL", "BKR",
  # Technology & Semiconductors
  "MSFT", "AAPL", "GOOGL", "AMZN", "META", "INTC", "NVDA", "AMD", "QCOM", "TXN", "AVGO", "MU", "AMAT", "IBM", "ORCL", "CSCO", "PANW", "CRWD",
  # Telecommunications
  "T", "VZ", "TMUS", "CMCSA", "CHTR",
  # Transportation & Logistics
  "UNP", "CSX", "NSC", "FDX", "UPS", "DAL", "UAL", "AAL", "LUV",
  # Healthcare & Pharmaceuticals
  "JNJ", "PFE", "MRK", "ABBV", "LLY", "UNH", "CVS", "ELV", "MCK", "COR", "CAH", "MRNA",
  # Agriculture & Food Supply
  "ADM", "BG", "DE", "CTVA", "TSN", "GIS", "K", "CF", "MOS",
  # Automotive & Heavy Manufacturing
  "F", "GM", "CAT", "CMI", "PCAR",
  # Materials & Chemicals
  "DOW", "DD", "NUE", "FCX"
)

# 2. Data Wrangling
cat("Fetching daily data for 100 tickers...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = "2018-01-01", to = "2026-12-31")

cat("Calculating Indicators (This will take a moment)...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    SMA10 = SMA(close, n = 10),
    SMA20 = SMA(close, n = 20),
    SMA50 = SMA(close, n = 50),
    SMA200 = SMA(close, n = 200)
  )

# Calculate RSI 5 to 21
rsi_lengths <- 5:21
for (r in rsi_lengths) {
  df <- df %>% mutate(!!paste0("RSI_", r) := RSI(close, n = r))
}
df <- df %>% ungroup()

dates <- sort(unique(df$date))

cat("Pivoting data to wide format...\n")
make_wide <- function(data, col_name) {
  data %>% select(date, symbol, all_of(col_name)) %>% 
    pivot_wider(names_from = symbol, values_from = all_of(col_name)) %>% 
    arrange(date) %>% select(-date) %>% as.matrix()
}

O_mat <- make_wide(df, "open")
H_mat <- make_wide(df, "high")
L_mat <- make_wide(df, "low")
C_mat <- make_wide(df, "close")

SMA200_mat <- make_wide(df, "SMA200")
SMA_list <- list(
  "10" = make_wide(df, "SMA10"),
  "20" = make_wide(df, "SMA20"),
  "50" = make_wide(df, "SMA50")
)

RSI_list <- list()
for (r in rsi_lengths) {
  RSI_list[[as.character(r)]] <- make_wide(df, paste0("RSI_", r))
}

symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# 3. Core Backtest Function
run_backtest <- function(rsi_n, sma_n, sl_buf) {
  # Get correct matrices
  rsi_mat <- RSI_list[[as.character(rsi_n)]]
  sma_mat <- SMA_list[[as.character(sma_n)]]
  
  # State variables
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
  
  # Start loop at day 201 to ensure SMA200 is populated
  for (i in 201:length(dates)) {
    O_today <- O_mat[i, ]
    H_today <- H_mat[i, ]
    L_today <- L_mat[i, ]
    C_today <- C_mat[i, ]
    rsi <- rsi_mat[i, ]
    sma_entry <- sma_mat[i, ]
    sma200 <- SMA200_mat[i, ]
    
    active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
    triggered_indices <- integer(0)
    
    for (j in active_idx) {
      # PHASE A: Track Support & Resistance State
      if (rsi[j] >= 30 && !in_rsi_zone[j]) {
        if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) {
          recent_high[j] <- H_today[j]
        }
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
        # Apply parameterized stop-loss buffer
        stop_loss[j] <- lowest_low[j] * sl_buf
      }
      
      # PHASE B: Exits
      if (position[j] > 0) {
        exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
        
        if (L_today[j] <= stop_loss[j]) {
          global_cash <- global_cash + (shares[j] * exec_price_stop)
          shares[j] <- 0
          position[j] <- 0
          setup_armed[j] <- FALSE
        } else {
          # Check Target 1 (50% position sell)
          if (position[j] == 1 && H_today[j] >= target1[j]) {
            exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
            sell_shares <- floor(shares[j] / 2)
            global_cash <- global_cash + (sell_shares * exec_price_t1)
            shares[j] <- shares[j] - sell_shares
            position[j] <- 0.5
            stop_loss[j] <- entry_price[j] # RISK FREE TRAIL
          }
          # Check Target 2 (SMA 200)
          if (position[j] == 0.5 && H_today[j] >= sma200[j]) {
            exec_price_t2 <- ifelse(O_today[j] > sma200[j], O_today[j], sma200[j])
            global_cash <- global_cash + (shares[j] * exec_price_t2)
            shares[j] <- 0
            position[j] <- 0
          }
        }
      } 
      # PHASE C: Entry Check
      else if (position[j] == 0 && setup_armed[j]) {
        if (L_today[j] <= stop_loss[j]) {
          setup_armed[j] <- FALSE 
        } else if (C_today[j] > sma_entry[j] && C_today[j] < sma200[j]) {
          risk_pct <- (C_today[j] - stop_loss[j]) / C_today[j]
          if (risk_pct <= 0.15) {
             if (!is.na(swing_high[j]) && swing_high[j] > C_today[j]) {
                triggered_indices <- c(triggered_indices, j)
             }
          } else {
            setup_armed[j] <- FALSE 
          }
        }
      }
    }
    
    # PHASE D: Allocate Cash (Equal Distribution)
    if (global_cash > 0 && length(triggered_indices) > 0) {
      cash_per_trade <- global_cash / length(triggered_indices)
      for (j in triggered_indices) {
        entry_px <- C_today[j]
        alloc_shares <- floor(cash_per_trade / entry_px)
        if (alloc_shares > 0) {
          global_cash <- global_cash - (alloc_shares * entry_px)
          shares[j] <- alloc_shares
          position[j] <- 1
          entry_price[j] <- entry_px
          target1[j] <- swing_high[j]
          setup_armed[j] <- FALSE
        }
      }
    }
    
    # Mark-to-Market
    portfolio_value <- global_cash
    for (j in active_idx) {
      if (shares[j] > 0) {
        portfolio_value <- portfolio_value + (shares[j] * C_today[j])
      }
    }
    equity_curve[i] <- portfolio_value
  }
  
  return(equity_curve)
}

# 4. Parameter Grid Definition
grid <- expand.grid(
  rsi_n = 5:21,
  sma_n = c(10, 20, 50),
  sl_buf = c(0.99, 0.98, 0.97, 0.95) # 1%, 2%, 3%, 5% buffer
)
n_runs <- nrow(grid)
cat("Running Grid Search over", n_runs, "combinations...\n")

# Run Grid Search
results_list <- lapply(1:n_runs, function(idx) {
  if (idx %% 10 == 0) cat(sprintf("Completed %d of %d runs...\n", idx, n_runs))
  params <- grid[idx, ]
  eq <- run_backtest(params$rsi_n, params$sma_n, params$sl_buf)
  
  # Return data frame for plotting
  data.frame(
    Date = dates,
    Equity = eq,
    RunID = idx,
    RSI = params$rsi_n,
    SMA = params$sma_n,
    SL_Buf = params$sl_buf,
    Param_Label = paste0("RSI(", params$rsi_n, ") | SMA(", params$sma_n, ") | SL(-", round((1-params$sl_buf)*100, 0), "%)")
  ) %>% filter(Equity > 0)
})

# Combine all results
cat("Aggregating results...\n")
all_results <- bind_rows(results_list)

# 5. Find Winner and Plot
final_equities <- all_results %>%
  group_by(RunID, Param_Label) %>%
  summarize(Final_Equity = last(Equity), .groups = "drop") %>%
  arrange(desc(Final_Equity))

best_run_id <- final_equities$RunID[1]
best_label <- final_equities$Param_Label[1]
best_value <- final_equities$Final_Equity[1]

cat("\n============================================\n")
cat("Top 5 Combinations:\n")
print(head(final_equities, 5))
cat("============================================\n\n")

# Plot
all_results <- all_results %>%
  mutate(Is_Best = (RunID == best_run_id))

p <- ggplot() +
  # Plot all background runs in faint grey
  geom_line(data = filter(all_results, !Is_Best), 
            aes(x = Date, y = Equity, group = RunID), 
            color = "gray60", alpha = 0.2, linewidth = 0.5) +
  # Plot the best run in thick purple
  geom_line(data = filter(all_results, Is_Best), 
            aes(x = Date, y = Equity), 
            color = "purple", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Parameter Optimization - Spaghetti Plot",
    subtitle = paste0(n_runs, " Combinations. Best: ", best_label, " ($", format(round(best_value, 0), big.mark=","), ")"),
    y = "Portfolio Equity ($)", x = "Date"
  )

print(p)
ggsave("parameter_optimization_equity.png", plot = p, width = 10, height = 6)
cat("Optimization complete! Results saved to parameter_optimization_equity.png\n")
