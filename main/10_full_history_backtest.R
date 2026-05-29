# 10_full_history_backtest.R
# Advanced Reversion Strategy - Full 26-Year History Backtest (2000-2026)

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
cat("Fetching long-term history (2000-2026) for 100 tickers...\n")
# Fetch starting from 1999 to give room for the 200-day SMA in year 2000
raw_data <- tq_get(tickers, get = "stock.prices", from = "1999-01-01", to = "2026-12-31")

cat("Fetching S&P 500 data for benchmark comparison...\n")
sp500_data <- tq_get("^GSPC", get = "stock.prices", from = "2000-01-01", to = "2026-12-31") %>%
  select(date, SP500_Close = close)

cat("Calculating Indicators globally...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  drop_na(high, low, close) %>%
  mutate(
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200),
    RSI16 = RSI(close, n = 16),
    L5 = runMin(low, n = 5),
    H5 = runMax(high, n = 5),
    fastK = (close - L5) / (H5 - L5),
    fastK = ifelse(is.nan(fastK) | is.infinite(fastK), NA, fastK),
    fastK = zoo::na.locf(fastK, na.rm = FALSE),
    fastK = ifelse(is.na(fastK), 0.5, fastK),
    fastD = SMA(fastK, n = 7),
    stoch_bull = fastK > fastD
  ) %>%
  select(-L5, -H5, -fastK, -fastD) %>%
  ungroup() %>%
  # Filter out 1999 since we only used it for SMA calculation
  filter(date >= as.Date("2000-01-01"))

dates <- sort(unique(df$date))

make_wide <- function(col_name) {
  df %>% select(date, symbol, all_of(col_name)) %>% 
    pivot_wider(names_from = symbol, values_from = all_of(col_name)) %>% 
    arrange(date) %>% select(-date) %>% as.matrix()
}

cat("Pivoting matrices...\n")
O_mat <- make_wide("open")
H_mat <- make_wide("high")
L_mat <- make_wide("low")
C_mat <- make_wide("close")
SMA10_mat <- make_wide("SMA10")
SMA200_mat <- make_wide("SMA200")
RSI_mat <- make_wide("RSI16")
stoch_mat <- make_wide("stoch_bull")

symbols <- colnames(C_mat)
n_symbols <- length(symbols)

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
trade_log <- list()

cat("Running Continuous 26-Year Portfolio Event Loop...\n")

for (i in 1:length(dates)) {
  O_today <- O_mat[i, ]
  H_today <- H_mat[i, ]
  L_today <- L_mat[i, ]
  C_today <- C_mat[i, ]
  rsi <- RSI_mat[i, ]
  sma10 <- SMA10_mat[i, ]
  sma200 <- SMA200_mat[i, ]
  stoch_today <- stoch_mat[i, ]
  
  active_idx <- which(!is.na(C_today) & !is.na(sma200) & !is.na(rsi))
  triggered_indices <- integer(0)
  
  for (j in active_idx) {
    if (rsi[j] >= 30 && !in_rsi_zone[j]) {
      if (is.na(recent_high[j]) || H_today[j] > recent_high[j]) recent_high[j] <- H_today[j]
    }
    if (rsi[j] < 30) {
      if (!in_rsi_zone[j]) {
        in_rsi_zone[j] <- TRUE; swing_high[j] <- recent_high[j]; lowest_low[j] <- L_today[j]; setup_armed[j] <- FALSE; recent_high[j] <- NA
      } else {
        if (L_today[j] < lowest_low[j]) lowest_low[j] <- L_today[j]
      }
    }
    if (rsi[j] >= 30 && in_rsi_zone[j]) {
      in_rsi_zone[j] <- FALSE; setup_armed[j] <- TRUE; stop_loss[j] <- lowest_low[j] * 0.98
    }
    
    if (position[j] > 0) {
      exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
      if (L_today[j] <= stop_loss[j]) {
        global_cash <- global_cash + (shares[j] * exec_price_stop)
        trade_log[[length(trade_log) + 1]] <- data.frame(Date = dates[i], Symbol = symbols[j], Action = "STOP_LOSS", Price = exec_price_stop, Shares = -shares[j])
        shares[j] <- 0; position[j] <- 0; setup_armed[j] <- FALSE
      } else {
        if (position[j] == 1 && H_today[j] >= target1[j]) {
          exec_price_t1 <- ifelse(O_today[j] > target1[j], O_today[j], target1[j])
          sell_shares <- floor(shares[j] / 2)
          global_cash <- global_cash + (sell_shares * exec_price_t1)
          shares[j] <- shares[j] - sell_shares
          position[j] <- 0.5; stop_loss[j] <- entry_price[j]
          trade_log[[length(trade_log) + 1]] <- data.frame(Date = dates[i], Symbol = symbols[j], Action = "TARGET_1_HIT", Price = exec_price_t1, Shares = -sell_shares)
        }
        if (position[j] == 0.5 && H_today[j] >= sma200[j]) {
          exec_price_t2 <- ifelse(O_today[j] > sma200[j], O_today[j], sma200[j])
          global_cash <- global_cash + (shares[j] * exec_price_t2)
          trade_log[[length(trade_log) + 1]] <- data.frame(Date = dates[i], Symbol = symbols[j], Action = "TARGET_2_HIT", Price = exec_price_t2, Shares = -shares[j])
          shares[j] <- 0; position[j] <- 0
        }
      }
    } else if (position[j] == 0 && setup_armed[j]) {
      if (L_today[j] <= stop_loss[j]) {
        setup_armed[j] <- FALSE
      } else if (C_today[j] > sma10[j] && C_today[j] < sma200[j]) {
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
        trade_log[[length(trade_log) + 1]] <- data.frame(Date = dates[i], Symbol = symbols[j], Action = "ENTRY", Price = entry_px, Shares = alloc_shares)
      }
    }
  }
  
  portfolio_value <- global_cash
  for (j in active_idx) {
    if (shares[j] > 0) portfolio_value <- portfolio_value + (shares[j] * C_today[j])
  }
  equity_curve[i] <- portfolio_value
}

trade_df <- bind_rows(trade_log)

cat("\n============================================\n")
cat("Total Trades Executed:", nrow(trade_df), "\n")
cat("Final Portfolio Value: $", formatC(round(equity_curve[length(equity_curve)], 2), format="f", big.mark=","), "\n")
cat("============================================\n\n")

# Calculate Strategy Returns and Correlation against S&P 500
plot_data <- data.frame(Date = dates, Equity = equity_curve) %>% filter(!is.na(Equity) & Equity > 0)
plot_data <- plot_data %>% 
  mutate(Strategy_Return = Equity / lag(Equity) - 1) %>%
  left_join(sp500_data, by = c("Date" = "date"))

# Normalize SP500 for the plot (start at 100k)
sp_start <- plot_data$SP500_Close[1]
plot_data <- plot_data %>%
  mutate(SP500_Equity = (SP500_Close / sp_start) * 100000)

strat_cor <- cor(plot_data$Strategy_Return, plot_data$SP500_Close / lag(plot_data$SP500_Close) - 1, use = "complete.obs")
cat("Strategy vs S&P 500 Correlation (2000-2026):", round(strat_cor, 4), "\n")

# Plotting on Log Scale for long-term compound growth visibility
cat("Generating 26-Year Log-Scale Chart...\n")

# Pivot for ggplot
plot_data_long <- plot_data %>%
  select(Date, Strategy = Equity, `S&P 500` = SP500_Equity) %>%
  pivot_longer(cols = c(Strategy, `S&P 500`), names_to = "Series", values_to = "Value")

p <- ggplot(plot_data_long, aes(x = Date, y = Value, color = Series)) +
  geom_line(linewidth = 0.8) +
  theme_minimal() +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_color_manual(values = c("Strategy" = "darkblue", "S&P 500" = "gray50")) +
  labs(title = "Advanced Reversal Strategy: 26-Year Compound Growth",
       subtitle = "Logarithmic Scale (2000 - 2026). Stoch(5,7) Entry Filter.",
       y = "Portfolio Equity (Log Scale)",
       x = "Year")

print(p)
ggsave("./outputs/portfolio_full_history_log.png", plot = p, width = 12, height = 7)
cat("Done! Chart saved to outputs/portfolio_full_history_log.png\n")
