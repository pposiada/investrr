# 05.4_advanced_strategy_indicators.R
# Multi-Asset Advanced Reversion Strategy (Indicator Combinations Backtest)

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
cat("Fetching daily data for 100 tickers (This may take 1-2 minutes)...\n")
raw_data <- tq_get(tickers, get = "stock.prices", from = "2018-01-01", to = "2026-12-31")

cat("Fetching S&P 500 data for benchmark comparison...\n")
sp500_data <- tq_get("^GSPC", get = "stock.prices", from = "2018-01-01", to = "2026-12-31") %>%
  select(date, SP500_Close = close) %>%
  mutate(SP500_Return = SP500_Close / lag(SP500_Close) - 1)

cat("Calculating Indicators for all tickers (This might take a moment)...\n")
df <- raw_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    # Core Strategy Indicators
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200),
    RSI16 = RSI(close, n = 16),
    
    # 1. MACD Bullish
    ind_MACD_Bull = MACD(close)[,"macd"] > MACD(close)[,"signal"],
    
    # 2. MFI (Money Flow Index > 50)
    ind_MFI_Bull = MFI(cbind(high, low, close), volume) > 50,
    
    # 3. Aroon (Up > Dn)
    ind_Aroon_Bull = aroon(cbind(high, low), n = 20)[,"aroonUp"] > aroon(cbind(high, low), n = 20)[,"aroonDn"],
    
    # 4. CCI (> -100)
    ind_CCI_Bull = CCI(cbind(high, low, close), n = 14) > -100,
    
    # 5. ADX Trending (> 20)
    ind_ADX_Trend = ADX(cbind(high, low, close), n = 14)[,"ADX"] > 20,
    
    # 6. DI+ > DI-
    ind_DI_Bull = ADX(cbind(high, low, close), n = 14)[,"DIp"] > ADX(cbind(high, low, close), n = 14)[,"DIn"],
    
    # 7. Stochastic Bullish (FastK > FastD)
    ind_Stoch_Bull = stoch(cbind(high, low, close), nFastK = 14)[,"fastK"] > stoch(cbind(high, low, close), nFastK = 14)[,"fastD"],
    
    # 8. Stochastic Not Overbought (FastK < 0.8)
    ind_Stoch_NotOB = stoch(cbind(high, low, close), nFastK = 14)[,"fastK"] < 0.8,
    
    # 9. Bollinger Bands Above Lower Band
    ind_BB_AboveLower = close > BBands(cbind(high, low, close), n = 20)[,"dn"],
    
    # 10. Bollinger Bands Below Mid Band (Mean Reversion)
    ind_BB_BelowMid = close < BBands(cbind(high, low, close), n = 20)[,"mavg"],
    
    # 11. ATR Expanding (ATR > SMA(ATR))
    # We must be careful because ATR function returns a matrix. 
    # To compute SMA on it inside mutate efficiently:
    ind_ATR_Exp = ATR(cbind(high, low, close), n = 14)[,"atr"] > SMA(ATR(cbind(high, low, close), n = 14)[,"atr"], n = 14),
    
    # 12. OBV Bullish (OBV > SMA(OBV))
    obv_tmp = OBV(close, volume),
    ind_OBV_Bull = obv_tmp > SMA(obv_tmp, n = 10),
    
    # 13. CMF > 0
    ind_CMF_Bull = CMF(cbind(high, low, close), volume) > 0,
    
    # 14. Momentum > 0
    ind_MOM_Bull = momentum(close, n = 10) > 0,
    
    # 15. ROC > 0
    ind_ROC_Bull = ROC(close, n = 10) > 0,
    
    # 16. Williams %R Not Overbought (WPR in TTR returns 0 to 1, 0 is overbought high)
    ind_WPR_NotOB = WPR(cbind(high, low, close), n = 14) > 0.2,
    
    # 17. Williams %R Bullish (Not deeply oversold anymore)
    ind_WPR_Bull = WPR(cbind(high, low, close), n = 14) < 0.8,
    
    # 18. SMA50 Bullish (Close > SMA50)
    ind_SMA50_Bull = close > SMA(close, n = 50),
    
    # 19. Golden Cross (SMA50 > SMA200)
    ind_GoldenCross = SMA(close, n = 50) > SMA200,
    
    # 20. EMA Bullish (EMA9 > EMA21)
    ind_EMA_Bull = EMA(close, n = 9) > EMA(close, n = 21)
  ) %>%
  ungroup() %>%
  # Remove temporary column
  select(-obv_tmp)

# To make the event loop fast, we pivot the required columns to wide format
dates <- sort(unique(df$date))

cat("Pivoting data to wide format for fast time-series iteration...\n")
make_wide <- function(col_name) {
  df %>% select(date, symbol, all_of(col_name)) %>% 
    pivot_wider(names_from = symbol, values_from = all_of(col_name)) %>% 
    arrange(date) %>% select(-date) %>% as.matrix()
}

O_mat <- make_wide("open")
H_mat <- make_wide("high")
L_mat <- make_wide("low")
C_mat <- make_wide("close")
SMA10_mat <- make_wide("SMA10")
SMA200_mat <- make_wide("SMA200")
RSI_mat <- make_wide("RSI16")

indicator_names <- c(
  "ind_MACD_Bull", "ind_MFI_Bull", "ind_Aroon_Bull", "ind_CCI_Bull", 
  "ind_ADX_Trend", "ind_DI_Bull", "ind_Stoch_Bull", "ind_Stoch_NotOB", 
  "ind_BB_AboveLower", "ind_BB_BelowMid", "ind_ATR_Exp", "ind_OBV_Bull", 
  "ind_CMF_Bull", "ind_MOM_Bull", "ind_ROC_Bull", "ind_WPR_NotOB", 
  "ind_WPR_Bull", "ind_SMA50_Bull", "ind_GoldenCross", "ind_EMA_Bull"
)

cat("Pivoting 20 indicator filters to wide format...\n")
indicator_matrices <- list()
for (ind in indicator_names) {
  indicator_matrices[[ind]] <- make_wide(ind)
}

symbols <- colnames(C_mat)
n_symbols <- length(symbols)

# 3. Encapsulated Backtest Function
run_backtest <- function(ind_name = "Baseline") {
  # Initialize State
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
  
  # Event Loop
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
      sym <- symbols[j]
      
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
        stop_loss[j] <- lowest_low[j] * 0.98
      }
      
      # PHASE B: Exits & Risk Management
      if (position[j] > 0) {
        exec_price_stop <- ifelse(O_today[j] < stop_loss[j], O_today[j], stop_loss[j])
        
        if (L_today[j] <= stop_loss[j]) {
          global_cash <- global_cash + (shares[j] * exec_price_stop)
          shares[j] <- 0
          position[j] <- 0
          setup_armed[j] <- FALSE
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
            shares[j] <- 0
            position[j] <- 0
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
                
                # Check Indicator Filter
                passed_filter <- TRUE
                if (ind_name != "Baseline") {
                  ind_val <- ind_mat[i, j]
                  if (is.na(ind_val) || !ind_val) {
                    passed_filter <- FALSE
                  }
                }
                
                if (passed_filter) {
                  triggered_indices <- c(triggered_indices, j)
                }
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
          position[j] <- 1
          entry_price[j] <- entry_px
          target1[j] <- swing_high[j]
          setup_armed[j] <- FALSE
        }
      }
    }
    
    # Mark to Market
    portfolio_value <- global_cash
    for (j in active_idx) {
      if (shares[j] > 0) {
        portfolio_value <- portfolio_value + (shares[j] * C_today[j])
      }
    }
    equity_curve[i] <- portfolio_value
  }
  
  return(data.frame(Date = dates, Equity = equity_curve, Indicator = ind_name, stringsAsFactors = FALSE))
}

# 4. Iterating Through Indicators
all_results <- list()

cat("\nRunning Baseline Strategy Backtest...\n")
all_results[["Baseline"]] <- run_backtest("Baseline")

for (ind in indicator_names) {
  cat(sprintf("Running Strategy Backtest with Filter: %s...\n", ind))
  all_results[[ind]] <- run_backtest(ind)
}

plot_data <- bind_rows(all_results) %>% filter(!is.na(Equity) & Equity > 0)

# Calculate final equity for each to see the summary
final_equity <- plot_data %>%
  group_by(Indicator) %>%
  slice(n()) %>%
  arrange(desc(Equity)) %>%
  select(Indicator, Final_Equity = Equity)

cat("\n============================================\n")
cat("Final Equity by Indicator Filter:\n")
print(final_equity, n = 21)
cat("============================================\n\n")

# 5. Visualization
cat("Generating comparison plot...\n")

# Order the factor so Baseline is always visible and first
plot_data$Indicator <- factor(plot_data$Indicator, levels = c("Baseline", indicator_names))

p <- ggplot(plot_data, aes(x = Date, y = Equity, group = Indicator)) +
  # Add the indicator lines thin and slightly transparent
  geom_line(data = plot_data %>% filter(Indicator != "Baseline"), 
            aes(color = Indicator), alpha = 0.6, linewidth = 0.5) +
  # Add Baseline thick and black
  geom_line(data = plot_data %>% filter(Indicator == "Baseline"), 
            color = "black", linewidth = 1.5) +
  theme_minimal() +
  labs(title = "Strategy Comparison: Evaluating 20 Indicator Filters",
       subtitle = "Baseline strategy (Black) compared against variants requiring specific indicator confirmation.",
       y = "Portfolio Equity ($)",
       x = "Date",
       color = "Additional Filter") +
  theme(legend.position = "right", 
        legend.text = element_text(size = 8))

print(p)
ggsave("portfolio_indicators_comparison.png", plot = p, width = 12, height = 7)
cat("Comparison backtest complete! Results saved to portfolio_indicators_comparison.png\n")
