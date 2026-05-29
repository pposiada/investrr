# 04_backtesting.R
# Systematic Backtesting and Signal-Based Trading

# Ensure required packages are loaded
library(quantstrat)

# Remove any existing strategy/portfolio objects from the environment to prevent conflicts
suppressWarnings(rm("account.macross", "portfolio.macross", pos = .blotter))
suppressWarnings(rm("strategy.macross", "account.macross", "portfolio.macross", "sys.Data.macross"))

# Initialize currency and instruments
currency("USD")
stock("AAPL", currency = "USD", multiplier = 1)

# Fetch historical data for AAPL
getSymbols("AAPL", from = "2020-01-01", to = "2023-12-31", src = "yahoo", adjust = TRUE)

# Strategy settings
strategy.st <- "macross"
portfolio.st <- "macross"
account.st <- "macross"
initEq <- 100000

# Initialize portfolio and account (The blotter package handles accounting)
initPortf(portfolio.st, symbols = "AAPL", initDate = "2019-12-31", currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = "2019-12-31", currency = "USD", initEq = initEq)
initOrders(portfolio.st, initDate = "2019-12-31")

# Initialize the strategy object
strategy(strategy.st, store = TRUE)

cat("Setting up Strategy Indicators, Signals, and Rules...\n")

# 1. Indicators
# Add a 50-day Simple Moving Average (SMA)
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 50),
              label = "sma50")

# Add a 200-day Simple Moving Average (SMA)
add.indicator(strategy = strategy.st,
              name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              label = "sma200")

# 2. Signals
# Bullish crossover: 50-day SMA crosses above 200-day SMA
add.signal(strategy = strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("sma50", "sma200"), relationship = "gt"),
           label = "bullish_cross")

# Bearish crossover: 50-day SMA crosses below 200-day SMA
add.signal(strategy = strategy.st,
           name = "sigCrossover",
           arguments = list(columns = c("sma50", "sma200"), relationship = "lt"),
           label = "bearish_cross")

# 3. Rules
# Enter Long rule when bullish crossover occurs
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "bullish_cross",
                          sigval = TRUE,
                          orderqty = 100, # Fixed share sizing for simplicity
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "enter",
         label = "enter_long")

# Exit Long rule when bearish crossover occurs
add.rule(strategy = strategy.st,
         name = "ruleSignal",
         arguments = list(sigcol = "bearish_cross",
                          sigval = TRUE,
                          orderqty = "all",
                          ordertype = "market",
                          orderside = "long",
                          replace = FALSE,
                          prefer = "Open"),
         type = "exit",
         label = "exit_long")

cat("Running Backtest...\n")

# Apply the strategy
out <- applyStrategy(strategy = strategy.st, portfolios = portfolio.st)

cat("Updating Portfolio and Account Accounting...\n")

# Update accounting (blotter)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)

# Generate performance visualization
cat("Plotting equity curve...\n")
png("backtest_equity_curve.png", width = 800, height = 600)
chart.Posn(portfolio.st, Symbol = "AAPL")
dev.off()

cat("Backtest complete. Equity curve saved to backtest_equity_curve.png\n")

# Show trade statistics
tstats <- tradeStats(portfolio.st)
cat("\nTrade Statistics:\n")
print(tstats[, c("Symbol", "Num.Trades", "Net.Trading.PL", "Max.Drawdown")])
