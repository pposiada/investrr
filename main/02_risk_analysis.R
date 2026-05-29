# 02_risk_analysis.R
# Econometric Analysis of Financial Risk and Performance

library(PerformanceAnalytics)

# Load the xts data created in step 01
if (!file.exists("portfolio_returns_xts.rds")) {
  stop("Please run 01_data_pipeline.R first to generate the data.")
}
portfolio_returns_xts <- readRDS("portfolio_returns_xts.rds")

# We create an equally weighted portfolio for analysis purposes
weights <- rep(1/ncol(portfolio_returns_xts), ncol(portfolio_returns_xts))
portfolio_return <- Return.portfolio(portfolio_returns_xts, weights = weights)

cat("Calculating Risk Metrics...\n")

# 1. Non-Normal Distributions and Tail Risk Metrics
# Modified Value at Risk (mVaR) using Cornish-Fisher expansion
mVaR <- VaR(portfolio_returns_xts, p = 0.95, method = "modified")
cat("\nModified VaR (95%):\n")
print(mVaR)

# Expected Shortfall (ES) / Conditional VaR
mES <- ES(portfolio_returns_xts, p = 0.95, method = "modified")
cat("\nModified Expected Shortfall (95%):\n")
print(mES)

# Maximum Drawdown
max_drawdowns <- maxDrawdown(portfolio_returns_xts)
cat("\nMaximum Drawdown:\n")
print(max_drawdowns)

# 2. Risk-Adjusted Performance Ratios
# Sortino Ratio (using 0% as Minimum Acceptable Return)
sortino <- SortinoRatio(portfolio_returns_xts, MAR = 0)
cat("\nSortino Ratio (MAR = 0):\n")
print(sortino)

# Sharpe Ratio
sharpe <- SharpeRatio(portfolio_returns_xts, Rf = 0, p = 0.95, FUN = "StdDev")
cat("\nSharpe Ratio:\n")
print(sharpe)

# 3. Systematic Performance Attribution and Reporting
# Generate a comprehensive visual summary (Tearsheet)
png("performance_summary.png", width = 800, height = 600)
charts.PerformanceSummary(portfolio_return, 
                          main = "Equally Weighted Portfolio Performance",
                          colorset = rich10equal)
dev.off()
cat("\nPerformance tearsheet saved to performance_summary.png\n")

# Tabular summary of Downside Risk
downside_table <- table.DownsideRisk(portfolio_returns_xts)
cat("\nDownside Risk Table:\n")
print(downside_table)
