# 03_portfolio_optimization.R
# Advanced Portfolio Optimization and Constrained Search

library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

# Load the xts data created in step 01
if (!file.exists("portfolio_returns_xts.rds")) {
  stop("Please run 01_data_pipeline.R first to generate the data.")
}
portfolio_returns_xts <- readRDS("portfolio_returns_xts.rds")
funds <- colnames(portfolio_returns_xts)

cat("Setting up Portfolio Specification...\n")

# 1. The Portfolio Specification Lifecycle
# Create the initial portfolio object
portf_init <- portfolio.spec(assets = funds)

# Add Constraints
# - Full investment constraint (weights sum to 1)
portf_init <- add.constraint(portfolio = portf_init, type = "full_investment")
# - Box constraints (long only, weights between 0.05 and 0.40)
portf_init <- add.constraint(portfolio = portf_init, type = "box", min = 0.05, max = 0.40)

# Add Objectives
# - Minimize variance (standard Markowitz)
portf_minvar <- add.objective(portfolio = portf_init, type = "risk", name = "var")

cat("Running Optimization (Minimum Variance)...\n")

# 2. Solver Integration
# Use ROI to solve the quadratic problem
opt_minvar <- optimize.portfolio(R = portfolio_returns_xts, 
                                 portfolio = portf_minvar, 
                                 optimize_method = "ROI", 
                                 trace = TRUE)

cat("\nOptimal Weights (Minimum Variance):\n")
print(extractWeights(opt_minvar))

# 3. Random Portfolio Generation for visualization
cat("Generating Random Portfolios...\n")
# Maximize return and minimize Expected Tail Loss (ETL/ES)
portf_etl <- add.objective(portfolio = portf_init, type = "return", name = "mean")
portf_etl <- add.objective(portfolio = portf_etl, type = "risk", name = "ES", arguments = list(p=0.95))

# Use random portfolios method (sample)
opt_random <- optimize.portfolio(R = portfolio_returns_xts, 
                                 portfolio = portf_etl, 
                                 optimize_method = "random", 
                                 search_size = 2000, 
                                 trace = TRUE)

# Plot the efficient frontier cloud
png("random_portfolios.png", width = 800, height = 600)
chart.RiskReward(opt_random, risk.col = "ES", return.col = "mean", main="Random Portfolios (ES vs Mean)")
dev.off()
cat("Random portfolios plotted to random_portfolios.png\n")

# 4. Black-Litterman Model Template
cat("Setting up Black-Litterman Model...\n")
# Calculate implied equilibrium returns (pi) based on equal market capitalization
w_mkt <- rep(1/length(funds), length(funds)) 
# Covariance matrix
Sigma <- cov(portfolio_returns_xts)
# Risk aversion parameter (lambda) - approximation
lambda <- 2.5
# Implied equilibrium returns
pi_implied <- lambda * (Sigma %*% w_mkt)

# Specify Absolute View: "Asset 1 (AAPL) will return 1%"
P <- matrix(0, nrow=1, ncol=length(funds))
colnames(P) <- funds
P[1, "AAPL"] <- 1
Q <- c(0.01)

# Run Black-Litterman Formula to blend views
bl_results <- BlackLittermanFormula(pi_implied, Sigma, P, Q, tau = 0.025)

cat("\nBlack-Litterman Expected Returns:\n")
print(bl_results$BLMu)
