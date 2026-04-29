# 00_setup.R
# Script to install and load necessary packages for the quantitative portfolio management architecture.

# CRAN Packages
cran_packages <- c(
  "tidyverse", 
  "tidyquant", 
  "PerformanceAnalytics", 
  "PortfolioAnalytics",
  "remotes",
  "ROI",
  "ROI.plugin.glpk",
  "ROI.plugin.quadprog",
  "DEoptim",
  "pso"
)

# Install CRAN packages if not already installed
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}

# Install TradeAnalytics packages from GitHub (quantstrat and dependencies)
# Note: FinancialInstrument, blotter, and quantstrat are maintained on GitHub by braverock
if (!requireNamespace("FinancialInstrument", quietly = TRUE)) {
  remotes::install_github("braverock/FinancialInstrument")
}
if (!requireNamespace("blotter", quietly = TRUE)) {
  remotes::install_github("braverock/blotter")
}
if (!requireNamespace("quantstrat", quietly = TRUE)) {
  remotes::install_github("braverock/quantstrat")
}

message("Setup complete. All required packages are installed.")
