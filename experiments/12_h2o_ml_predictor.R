# 12_h2o_ml_predictor.R
# WIG20 Machine Learning Price Predictor using H2O AutoML

library(quantmod)
library(tidyquant)
library(dplyr)
library(tidyr)
library(ggplot2)
library(h2o)

# 1. Fetch Data
cat("Fetching WIG20 historical data (using PKO.WA as proxy since WIG20 index is often unavailable on Yahoo)...\n")
ticker <- "PKO.WA" # Using PKO Bank Polski as the largest WIG20 proxy
raw_data <- tq_get(ticker, get = "stock.prices", from = "2010-01-01", to = Sys.Date())

if (is.logical(raw_data) && is.na(raw_data[1])) {
  stop(sprintf("Failed to fetch data for %s from Yahoo Finance. Please check your internet connection.", ticker))
}

# 2. Calculate Technical Indicators (Features)
cat("Calculating Technical Features...\n")
df_features <- raw_data %>%
  arrange(date) %>%
  drop_na(high, low, close) %>%
  mutate(
    # Core moving averages
    SMA10 = SMA(close, n = 10),
    SMA200 = SMA(close, n = 200),
    
    # RSI
    RSI14 = RSI(close, n = 14),
    
    # Stochastic (10,5)
    L10 = runMin(low, n = 10),
    H10 = runMax(high, n = 10),
    fastK = (close - L10) / (H10 - L10),
    fastK = ifelse(is.nan(fastK) | is.infinite(fastK), NA, fastK),
    fastK = zoo::na.locf(fastK, na.rm = FALSE),
    fastK = ifelse(is.na(fastK), 0.5, fastK),
    fastD = SMA(fastK, n = 5),
    stoch_bull = as.numeric(fastK > fastD),
    
    # Target Variable: Tomorrow's Closing Price (T+1)
    target_close = dplyr::lead(close, 1)
  ) %>%
  select(-L10, -H10) %>%
  drop_na() # Remove rows with NAs (due to 200 SMA or the lead shift)

cat(sprintf("Prepared %d days of feature-rich data.\n", nrow(df_features)))

# 3. Initialize H2O Cluster
cat("Initializing H2O Cluster...\n")
h2o.init(nthreads = -1, max_mem_size = "4G")

# 4. Convert to H2O Frame and Chronological Split (80/20)
h2o_df <- as.h2o(df_features)

# Find the cutoff date for an 80% / 20% chronological split
split_idx <- floor(nrow(df_features) * 0.8)
cutoff_date <- df_features$date[split_idx]
cat(sprintf("Chronological Split: Training on data before %s, Testing after.\n", cutoff_date))

train_h2o <- h2o_df[h2o_df$date < as.character(cutoff_date), ]
test_h2o  <- h2o_df[h2o_df$date >= as.character(cutoff_date), ]

# Define predictors (x) and target (y)
y <- "target_close"
x <- c("open", "high", "low", "close", "volume", 
       "SMA10", "SMA200", "RSI14", "fastK", "fastD", "stoch_bull")

# 5. Run AutoML
cat("\nStarting H2O AutoML (Grid Search across models)...\n")
# max_models limits the number of base models (excluding stacked ensembles)
aml <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  leaderboard_frame = test_h2o,
  max_models = 15,
  seed = 1234,
  sort_metric = "RMSE"
)

# 6. View Leaderboard
cat("\n=== AutoML Leaderboard ===\n")
lb <- aml@leaderboard
print(lb, n = nrow(lb))

# 7. Evaluate the Best Model
best_model <- aml@leader
cat("\nBest Model Found:", best_model@algorithm, "\n")

# Variable Importance (if applicable)
if (best_model@algorithm %in% c("gbm", "drf", "xgboost")) {
  h2o.varimp_plot(best_model)
}

# 8. Predictions and Plotting on Test Set
cat("\nGenerating predictions on the Test Set...\n")
preds_h2o <- h2o.predict(best_model, test_h2o)
preds_df <- as.data.frame(preds_h2o)
test_df <- as.data.frame(test_h2o)

# Combine for plotting
plot_data <- data.frame(
  Date = as.Date(test_df$date),
  Actual = test_df$target_close,
  Predicted = preds_df$predict
)

# Visualize Actual vs Predicted
p <- ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual Close (T+1)"), linewidth = 0.8) +
  geom_line(aes(y = Predicted, color = "Predicted Close"), linewidth = 0.8, alpha = 0.7) +
  scale_color_manual(values = c("Actual Close (T+1)" = "black", "Predicted Close" = "blue")) +
  theme_minimal() +
  labs(
    title = "WIG20 T+1 Price Prediction (H2O AutoML)",
    subtitle = paste("Best Model:", best_model@algorithm),
    x = "Date",
    y = "Price",
    color = "Legend"
  ) +
  theme(legend.position = "bottom")

if(!dir.exists("outputs")) dir.create("outputs")
ggsave("outputs/h2o_wig20_prediction.png", plot = p, width = 10, height = 6)
cat("Plot saved to outputs/h2o_wig20_prediction.png\n")

# h2o.shutdown(prompt = FALSE) # Uncomment to auto-shutdown cluster
