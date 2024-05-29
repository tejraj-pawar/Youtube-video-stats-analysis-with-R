if (!require('tidyverse')) install.packages('tidyverse', quiet = TRUE); library('tidyverse')
if (!require('skimr')) install.packages('skimr', quiet = TRUE); library('skimr')
if (!require('h2o')) install.packages('h2o', quiet = TRUE); library('h2o')
if (!require('DALEXtra')) install.packages('DALEXtra', quiet = TRUE); library('DALEXtra')

# Read RDS post processed file
processed_youtube_df <- readRDS("processed_youtube_ml.rds")

# Initialize H2O
h2o.init(min_mem_size = "2G", nthreads=-1)

# Push data to H2O
data_h2o_no_destination <- as.h2o(processed_youtube_df)

# Partition the data into training and validation
splits <- h2o.splitFrame(data = data_h2o_no_destination, seed = 1234, ratios = c(0.8))
train_h2o <- splits[[1]]
valid_h2o <- splits[[2]]
rm(splits)

y <- "Duration_Minutes" # Outcome
x <- setdiff(names(train_h2o), y) # Predictors

# Hyper parameter grid
hyper_params <- list(ntrees = c(50, 100, 200),
                     learn_rate = c(0.01, 0.05, 0.1),
                     max_depth = c(3, 5, 7))

# Search criteria
search_criteria <- list(strategy = "RandomDiscrete", max_models = 10)

# Train GBM model with grid search
gbm <- h2o.grid(
  algorithm = "gbm",
  grid_id = "dl_grid_random_gbm",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

# Best model
# Get the grid results, sorted by RMSE
grid_sort_accuracy <- h2o.getGrid(grid_id = "dl_grid_random_gbm", sort_by = "rmse", decreasing = FALSE)
grid_sort_accuracy
best_gbm <- h2o.getModel(grid_sort_accuracy@model_ids[[1]])
plot(best_gbm)
summary(best_gbm)

#Save model
h2o.saveModel(object = best_gbm, path = getwd(), force = TRUE, filename = "4-gbm-model-duration.h2o")

# Shutdown H2O
h2o.shutdown(prompt=FALSE)