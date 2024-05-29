if (!require('tidyverse')) install.packages('tidyverse', quiet = TRUE); library('tidyverse')
if (!require('skimr')) install.packages('skimr', quiet = TRUE); library('skimr')
if (!require('h2o')) install.packages('h2o', quiet = TRUE); library('h2o')
if (!require('DALEXtra')) install.packages('DALEXtra', quiet = TRUE); library('DALEXtra')

processed_youtube_df <- readRDS("processed_youtube_ml.rds")

h2o.init(min_mem_size = "2G", nthreads=-1)

data_h2o_no_destination <- as.h2o(processed_youtube_df)

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data_h2o_no_destination, seed = 1234, ratios = c(0.8)) # 80/20 split
train_h2o <- splits[[1]] # from training data
valid_h2o <- splits[[2]] # from training data
rm(splits)

y <- "Utc_Day_Part" # column name for outcome
x <- setdiff(names(train_h2o), y) # column names for predictors

hyper_params <- list(
  activation = c("Rectifier", "Tanh", "Maxout", "RectifierWithDropout",
                 "TanhWithDropout", "MaxoutWithDropout"),
  hidden = list(c(50,50), c(32,32,32), c(16,16,16), c(25,25,25,25)),
  input_dropout_ratio = c(0, 0.05),
  l1 = seq(from=0, to=1e-4, by=1e-6),
  l2 = seq(from=0, to=1e-4, by=1e-6)
)

search_criteria = list(
  strategy = "RandomDiscrete",
  seed=1234567,
  stopping_metric = "auto",
  stopping_rounds=5,
  stopping_tolerance=0.01,
  max_runtime_secs = 600,
  max_models = 100
)

grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "dl_grid_random",
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  epochs = 150,
  score_validation_samples = 10000,
  score_duty_cycle = 0.025,
  max_w2 = 10,
  hyper_params = hyper_params,
  search_criteria = search_criteria
)

# Best model
# Get the grid results, sorted by accuracy
grid_sort_accuracy <- h2o.getGrid(grid_id = "dl_grid_random", sort_by = "accuracy", decreasing = TRUE)
grid_sort_accuracy
best_dl <- h2o.getModel(grid_sort_accuracy@model_ids[[1]])
plot(best_dl)
summary(best_dl)

#Save model
h2o.saveModel(object = best_dl, path = getwd(), force = TRUE, filename = "4-dl-model-day-part.h2o")

h2o.shutdown(prompt=FALSE)