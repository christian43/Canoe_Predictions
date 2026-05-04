library(mlr3verse) # High-level interface and helpers around mlr3
library(mlr3tuningspaces) # Predefined tuning spaces
library(mlr3tuning) # Hyperparameter tuning infrastructure
library(mlr3extralearners) # Additional learners (e.g. xgboost wrappers)
library(mlr3) # Core mlr3 functionality
library(data.table) # Efficient data structures and fast operations on large tables

set.seed(488) # Fix random seed for reproducibility

task <- readRDS("temp/task.rds") # Load classification task
res_task <- readRDS("temp/res_task_benchmark_final.rds") # Load benchmark results

# Get resample result for tuned XGBoost (outer CV for this learner)
rr_xgb <- res_task$resample_result(learner_id = "xgboost tuned on bacc")

# Extract tuning results (best hyperparameters + performance) from each outer fold
params_list <- lapply(rr_xgb$learners, function(x) x$tuning_result)

# Combine all folds' tuning results into one data.table / data.frame
aggregate_results <- rbindlist(params_list)

# Identify fold with best balanced accuracy
best_idx <- which.max(aggregate_results$classif.bacc)

# Extract the tuned XGBoost graph learner corresponding to the best outer fold
glrn_xgboost <- rr_xgb$learners[[best_idx]]$model$learner

# Train this best-tuned learner once more on the full task
glrn_xgboost <- glrn_xgboost$train(task)

# Save the final trained model for deployment / later prediction
saveRDS(glrn_xgboost, file = "temp/glrnxgboost_final.rds")
