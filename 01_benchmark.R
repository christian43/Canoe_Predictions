# seed and packages ----

rm(list = ls()) # Clear workspace
set.seed(5673) # Set random seed for reproducibility

# Packages for environment management, ML, tuning, visualization, and parallelization
library(renv)
library(mlr3)
library(mlr3verse)
library(mlr3fselect)
library(mlr3tuningspaces)
library(mlr3tuning)
library(mlr3viz)
library(mlr3extralearners)
library(future.apply)

# iniate environment ----
renv::snapshot() # Store current package state in renv lockfile

# read and process data -----

kanu <- readRDS("") # Load dataset (path needs to be specified)

# create tasks and learners ----

# Create classification task with BK as target
task <- as_task_classif(
  kanu,
  target   = "BK", # Target variable
  id       = "kanu", # Task ID
  positive = "Bundeskader" # Positive class label
)
task$col_roles$stratum <- task$target_names # Use target for stratification
prop.table(table(task$data(cols = "BK"))) # Show class distribution

saveRDS(task, "temp/task.rds") # Save task object

# define models ----

cv_inner <- rsmp("cv", folds = 5) # Inner CV for tuning
cv_outer <- rsmp("cv", folds = 10) # Outer CV for benchmarking

msr_bacc <- msr("classif.bacc") # Balanced accuracy metric
msr_auc <- msr("classif.auc") # AUC metric

# Base learners
lrn_ranger <- lrn("classif.ranger", predict_type = "prob")
lrn_ranger$id <- "Ranger"
lrn_rpart <- lrn("classif.rpart", predict_type = "prob")
lrn_rpart$id <- "Rpart"
lrn_featureless <- lrn("classif.featureless", predict_type = "prob")
lrn_featureless$id <- "Featureless"

# Preprocessing: class balancing via oversampling (ratio is tuned)
po_over <- po(
  "classbalancing",
  id        = "oversample",
  adjust    = "minor",
  reference = "minor",
  shuffle   = TRUE,
  ratio     = to_tune(1, 6)
)

po_scale <- po("scale") # Standardize numerical features
po_threshold <- po("threshold") # Tune decision threshold post-hoc

# Tunable Ranger learner (Random Forest) with hyperparameters to tune
po_lrn_ranger_tuned <- lrn(
  "classif.ranger",
  predict_type    = "prob",
  num.trees       = to_tune(100, 2000),
  mtry.ratio      = to_tune(0.1, 1),
  sample.fraction = to_tune(0.1, 1)
)

# Pipeline: oversampling → scaling → Ranger → threshold
glrn_ranger_tuned <- po_over %>>% po_scale %>>% po_lrn_ranger_tuned %>>% po_threshold
glrn_ranger_tuned <- as_learner(glrn_ranger_tuned)
glrn_ranger_tuned$id <- "Ranger tuned"
glrn_ranger_tuned$param_set$values$threshold.thresholds <- to_tune(p_dbl(lower = 0, upper = 1))

# Auto-tuner for Ranger optimized for AUC
glrn_ranger_tuned_auc <- auto_tuner(
  tuner      = tnr("random_search"),
  learner    = glrn_ranger_tuned,
  resampling = cv_inner,
  measure    = msr_auc,
  terminator = trm("run_time", secs = 5000)
)
glrn_ranger_tuned_auc$id <- "ranger tuned on auc"

# Auto-tuner for Ranger optimized for balanced accuracy
glrn_ranger_tuned_bacc <- auto_tuner(
  tuner      = tnr("random_search"),
  learner    = glrn_ranger_tuned,
  resampling = cv_inner,
  measure    = msr_bacc,
  terminator = trm("run_time", secs = 5000)
)
glrn_ranger_tuned_bacc$id <- "ranger tuned on bacc"

# Load predefined tuning space for XGBoost and get corresponding learner
tuning_space <- lts("classif.xgboost.default")
po_lrn_xgboost <- tuning_space$get_learner(predict_type = "prob")

# Pipeline: oversampling → robustify (handle outliers/missings) → XGBoost → threshold
glrn_xgboost <- as_learner(
  po_over %>>% ppl("robustify") %>>% po_lrn_xgboost %>>% po_threshold
)
glrn_xgboost$param_set$values$threshold.thresholds <- to_tune(p_dbl(lower = 0, upper = 1))

# Auto-tuner for XGBoost optimized for AUC
glrn_xgboost_tuned_auc <- auto_tuner(
  tuner      = tnr("random_search"),
  learner    = glrn_xgboost,
  resampling = cv_inner,
  measure    = msr_auc,
  terminator = trm("run_time", secs = 5000)
)
glrn_xgboost_tuned_auc$id <- "xgboost tuned on auc"

# Auto-tuner for XGBoost optimized for balanced accuracy
glrn_xgboost_tuned_bacc <- auto_tuner(
  tuner      = tnr("random_search"),
  learner    = glrn_xgboost,
  resampling = cv_inner,
  measure    = msr_bacc,
  terminator = trm("run_time", secs = 5000)
)
glrn_xgboost_tuned_bacc$id <- "xgboost tuned on bacc"

# benchmark ----

# Benchmark design: one task, multiple (possibly tuned) learners, outer CV
design <- benchmark_grid(
  task,
  c(
    lrn_rpart,
    lrn_ranger,
    lrn_featureless,
    glrn_ranger_tuned_auc,
    glrn_ranger_tuned_bacc,
    glrn_xgboost_tuned_auc,
    glrn_xgboost_tuned_bacc
  ),
  cv_outer
)

# Parallelization: multisession using all but one core
plan(
  multisession,
  workers = availableCores() - 1
)

# Run benchmark and store fitted models
res_task <- benchmark(
  design,
  store_models = TRUE
)

### save
saveRDS(res_task, "temp/res_task_benchmark_final.rds") # Save benchmark results
