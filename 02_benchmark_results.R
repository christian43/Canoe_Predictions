# packages ----

# Load libraries for ML, performance measures, visualization, LaTeX export, ROC analysis, colors
library(mlr3)
library(mlr3measures)
library(mlr3viz)
library(xtable)
library(ROCR)
library(colorspace)

# load benchmark results ----
# Load benchmark object containing resampling and tuning results
res_task <- readRDS("temp/res_task_benchmark_final.rds")

# Define set of performance measures to aggregate over resampling folds
measures <- list(
  msr("classif.auc"),
  msr("classif.bacc"),
  msr("classif.sensitivity"),
  msr("classif.specificity"),
  msr("classif.logloss"),
  msr("classif.prauc")
)

# Aggregate benchmark results (mean over outer CV folds for each learner and measure)
scores_res_task <- res_task$aggregate(measures)

# Extract selected columns into a plain data.frame for tabular reporting
scores <- as.data.frame(scores_res_task)[, c(
  "learner_id",
  "classif.auc",
  "classif.bacc",
  "classif.sensitivity",
  "classif.specificity",
  "classif.logloss",
  "classif.prauc"
)]

# Persist the table of aggregated scores for later use
saveRDS(scores, "temp/benchmarkresults_tab_final.rds")

# Rename columns for more readable LaTeX table headings
names(scores) <- c("Model", "AUC", "BACC", "Sensitivity", "Specificity", "Log loss", "PRAUC")

# Convert to xtable and bold column names
scores_xt <- xtable(scores)
colnames(scores_xt) <- paste0("\\textbf{", colnames(scores_xt), "}")

# Export LaTeX table without row names and without floating environment
print.xtable(
  scores_xt,
  file = "tab/scorerestask_final.tex",
  include.rownames = FALSE,
  floating = FALSE,
  sanitize.colnames.function = identity
)

# confusion matrix ----

# Extract prediction object from a specific resample result (here: 7th learner/result)
p <- res_task$resample_results$resample_result[[7]]$prediction()

# Compute confusion matrix for selected prediction, using correct positive class
conf <- mlr3measures::confusion_matrix(
  truth    = p$truth,
  response = p$response,
  positive = res_task$tasks$task[[1]]$positive
)

# Save predictions for further analyses or diagnostics
saveRDS(p, "temp/benchmark_predictions_final.rds")

# Export confusion matrix as LaTeX table
print.xtable(
  xtable(conf$matrix),
  file = "tab/confmatrix_final.tex",
  auto = TRUE,
  include.rownames = FALSE,
  floating = FALSE
)

# plot confusion matrix ----

# Load custom plotting function for confusion matrices
source("~/Documents/Rfunctions/plotConfusionMatrix.R")

# Open PDF device for confusion matrix plot
cairo_pdf("~/Documents/Figures/Saal_Kanu_confusionmatrix_final.pdf", height = 6, width = 6)

# Plot confusion matrix with custom class labels and hard-coded AUC value
plotConfusionMatrix(conf, Class1 = "International", Class2 = "National", auc = 0.81)

# Close PDF device
dev.off()

# ROC curve ----

# Extract list of predictions (per outer fold) for ROC analysis
x <- res_task$resample_results$resample_result[[7]]$predictions()

# Extract predicted probabilities for target class "Bundeskader" from each fold
predictions <- lapply(x, function(x) x$prob[, "Bundeskader"])

# Extract true labels as numeric and relabel classes (swap coding 1/2)
labels <- lapply(x, function(x) as.numeric(x$truth))
labels <- lapply(labels, function(x) ifelse(x == 2, 1, 2))

# Build ROCR prediction object from probabilities and labels
pred <- prediction(predictions, labels)

# Compute TPR/FPR for ROC curve
perf <- performance(pred, "tpr", "fpr")

# Define color palette for ROC plotting
col_roc <- sequential_hcl(n = 10, palette = "Purples 2")

# Open PDF device for ROC plot
cairo_pdf("~/Documents/Figures/Saal_Kanu_roc_final.pdf", height = 6, width = 6)

# Set plot style: L-shaped box, no top/right border
par(bty = "L")

# Plot ROC curve (averaged over thresholds) with custom line width and colors
plot(
  perf,
  avg = "threshold",
  colorize = FALSE,
  lwd = 3,
  col = col_roc,
  main = "ROC Curve"
)

# Overlay same ROC curve as a dashed line (e.g. second representation)
plot(
  perf,
  lty  = 3,
  add  = TRUE,
  col  = col_roc[5]
)

# Add diagonal reference line representing random classifier
abline(coef = c(0, 1), lty = 1, col = "grey78")

# Close PDF device
dev.off()

# best model ----

# Extract resample result for tuned XGBoost model optimized on BACC
rr_xgb <- res_task$resample_result(learner_id = "xgboost tuned on bacc")

# Collect tuning results (best hyperparameters) from each outer fold
params_list <- lapply(rr_xgb$learners, function(at) at$tuning_result)
params_list # Inspect list of parameter sets

# Combine tuning results from all folds into one data.frame
aggregate_results <- do.call(rbind, params_list)
aggregate_results # Inspect to identify overall best configuration
