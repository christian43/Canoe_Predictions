# packages ----


# read data ----

glrn_xgboost <- readRDS("temp/glrnxgboost_final.rds")
task <- readRDS("temp/task.rds")
task_val_2021 <- readRDS("temp/task_val_2021.rds")
task_val_2022 <- readRDS("temp/task_val_2022.rds")
task_val_2023 <- readRDS("temp/task_val_2023.rds")

#  helper: remove any special column roles (e.g., always_included) ----
make_clean_task <- function(t) {
  dt <- as.data.table(t$data(cols = c(t$target_names, t$feature_names)))
  TaskClassif$new(id = paste0(t$id, "_clean"), backend = dt, target = t$target_names)
}
task_clean <- make_clean_task(task)

dt_val <- rbindlist(list(
  as.data.table(task_val_2021$data()),
  as.data.table(task_val_2022$data()),
  as.data.table(task_val_2023$data())
), fill = TRUE)

task_val_all <- TaskClassif$new(id = "val_all", backend = dt_val, target = "BK")
X_ref <- task_val_all$data(cols = task_clean$feature_names)

# bootstrap SHAP stability (global importance = mean(|phi|)) ----
set.seed(1)
B <- 200 # bootstrap repeats
M <- 30 # ref points per repeat
S <- 200 # Shapley sample.size

boot_imp <- rbindlist(lapply(1:B, function(b) {
  # bootstrap TRAINING rows
  ids <- sample(task_clean$row_ids, replace = TRUE)
  task_b <- task_clean$clone()
  task_b$filter(ids)

  # refit model (your graph learner)
  lrn_b <- glrn_xgboost$clone()
  lrn_b$train(task_b)

  # iml predictor
  pred_b <- Predictor$new(
    model = lrn_b,
    data  = task_b$data(cols = task_clean$feature_names),
    y     = task_b$data(cols = task_clean$target_names)[[1]]
  )

  # compute SHAP on fixed reference set
  ii <- sample(seq_len(nrow(X_ref)), min(M, nrow(X_ref)))
  shap_b <- rbindlist(lapply(ii, function(i) {
    sh <- Shapley$new(pred_b, x.interest = X_ref[i, ], sample.size = S)
    as.data.table(sh$results)[, .(feature, phi)]
  }))

  out <- shap_b[, .(imp = mean(abs(phi))), by = feature]
  out[, bootstrap := b]
  out
}))

saveRDS(boot_imp, "temp/boot_shap_importance.rds")


saveRDS(boot_imp, "temp/boot_shap_importance.rds")

readRDS("temp/boot_shap_importance.rds")

rank_tbl <- boot_imp[, .(
  rank = rank(-imp),
  feature = feature
), by = .(bootstrap)]

feature_stab <- rank_tbl[, .(
  rank_med = round(median(rank), 0),
  rank_lo  = round(as.numeric(quantile(rank, 0.025)), 0),
  rank_hi  = round(as.numeric(quantile(rank, 0.975)), 0)
), by = feature][order(rank_med)]

setnames(feature_stab,
  old = c("feature", "rank_med", "rank_lo", "rank_hi"),
  new = c("Feature", "Rank Median", "Rank low", "Rank high")
)
feature_stab <- xtable(feature_stab,
  digits = c(0, 0, 0, 0, 0)
)
colnames(feature_stab) <- paste0("\\textbf{", colnames(feature_stab), "}")

print.xtable(
  feature_stab,
  file = "tab/feature_stab.tex",
  include.rownames = FALSE,
  floating = FALSE,
  sanitize.colnames.function = identity,
  booktabs = TRUE
)


# clean workspace ----

rm(list = setdiff(ls(), c("")))
