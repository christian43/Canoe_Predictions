#
# packages ----
library(xtable)
library(effsize)

# calc CI from quantiles ----
# Load the saved benchmark results from an RDS file
res_task <- readRDS("temp/res_task_benchmark_final.rds")

measures <- list(
  msr("classif.auc"),
  msr("classif.bacc"),
  msr("classif.logloss"),
  msr("classif.prauc"),
  msr("classif.sensitivity"), # Recall(Bundeskader)
  msr("classif.specificity"), # Recall(Landeskader)
  msr("classif.ppv"), # Precision(Bundeskader)
  msr("classif.npv") # Precision(Landeskader)
)

ids <- vapply(measures, \(m) m$id, character(1))
sc <- as.data.table(res_task$score(measures))

ci_tbl <- sc[, lapply(.SD, function(x) {
  x <- na.omit(x)
  m <- mean(x)
  lo <- as.numeric(quantile(x, 0.025))
  up <- as.numeric(quantile(x, 0.975))
  sprintf("%.2f [%.2f, %.2f]", m, lo, up)
}), by = .(task_id, learner_id), .SDcols = ids]
ci_tbl[, task_id := NULL]

saveRDS(ci_tbl, "temp/ci_tbl.rds")

ci_tbl <- xtable(ci_tbl)
colnames(ci_tbl) <- paste0("\\textbf{", colnames(ci_tbl), "}")

print.xtable(ci_tbl,
  file = "tab/scorerestask_final_CI.tex",
  include.rownames = FALSE,
  floating = FALSE,
  sanitize.colnames.function = identity,
  booktabs = TRUE
)


# calc F1 ----

fb <- as.data.table(res_task$score(list(msr("classif.fbeta", beta = 1))))

sumtbl <- fb[, .(
  mean_f1 = mean(classif.fbeta, na.rm = TRUE),
  n_na    = sum(is.na(classif.fbeta)),
  n_nan   = sum(is.nan(classif.fbeta)),
  n_total = .N
), by = .(task_id, learner_id)]

sumtbl


# Consider whether 2021-2023 cohorts differ systematically from training data (1992-2019) ----
task <- readRDS("temp/task.rds")
task_val_2021 <- readRDS("temp/task_val_2021.rds")
task_val_2022 <- readRDS("temp/task_val_2022.rds")
task_val_2023 <- readRDS("temp/task_val_2023.rds")

# helper: task -> data.table with cohort label
dt_task <- function(t, cohort) {
  dt <- as.data.table(t$data())
  dt[, cohort := cohort]
  dt
}

dt <- rbindlist(list(
  dt_task(task, "train_1992_2019"),
  dt_task(task_val_2021, "val_2021"),
  dt_task(task_val_2022, "val_2022"),
  dt_task(task_val_2023, "val_2023")
), fill = TRUE)

target <- task$target_names

# prevalence shift (BK-rate)
dt[, .(n = .N, pos_rate = mean(get(target) == task$positive)), by = cohort]

# feature shift ----
num_names <- setdiff(names(dt), c("BK", "Disziplin", "cohort"))

# KS + Effektgröße (standardized mean difference, Hedges g)
eff_vs_train <- function(feat, coh) {
  x <- na.omit(dt[cohort == "train_1992_2019", get(feat)])
  y <- na.omit(dt[cohort == coh, get(feat)])

  data.table(
    feature = feat,
    cohort = coh,
    n_train = length(x),
    n_val = length(y),
    ks_p = suppressWarnings(stats::ks.test(x, y)$p.value),
    hedges_g = effsize::cohen.d(y, x, hedges.correction = TRUE)$estimate
  )
}

res_shift <- rbindlist(lapply(num_names, \(f)
rbindlist(lapply(c("val_2021", "val_2022", "val_2023"), \(c2) eff_vs_train(f, c2)))))

# Top shifts overall
top_shift_overall <- res_shift[
  order(-abs(hedges_g))
][1:10]

# Top shifts per validation year
top_shift_by_year <- res_shift[
  order(cohort, -abs(hedges_g))
][, head(.SD, 5), by = cohort]

# Mean absolute shift per feature across years
top_shift_mean_feature <- res_shift[
  , .(mean_abs_g = mean(abs(hedges_g), na.rm = TRUE)),
  by = feature
][order(-mean_abs_g)]

top_shift_overall
top_shift_by_year
top_shift_mean_feature


# clean workspace ----

rm(list = setdiff(ls(), c("")))
