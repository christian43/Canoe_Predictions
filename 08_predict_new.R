# Packages  -----

library(data.table)
library(mlr3)
library(mlr3verse)
library(mlr3tuningspaces)
library(mlr3tuning)
library(mlr3extralearners)
library(xtable)
library(MASS)
library(rstatix)
library(colorspace)
library(xtable)
library(lubridate)
library(caret)
library(pROC)
library(cvms)
set.seed(657)

# load learner and results from benchmarking ----

glrn_xgboost <- readRDS("temp/glrnxgboost_final.rds")

iso_mod <- readRDS("temp/iso_calibrator.rds")

# Predicitions on validation set ----

groundtruth <- fread("data/csv/Athleten_17-03-2025-6.csv")
vec_nachname <- groundtruth$Nachname
vec <- c(
  "Voigt",
  "Methner",
  "Gessert",
  "Haase",
  "Grüneich",
  "Tege",
  "Csides",
  "Vandersee",
  "Vangermain",
  "Orphal"
)

# validation set 2021 -----

new_2021 <- fread("data/csv/2021_newdata.csv", header = TRUE, sep = ";", dec = ",")
new_2021$alter <- 2021 - new_2021$alter
new_2021 <- new_2021[apply(new_2021 != 0, 1, all), ]
new_2021$Disziplin <- as.factor(new_2021$Disziplin)

vec <- new_2021[, Name %in% vec_nachname]
new_2021[, BK := ifelse(vec, "Bundeskader", "Landeskader")]
new_2021[BK == "Bundeskader"]

# alle integer-Spalten zu numeric umwandeln
new_2021[] <- lapply(new_2021, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})
new_2021$alter <- as.integer(new_2021$alter)
saveRDS(new_2021, "temp/new_2021.rds")

# Validation Set als Task
task_val_2021 <- as_task_classif(
  new_2021[, -c(1, 2, 3), with = FALSE],
  target = "BK", # Spaltenname deiner Labels
  positive = "Bundeskader", # oder "Bundeskader", je nach Kodierung
  id = "val2021"
)

saveRDS(task_val_2021, "temp/task_val_2021.rds")

task_val_2021 <- readRDS("temp/task_val_2021.rds")

# Modell auf Validierungs-Task anwenden
pred_val_2021 <- glrn_xgboost$predict(task_val_2021)


pred_val_2021$confusion

bk_cases <- as.data.table(pred_val_2021)
bk_cases[truth == "Bundeskader"]

# mehrere Kennzahlen definieren
as.data.table(mlr_measures)

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

# Performance berechnen
pred_val_2021_scores <- pred_val_2021$score(measures)

# Konvertieren (falls noch nicht data.table)
pred_val_2021 <- as.data.table(pred_val_2021)
# Prob-Spalten aus pred anhängen (incl. truth/response)
full_2021 <- cbind(new_2021, pred_val_2021[, .(truth, response, prob.Bundeskader, prob.Landeskader)])

# validation set 2022 -----

new_2022 <- fread("data/csv/2022_newdata.csv", header = TRUE, sep = ";", dec = ",")
new_2022$alter <- 2022 - new_2022$alter
new_2022 <- new_2022[apply(new_2022 != 0, 1, all), ]
new_2022$KG <- as.numeric(new_2022$KG)
new_2022$Disziplin <- factor(new_2022$Disziplin,
  levels = c("Canadier", "Kajak_m", "Kajak_w")
)

vec <- new_2022[, Name %in% vec_nachname]
new_2022[, BK := ifelse(vec, "Bundeskader", "Landeskader")]
# check
new_2022[BK == "Bundeskader"]
new_2022[Name == "Müller", BK := "Landeskader"]

# alle integer-Spalten zu numeric umwandeln
new_2022[] <- lapply(new_2022, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})
new_2022$alter <- as.integer(new_2022$alter)
saveRDS(new_2022, "temp/new_2022.rds")

# Validation Set als Task
task_val_2022 <- as_task_classif(
  new_2022[, -c(1, 2, 3), with = FALSE],
  target = "BK", # Spaltenname deiner Labels
  positive = "Bundeskader", # oder "Bundeskader", je nach Kodierung
  id = "val2022"
)

saveRDS(task_val_2022, "temp/task_val_2022.rds")


# Modell auf Validierungs-Task anwenden
task_val_2022 <- readRDS("temp/task_val_2022.rds")
pred_val_2022 <- glrn_xgboost$predict(task_val_2022)
pred_val_2022$confusion

bk_cases <- as.data.table(pred_val_2022)
bk_cases[truth == "Bundeskader"]

# Performance berechnen
pred_val_2022_scores <- pred_val_2022$score(measures)

# Konvertieren (falls noch nicht data.table)
pred_val_2022 <- as.data.table(pred_val_2022)
# Prob-Spalten aus pred anhängen (incl. truth/response)
full_2022 <- cbind(new_2022, pred_val_2022[, .(truth, response, prob.Bundeskader, prob.Landeskader)])

# validation set 2023 -----

new_2023 <- fread("data/csv/2023_newdata.csv", header = TRUE, sep = ";", dec = ",")
new_2023$alter <- 2023 - new_2023$alter
# nehme 13 jährige raus, das zum Zeitpunkt 2025 für die 13 jährigen (2023) noch keine NK2 Status vergeben wird
new_2023 <- new_2023[!alter <= 13]
new_2023$KG <- as.numeric(new_2023$KG)

new_2023$Disziplin <- factor(new_2023$Disziplin,
  levels = c("Canadier", "Kajak_m", "Kajak_w")
)

vec <- new_2023[, Name %in% vec_nachname]
new_2023[, BK := ifelse(vec, "Bundeskader", "Landeskader")]
new_2023[BK == "Bundeskader"]
new_2023[Name == "Müller", BK := "Landeskader"]
new_2023[Name == "Brendel", BK := "Landeskader"]

# alle integer-Spalten zu numeric umwandeln
new_2023[] <- lapply(new_2023, function(x) {
  if (is.integer(x)) as.numeric(x) else x
})
new_2023$alter <- as.integer(new_2023$alter)
saveRDS(new_2023, "temp/new_2023.rds")

# Validation Set als Task
task_val_2023 <- as_task_classif(
  new_2023[, -c(1, 2, 3), with = FALSE],
  target = "BK", # Spaltenname deiner Labels
  positive = "Bundeskader", # oder "Bundeskader", je nach Kodierung
  id = "val2023"
)
saveRDS(task_val_2023, "temp/task_val_2023.rds")
task_val_2023 <- readRDS("temp/task_val_2023.rds")

# Modell auf Validierungs-Task anwenden
pred_val_2023 <- glrn_xgboost$predict(task_val_2023)
pred_val_2023$confusion
bk_cases <- as.data.table(pred_val_2023)
bk_cases[truth == "Bundeskader"]
vec <- bk_cases[truth == "Bundeskader"]$row_ids

# Performance berechnen
pred_val_2023_scores <- pred_val_2023$score(measures)

# Konvertieren (falls noch nicht data.table)
pred_val_2023 <- as.data.table(pred_val_2023)
# Prob-Spalten aus pred anhängen (incl. truth/response)

full_2023 <- cbind(new_2023, pred_val_2023[, .(truth, response, prob.Bundeskader, prob.Landeskader)])

# print latex table prediction scores ----
scores <- rbind(
  pred_val_2021_scores,
  pred_val_2022_scores,
  pred_val_2023_scores
)
scores <- as.data.table(scores)
scores[, "Validation set" := c("2021", "2022", "2023")] # Rownames als Spalte
setcolorder(scores, c("Validation set", setdiff(names(scores), "Validation set")))


xt_scores <- xtable(scores)
colnames(xt_scores) <- paste0("\\textbf{", colnames(xt_scores), "}")

print.xtable(xt_scores,
  file = "tab/score_validationset.tex",
  include.rownames = FALSE,
  floating = FALSE,
  sanitize.colnames.function = identity
)

# print latex table confusion matrices ----

conf2021 <- pred_val_2021$confusion
colnames(conf2021) <- c("International", "National")
rownames(conf2021) <- c("International", "National")
conf2021 <- as.matrix(conf2021)
rownames(conf2021) <- paste("response", rownames(conf2021), sep = ": ")
colnames(conf2021) <- paste("truth", colnames(conf2021), sep = ": ")
print.xtable(
  xtable(conf2021),
  file = "tab/conf2021.tex",
  include.rownames = TRUE,
  floating = FALSE,
  sanitize.colnames.function = identity,
  booktabs = TRUE
)

conf2022 <- pred_val_2022$confusion
colnames(conf2022) <- c("International", "National")
rownames(conf2022) <- c("International", "National")
conf2022 <- as.matrix(conf2022)
rownames(conf2022) <- paste("response", rownames(conf2022), sep = ": ")
colnames(conf2022) <- paste("truth", colnames(conf2022), sep = ": ")
print.xtable(
  xtable(conf2022),
  file = "tab/conf2022.tex",
  include.rownames = TRUE,
  floating = FALSE,
  sanitize.colnames.function = identity,
  booktabs = TRUE
)


conf2023 <- pred_val_2023$confusion
colnames(conf2023) <- c("International", "National")
rownames(conf2023) <- c("International", "National")
conf2023 <- as.matrix(conf2023)
rownames(conf2023) <- paste("response", rownames(conf2023), sep = ": ")
colnames(conf2023) <- paste("truth", colnames(conf2023), sep = ": ")
print.xtable(
  xtable(conf2023),
  file = "tab/conf2023.tex",
  include.rownames = TRUE,
  floating = FALSE,
  sanitize.colnames.function = identity,
  booktabs = TRUE
)

# final table ----

full <- rbind(
  full_2021,
  full_2022,
  full_2023
)

vec <- c(
  "Voigt",
  "Methner",
  "Gessert",
  "Haase",
  "Grüneich",
  "Tege",
  "Csides",
  "Vandersee",
  "Vangermain",
  "Orphal"
)

full[Name %in% vec]

full[Datum == "18757", Datum := "2021-05-10"]
full[Datum == "19077", Datum := "2022-03-26"]
full[Datum == "19441", Datum := "2023-03-25"]

fwrite(full[, !"BK"], "temp/full.csv")

# print frequency ----


new_2021 <- readRDS("temp/new_2021.rds")
new_2022 <- readRDS("temp/new_2022.rds")
new_2023 <- readRDS("temp/new_2023.rds")

new <- rbind(new_2021, new_2022, new_2023)

tab <- ftable(addmargins(table(
  new$Disziplin,
  new$BK,
  new$alter
)))

table(table(paste(new$Name, new$Vorname)))

# age distribution of validation at last measurment
new[, max(alter), by = paste(new$Name, new$Vorname)][, list(mean(V1), sd(V1))]

print.xtableFtable(xtableFtable(tab),
  file = "tab/tabfrequency_validationset.tex",
  auto = TRUE,
  floating = FALSE,
  sanitize.colnames.function = identity,
)

# plot confusion matrix ----

task_val_2021 <- readRDS("temp/task_val_2021.rds")
task_val_2022 <- readRDS("temp/task_val_2022.rds")
task_val_2023 <- readRDS("temp/task_val_2023.rds")
glrn_xgboost <- readRDS("temp/glrnxgboost_final.rds")


pred_val_2021 <- glrn_xgboost$predict(task_val_2021)
cm <- pred_val_2021$confusion
df_long <- as.data.frame(as.table(cm))
names(df_long) <- c("Prediction", "Target", "N") # Target = truth
levels(df_long$Prediction)[levels(df_long$Prediction) == "Bundeskader"] <- "International"
levels(df_long$Target)[levels(df_long$Target) == "Bundeskader"] <- "International"
levels(df_long$Prediction)[levels(df_long$Prediction) == "Landeskader"] <- "National"
levels(df_long$Target)[levels(df_long$Target) == "Landeskader"] <- "National"

p_cm <- plot_confusion_matrix(df_long,
  palette = list(
    low  = colorspace::sequential_hcl(3, palette = "Blues 3")[3],
    high = colorspace::sequential_hcl(3, palette = "Blues 3")[2]
  )
)

ggsave("fig/Saal_Kanu_confusionmatrix_2021.pdf",
  p_cm,
  width = 6,
  height = 6,
  units = "in"
)

pred_val_2022 <- glrn_xgboost$predict(task_val_2022)
cm <- pred_val_2022$confusion
df_long <- as.data.frame(as.table(cm))
names(df_long) <- c("Prediction", "Target", "N") # Target = truth
levels(df_long$Prediction)[levels(df_long$Prediction) == "Bundeskader"] <- "International"
levels(df_long$Target)[levels(df_long$Target) == "Bundeskader"] <- "International"
levels(df_long$Prediction)[levels(df_long$Prediction) == "Landeskader"] <- "National"
levels(df_long$Target)[levels(df_long$Target) == "Landeskader"] <- "National"

p_cm <- plot_confusion_matrix(df_long,
  palette = list(
    low  = colorspace::sequential_hcl(3, palette = "Blues 3")[3],
    high = colorspace::sequential_hcl(3, palette = "Blues 3")[2]
  )
)

ggsave("fig/Saal_Kanu_confusionmatrix_2022.pdf",
  p_cm,
  width = 6,
  height = 6,
  units = "in"
)

pred_val_2023 <- glrn_xgboost$predict(task_val_2023)
cm <- pred_val_2023$confusion
df_long <- as.data.frame(as.table(cm))
names(df_long) <- c("Prediction", "Target", "N") # Target = truth
levels(df_long$Prediction)[levels(df_long$Prediction) == "Bundeskader"] <- "International"
levels(df_long$Target)[levels(df_long$Target) == "Bundeskader"] <- "International"
levels(df_long$Prediction)[levels(df_long$Prediction) == "Landeskader"] <- "National"
levels(df_long$Target)[levels(df_long$Target) == "Landeskader"] <- "National"

p_cm <- plot_confusion_matrix(df_long,
  palette = list(
    low  = colorspace::sequential_hcl(3, palette = "Blues 3")[3],
    high = colorspace::sequential_hcl(3, palette = "Blues 3")[2]
  )
)

ggsave("fig/Saal_Kanu_confusionmatrix_2023.pdf",
  p_cm,
  width = 6,
  height = 6,
  units = "in"
)
# age by year ---

lapply(list(
+   `2021` = task_val_2021,
+   `2022` = task_val_2022,
+   `2023` = task_val_2023
+ ), \(x) as.data.table(x$data())[, .N, by = alter][order(alter)])
