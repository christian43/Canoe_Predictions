set.seed(1234)

# load packages ----

library(colorspace)
library(iml)
library(mlr3)
library(mlr3pipelines)
library(future)

# get task and learner ----

# load model and task
glrn_xgboost <- readRDS("temp/glrnxgboost_final.rds")
# trainingset
task <- readRDS("temp/task.rds")

# validationset
task_val_2021 <- readRDS("temp/task_val_2021.rds")
task_val_2022 <- readRDS("temp/task_val_2022.rds")
task_val_2023 <- readRDS("temp/task_val_2023.rds")

dt <- rbindlist(list(
  task_val_2021$data(),
  task_val_2022$data(),
  task_val_2023$data()
), use.names = TRUE, fill = TRUE)

task_val_all <- as_task_classif(
  dt,
  target = "BK", # Target variable for classification
  id = "kanu_val_all", # Task identifier
  positive = "Bundeskader" # Define the positive class (for binary classification)
)

# print table subject description ----
tab <- ftable(addmargins(table(
  task_val_all$data()$Disziplin,
  task_val_all$data()$BK,
  task_val_all$data()$alter
)))

# iml shap on validationset global ----

predictor <- Predictor$new(
  model = glrn_xgboost,
  data = task$data()[, task$feature_names, with = FALSE],
  y = task$data()[[task$target_names]]
)

X <- task_val_all$data(cols = task_val_all$feature_names)

set.seed(123)
idx <- seq_len(nrow(X)) # alle 103 Beobachtungen

shap_list <- lapply(idx, function(i) {
  Shapley$new(
    predictor,
    x.interest  = X[i, ],
    sample.size = 300
  )
})

# Shapley-Objekte zu einem Dataframe aggregieren
shap_dt <- rbindlist(lapply(shap_list, function(sh) {
  as.data.table(sh$results)[, .(feature, class, phi, phi.var, feature.value)]
}))

saveRDS(shap_dt, "temp/shap_dt.rds")

shap_dt <- readRDS("temp/shap_dt.rds")

shap_dt[, c("feat.name", "val.raw") := tstrsplit(feature.value, "=", fixed = TRUE)]
shap_dt[val.raw %in% c("Kajak_w", "Kajak_m", "Canadier"),
        feat.name := val.raw]
shap_dt[, value.num := suppressWarnings(as.numeric(val.raw))]
shap_dt[val.raw %in% c("Kajak_w", "Kajak_m", "Canadier"),
        value.num := 1]

shap_dt$feat.name[shap_dt$feat.name == "Kajak_w"] <- "Kayak Female"
shap_dt$feat.name[shap_dt$feat.name == "Kajak_m"] <- "Kayak Male"
shap_dt$feat.name[shap_dt$feat.name == "Canadier"] <- "Canoe"

shap_dt$feat.name[shap_dt$feat.name == "ASW"] <- "ASW"
shap_dt$feat.name[shap_dt$feat.name == "Ausd"] <- "EndRun"
shap_dt$feat.name[shap_dt$feat.name == "B2000"] <- "B2000"
shap_dt$feat.name[shap_dt$feat.name == "B250"] <- "B250"
shap_dt$feat.name[shap_dt$feat.name == "Disziplin"] <- "Discipline"
shap_dt$feat.name[shap_dt$feat.name == "KG"] <- "BM"
shap_dt$feat.name[shap_dt$feat.name == "KH"] <- "BH"
shap_dt$feat.name[shap_dt$feat.name == "KWW"] <- "BT"
shap_dt$feat.name[shap_dt$feat.name == "Kraft"] <- "BP"
shap_dt$feat.name[shap_dt$feat.name == "Sprint_30m"] <- "LS"
shap_dt$feat.name[shap_dt$feat.name == "h_unten"] <- "SH"
shap_dt$feat.name[shap_dt$feat.name == "alter"] <- "Age"

shap_dt$feature[shap_dt$feature == "ASW"] <- "ASW"
shap_dt$feature[shap_dt$feature == "Ausd"] <- "EndRun"
shap_dt$feature[shap_dt$feature == "B2000"] <- "B2000"
shap_dt$feature[shap_dt$feature == "B250"] <- "B250"
shap_dt$feature[shap_dt$feature == "Disziplin"] <- "Discipline"
shap_dt$feature[shap_dt$feature == "KG"] <- "BM"
shap_dt$feature[shap_dt$feature == "KH"] <- "BH"
shap_dt$feature[shap_dt$feature == "KWW"] <- "BT"
shap_dt$feature[shap_dt$feature == "Kraft"] <- "BP"
shap_dt$feature[shap_dt$feature == "Sprint_30m"] <- "LS"
shap_dt$feature[shap_dt$feature == "h_unten"] <- "SH"
shap_dt$feature[shap_dt$feature == "alter"] <- "Age"

levels(shap_dt$class)[levels(shap_dt$class) == "Bundeskader"] <- "International"
levels(shap_dt$class)[levels(shap_dt$class) == "Landeskader"] <- "National"

# Globale Importance: mittlerer absoluter SHAP-Wert je Feature
global_shap <- shap_dt[class == "International",   # Zielklasse wählen
  .(mean_abs_phi = mean(abs(phi))),
  by = feature
][order(mean_abs_phi)]

col_imp <- sequential_hcl(
  n = length(imp),
  palette = "Purples 2",
)

cairo_pdf("fig/Saal_Kanu_global_shap_validationset.pdf", 
          width = 8, 
          height = 6)
par(mar = c(4, 8, 4, 2))
barplot(
  global_shap$mean_abs_phi,
  names.arg = global_shap$feature,
  horiz = TRUE,
  las = 2,
  main = "Global SHAP feature importance for (P(International)))",
  cex.names = 1,
  xlab = "Mean absolute SHAP value",
  border = "white",
  col = rev(col_imp),
  xaxt = "n"
)
grid(NULL, NA)
axis(1,
  at = pretty(c(0, max(global_shap$mean_abs_phi))),
  labels = pretty(c(0, max(global_shap$mean_abs_phi))),
  tick = FALSE,
  las = 1,
  lwd = 1
)
dev.off()

# Breimans permutation on validationset -----

predictor <- Predictor$new(
  model = glrn_xgboost,
  data = task_val_all$data()[, task_val_all$feature_names, with = FALSE],
  y = task_val_all$data()[[task_val_all$target_names]]
)

set.seed(1234)
imp_perm <- FeatureImp$new(predictor,
  loss = "ce",
  n.repetitions = 100
)
saveRDS(imp_perm, 'temp/imp_perm.rds'

# check importance from model ----

imp <- glrn_xgboost$importance()
imp <- sort(imp)

