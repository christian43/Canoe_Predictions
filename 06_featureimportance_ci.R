set.seed(1234) # Ensure reproducibility for bootstrap resampling and any other random operations

# load packages ----
# colorspace: color palettes and color manipulation
# iml: model interpretability tools (e.g., SHAP-related workflows)
# mlr3 / mlr3pipelines: machine learning framework and pipeline support
# future: parallelization backend
library(colorspace)
library(iml)
library(mlr3)
library(mlr3pipelines)
library(future)

# iml shap on validationset ----

# Load precomputed SHAP results
shap_dt <- readRDS("temp/shap_dt.rds")

# Split combined "feature=value" string into feature name and raw value
shap_dt[, c("feat.name", "val.raw") := tstrsplit(feature.value, "=", fixed = TRUE)]

# For discipline categories, use the category itself as feature name
shap_dt[
  val.raw %in% c("Kajak_w", "Kajak_m", "Canadier"),
  feat.name := val.raw
]

# Try to convert raw feature values to numeric
shap_dt[, value.num := suppressWarnings(as.numeric(val.raw))]

# Encode discipline indicator variables as 1
shap_dt[
  val.raw %in% c("Kajak_w", "Kajak_m", "Canadier"),
  value.num := 1
]

# Rename feature labels to English / publication-friendly names
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

# Rename original feature column consistently with feat.name labels
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

# Recode class labels to English
levels(shap_dt$class)[levels(shap_dt$class) == "Bundeskader"] <- "International"
levels(shap_dt$class)[levels(shap_dt$class) == "Landeskader"] <- "National"

# Global feature importance:
# compute mean absolute SHAP value per feature for the "International" class,
# plus bootstrap confidence intervals
B <- 2000 # Number of bootstrap resamples

global_shap_boot <- shap_dt[class == "International",
  {
    x <- abs(phi) # Absolute SHAP values represent magnitude of contribution
    boot_means <- replicate(B, mean(sample(x, replace = TRUE))) # Bootstrap mean abs(SHAP)
    .(
      mean_abs = mean(x), # Observed mean absolute SHAP value
      lo = quantile(boot_means, 0.025, names = FALSE), # Lower 95% CI bound
      hi = quantile(boot_means, 0.975, names = FALSE), # Upper 95% CI bound
      n = .N # Number of SHAP observations for this feature
    )
  },
  by = feature
][order(mean_abs)] # Sort features by importance

# Create a sequential purple color palette for plotting
col_imp <- sequential_hcl(n = nrow(global_shap_boot), palette = "Purples 2")
col_imp <- lighten(col_imp, amount = 0.25)

# Save plot as PDF
cairo_pdf("~/Documents/Figures/Saal_Kanu_global_shap_validationset_ci.pdf",
  width = 6,
  height = 6
)

# Adjust plot margins and border style
par(
  mar = c(5, 7, 4, 7),
  bty = "L"
)

# Define x-axis limits based on bootstrap confidence interval range
xmax <- max(global_shap_boot$hi, na.rm = TRUE)
xmin <- min(0, global_shap_boot$lo, na.rm = TRUE)

# Y positions for features
y <- seq_len(nrow(global_shap_boot)) # Could also use reordered positions

# Plot mean absolute SHAP values as points
plot(global_shap_boot$mean_abs, y,
  xlim = c(xmin, xmax), yaxt = "n",
  xlab = "Mean absolute SHAP value",
  ylab = "",
  # main = "Global SHAP feature importance for P(International)",
  pch = 16,
  col = gray(0.15, 1)
)

# Add feature names on y-axis
axis(2,
  at = y,
  labels = global_shap_boot$feature,
  las = 2
)

# Add horizontal confidence interval bars
segments(global_shap_boot$lo,
  y,
  global_shap_boot$hi,
  y,
  col = gray(0.15, 1),
  lwd = 2
)

# Close PDF device and write file to disk
dev.off()

# clean workspace ----

# Remove all objects from the workspace
rm(list = setdiff(ls(), c("")))
