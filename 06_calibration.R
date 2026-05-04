# load package for visualization functions ----
library(mlr3viz)

# calibration ----
# load out-of-fold benchmark predictions
x <- readRDS("temp/benchmark_predictions_final.rds")

# create data frame with true class labels and predicted probabilities
x1 <- data.frame(
  x$truth,
  x$data$prob
)

# calibration with 10-fold cross-validation
set.seed(1)
K <- 10
fold <- sample(rep(1:K, length.out = nrow(x1)))

# create binary outcome: 1 = Bundeskader, 0 = other
y <- as.numeric(x1$x.truth == "Bundeskader")

# initialize vectors for calibrated out-of-fold predictions
p_glm <- rep(NA_real_, nrow(x1))
p_iso <- rep(NA_real_, nrow(x1))

# loop over cross-validation folds
for (k in 1:K) {
  # define test and training indices
  te <- fold == k
  tr <- !te

  # create training and test data for calibration models
  dtr <- data.frame(y = y[tr], p = x1$Bundeskader[tr])
  dte <- data.frame(p = x1$Bundeskader[te])

  # fit logistic calibration model (Platt scaling) on training fold
  cal_glm <- glm(y ~ p, data = dtr, family = binomial())

  # predict calibrated probabilities for test fold
  p_glm[te] <- predict(cal_glm, newdata = dte, type = "response")

  # fit isotonic regression on training fold
  o <- order(dtr$p)
  iso <- isoreg(dtr$p[o], dtr$y[o])

  # predict isotonic-calibrated probabilities for test fold
  p_iso[te] <- approx(
    x = dtr$p[o], y = iso$yf, xout = dte$p,
    rule = 2, ties = "ordered"
  )$y
}

# compare Brier scores before and after calibration
c(
  brier_before = mean((x1$Bundeskader - y)^2, na.rm = TRUE),
  brier_glm_oos = mean((p_glm - y)^2, na.rm = TRUE),
  brier_iso_oos = mean((p_iso - y)^2, na.rm = TRUE)
)

# function to create binned calibration plots
plot_binned_cal <- function(probs, truth, title) {
  # group predicted probabilities into bins of width 0.1
  bins <- cut(probs, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)

  # calculate mean predicted probability and observed event rate per bin
  bin_means <- tapply(probs, bins, mean)
  real_rates <- tapply(truth, bins, mean)
  ok <- !is.na(bin_means) & !is.na(real_rates)

  # plot calibration curve
  plot(bin_means[ok], real_rates[ok],
    xlim = c(0, 1), ylim = c(0, 1), pch = 19, col = "blue",
    xlab = "Predicted probability", ylab = "Observed rate", main = title
  )

  # add reference line for perfect calibration
  abline(0, 1, lty = 2, col = "grey")

  # connect calibration points
  lines(bin_means[ok], real_rates[ok], col = "blue", lwd = 1)

  # add rugs for positive and negative cases
  rug(probs[truth == 1], side = 3, col = "darkgreen")
  rug(probs[truth == 0], side = 1, col = "red")
}

# show calibration plots before and after calibration
par(mfrow = c(1, 3))
plot_binned_cal(x1$Bundeskader, y, "Before (Bundeskader, OOF)")
plot_binned_cal(p_glm, y, "Logistic (Bundeskader, OOF)")
plot_binned_cal(p_iso, y, "Isotonic (Bundeskader, OOF)")
par(mfrow = c(1, 1))

# fit final logistic calibration model on all data
cal_glm_final <- glm(y ~ Bundeskader, data = x1, family = binomial())

# save final logistic calibration model
saveRDS(cal_glm_final, file = "temp/glm_calibrator_bundeskader.rds")

# prepare full data for final isotonic calibration
p_all <- x1$Bundeskader
y_all <- as.numeric(x1$x.truth == "Bundeskader")

# sort probabilities for isotonic regression
o_all <- order(p_all)
iso_final <- isoreg(p_all[o_all], y_all[o_all])

# save final isotonic calibration model
saveRDS(list(x = p_all[o_all], yf = iso_final$yf),
  file = "temp/iso_calibrator.rds"
)

# remove all objects from workspace ----
rm(list = setdiff(ls(), c("")))
