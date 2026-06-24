library(colorspace)
library(xtable)
library(mlr3)
library(vioplot)


q2 <- colorspace::diverging_hcl(2, palette = "Blue-Red 2", alpha = 1)
q2_legend <- colorspace::diverging_hcl(2, palette = "Blue-Red 2", alpha = 1)

#### read data ----
tsk_data <- readRDS("temp/task.rds")
tsk_data <- tsk_data$data()

# setcolname
colnames(tsk_data)[colnames(tsk_data) == "ASW"] <- "ASW"
colnames(tsk_data)[colnames(tsk_data) == "Ausd"] <- "EndRun"
colnames(tsk_data)[colnames(tsk_data) == "B2000"] <- "B2000"
colnames(tsk_data)[colnames(tsk_data) == "B250"] <- "B250"
colnames(tsk_data)[colnames(tsk_data) == "KG"] <- "BM"
colnames(tsk_data)[colnames(tsk_data) == "KH"] <- "BH"
colnames(tsk_data)[colnames(tsk_data) == "KWW"] <- "BT"
colnames(tsk_data)[colnames(tsk_data) == "Kraft"] <- "BP"
colnames(tsk_data)[colnames(tsk_data) == "Sprint_30m"] <- "LS"
colnames(tsk_data)[colnames(tsk_data) == "h_unten"] <- "SH"
colnames(tsk_data)[colnames(tsk_data) == "Disziplin"] <- "Discipline"
colnames(tsk_data)[colnames(tsk_data) == "alter"] <- "Age"
colnames(tsk_data)[colnames(tsk_data) == "BK"] <- "PL"

levels(tsk_data$Discipline)[levels(tsk_data$Discipline) == "Kajak_m"] <- "Kayak male"
levels(tsk_data$Discipline)[levels(tsk_data$Discipline) == "Kajak_w"] <- "Kayak female"
levels(tsk_data$Discipline)[levels(tsk_data$Discipline) == "Canadier"] <- "Canoe"
levels(tsk_data$PL)[levels(tsk_data$PL) == "Landeskader"] <- "National"
levels(tsk_data$PL)[levels(tsk_data$PL) == "Bundeskader"] <- "International"

#### table descriptive ----

l <- split(tsk_data, by = c(
  "Discipline",
  "PL",
  "Age"
))
l <- l[lapply(l, nrow) > 0]
source("src/getStats.R")
desc <- lapply(l, function(x) getStats(x))
desc <- rbindlist(desc)
setorder(desc, Discipline, -Age)

print.xtable(xtable(desc),
  file = "tab/tabdesc.tex",
  auto = TRUE,
  floating = FALSE,
  include.rownames = FALSE,
  sanitize.colnames.function = identity,
)

#### strip chart ----
# plot

tsk_data_long <- melt(tsk_data,
  id.vars = c("PL", "Discipline", "Age")
)
tsk_data_long <- data.table(tsk_data_long)
# kayak_female
kayak_female <- tsk_data_long[Discipline == "Kayak female" & Age != 17 & Age != 4]
kayak_female_list <- split(kayak_female, by = "variable")

kayak_female_list <- kayak_female_list[c(
  "ASW",
  "BM",
  "BH",
  "SH",
  "BP",
  "BT",
  "LS",
  "EndRun",
  "B2000",
  "B250"
)]



source("src/plot_boxplots.R")
cairo_pdf("fig/Saal_Kanu_stripchart_kayak_female.pdf",
  height = 6,
  width = 12
)
par(mfrow = c(2, 5), oma = c(3, 2, 4, 1), xpd = NA)
lapply(kayak_female_list, function(x) plot_boxplots(x))
title(xlab = "Age", outer = TRUE, line = 0, cex.lab = 1.5, adj = 0.5)
title(ylab = "Points/Units", outer = TRUE, line = 0.5, cex.lab = 1.5, adj = 0.5, las = 3)
title(main = "Kayak Female", outer = TRUE, line = 1, cex.main = 2)
legend("bottom",
  c("International", "National"),
  col = q2_legend,
  pch = 16,
  cex = 1.4,
  horiz = TRUE,
  bty = "n",
  inset = -0.5,
)
dev.off()

kayak_male <- tsk_data_long[Discipline == "Kayak male"]
kayak_male_list <- split(kayak_male, by = "variable")
kayak_male_list <- kayak_male_list[c(
  "ASW",
  "BM",
  "BH",
  "SH",
  "BP",
  "BT",
  "LS",
  "EndRun",
  "B2000",
  "B250"
)]

source("src/plot_boxplots.R")
cairo_pdf("fig/Saal_Kanu_stripchart_kayak_male.pdf",
  height = 6,
  width = 12
)
par(mfrow = c(2, 5), oma = c(3, 2, 4, 1), xpd = NA)
lapply(kayak_male_list, function(x) plot_boxplots(x))
title(xlab = "Age", outer = TRUE, line = 0, cex.lab = 1.5, adj = 0.5)
title(ylab = "Points/Units", outer = TRUE, line = 0.5, cex.lab = 1.5, adj = 0.5, las = 3)
title(main = "Kayak Male", outer = TRUE, line = 1, cex.main = 2)
legend("bottom",
  c("International", "National"),
  col = q2_legend,
  pch = 16,
  horiz = TRUE,
  bty = "n",
  inset = -0.5,
)
dev.off()

canoe <- tsk_data_long[Discipline == "Canoe"]
canoe_list <- split(canoe, by = "variable")
canoe_list <- canoe_list[c(
  "ASW",
  "BM",
  "BH",
  "SH",
  "BP",
  "BT",
  "LS",
  "EndRun",
  "B2000",
  "B250"
)]

source("src/plot_boxplots.R")
cairo_pdf("fig/Saal_Kanu_stripchart_canoe.pdf",
  height = 6,
  width = 12
)
par(mfrow = c(2, 5), oma = c(3, 2, 4, 1), xpd = NA)
lapply(canoe_list, function(x) plot_boxplots(x))
title(xlab = "Age", outer = TRUE, line = 0, cex.lab = 1.5, adj = 0.5)
title(ylab = "Points/Units", outer = TRUE, line = 0.5, cex.lab = 1.5, adj = 0.5, las = 3)
title(main = "Canoe", outer = TRUE, line = 1, cex.main = 2)
legend("bottom",
  c("International", "National"),
  col = q2_legend,
  pch = 16,
  horiz = TRUE,
  bty = "n",
  inset = -0.5,
)
dev.off()



#### pairs ----
l <- split(tsk_data, by = "Discipline")

## plot canadier ----

cairo_pdf("fig/pairs_canadier.pdf",
  height = 10,
  width = 10
)
par()
x <- l[["Canoe"]]
col_BK <- qualitative_hcl(1, palette = "Dark 3")
col_LK <- gray(.0, .1)
mycols <- c(col_BK, col_LK)
pairs(x[, !"Discipline"],
  pch = 16,
  cex = 1,
  col = mycols[x$PL],
  lower.panel = NULL,
  gap = 1,
  oma = c(3.1, 3.1, 5, 3.1)
)
mtext("Canadier",
  side = 3,
  line = 3,
  font = 2,
  cex = 1.2,
  adj = 0
)
dev.off()

## plot kanu m ----
cairo_pdf("fig/pairs_kajak_m.pdf",
  height = 10,
  width = 10
)
par()
x <- l[["Kayak male"]]
col_BK <- qualitative_hcl(1, palette = "Dark 3")
col_LK <- gray(.0, .1)
mycols <- c(col_BK, col_LK)
pairs(x[, !"Discipline"],
  pch = 16,
  cex = 1,
  col = mycols[x$PL],
  lower.panel = NULL,
  gap = 1,
  oma = c(3.1, 3.1, 5, 3.1)
)
mtext("Kayak male",
  side = 3,
  line = 3,
  font = 2,
  cex = 1.2,
  adj = 0
)
dev.off()

## plot kanu w ----
cairo_pdf("fig/pairs_kajak_w.pdf",
  height = 10,
  width = 10
)
par()
x <- l[["Kayak female"]]
col_BK <- qualitative_hcl(1, palette = "Dark 3")
col_LK <- gray(.0, .1)
mycols <- c(col_BK, col_LK)
pairs(x[, !"Discipline"],
  pch = 16,
  cex = 1,
  col = mycols[x$PL],
  lower.panel = NULL,
  gap = 1,
  oma = c(3.1, 3.1, 5, 3.1)
)
mtext("Kayak female",
  side = 3,
  line = 3,
  font = 2,
  cex = 1.2,
  adj = 0
)
dev.off()

# frequency table ----

tsk_data <- readRDS("temp/task.rds")
tsk_data <- tsk_data$data()
# tsk_data <- tsk_data[!(Age == 4 | Age == 17)]

tab <- ftable(addmargins(table(
  tsk_data$Discipline,
  tsk_data$PL,
  tsk_data$Age
)))

print.xtableFtable(xtableFtable(tab),
  file = "tab/tabfrequency.tex",
  auto = TRUE,
  floating = FALSE,
  sanitize.colnames.function = identity,
)
