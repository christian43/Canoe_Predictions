getStats <- function(x) {
  n <- nrow(x)
  ind <- which(sapply(x, class) == "numeric")
  vec <- names(ind)
  means <- round(apply(x[, ..ind], 2, mean), 1)
  sds <- round(apply(x[, ..ind], 2, sd), 1)
  sds[is.na(sds)] <- 0
  means <- paste0(
    means,
    "(",
    sds,
    ")"
  )

  names(means) <- vec
  means <- t(data.frame(means))
  rownames(means) <- NULL
  tab <- data.frame(
    Discipline = unique(x$Discipline),
    Age = unique(x$Age),
    n = n,
    PL = unique(x$PL)
  )
  tab <- cbind(tab, means)
  tab <- as.data.table(tab)
  setcolorder(tab, c(
    "Discipline",
    "Age",
    "PL",
    "n",
    "BP",
    "EndRun",
    "BT",
    "LS",
    "B2000",
    "B250",
    "BM",
    "ASW",
    "BH",
    "SH"
  ))
  return(tab)
}
