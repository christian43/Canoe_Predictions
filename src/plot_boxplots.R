plot_boxplots <- function(x) {
  par(
    bty = "L",
    pty = "s",
    mar = c(3, 0, 3, 0)
  )

  cols <- q2

  stripchart(x$value ~ x$PL + x$Age,
    vertical = TRUE,
    method = "jitter",
    jitter = 0.2,
    pch = 16,
    cex = 1,
    xlab = "",
    ylab = "",
    at = c(1, 2, 3, 4, 5, 6, 7, 8),
    col = cols,
    xaxt = "n",
    yaxt = "n"
  )
  axis(2,
    cex.axis = 1.5
  )
  axis(1,
    at = c(1.5, 3.5, 5.5, 7.5),
    labels = c("13", "14", "15", "16"),
    cex.axis = 1.5
  )
  mtext(paste0(unique(x$variable)),
    side = 3,
    adj = 0.5,
    line = 1,
    cex = 1
  )
}
