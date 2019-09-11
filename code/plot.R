#' plotRLE
#'
#' @param A SingleCellExperiment object containing expression values, usually log-transformed counts.
#' @return A ggplot object

plotRLE <- function(x) {

  mat <- logcounts(x)

  med <- rowMedians(mat)

  rle <- mat - med

  qsd <- t(apply(rle, 2, quantile, prob = c(0.05, 0.25, 0.5, 0.75, 0.95)))

  num <- ncol(mat)

  rownames(qsd) <- seq_len(num)

  dat <- reshape2::melt(qsd, value.name = "expr", varnames = c("cell", "prob"))

  ggplot(dat, aes(cell, expr)) +
    geom_line(aes(group = prob, colour = prob)) +
    guides(colour = guide_legend(reverse = TRUE)) +
    labs(x = "Index", y = "RLE", colour = NULL) +
    theme_bw()

}
