plotRLE <- function(x) {

  mat <- logcounts(x)

  med <- apply(mat, 1, median)

  rle <- mat - med

  qsd <- t(apply(rle, 2, quantile, prob = c(0.05, 0.25, 0.5, 0.75, 0.95)))

  num <- ncol(x)

  rownames(qsd) <- seq_len(num)

  dat <- melt(qsd, value.name = "expr", varnames = c("cell", "prob"))

  ggplot(dat, aes(cell, expr)) +
    geom_line(aes(group = prob, colour = prob)) +
    scale_colour_brewer(name = "Quantile", palette = "Dark2", direction = -1) +
    guides(colour = guide_legend(reverse = TRUE)) +
    labs(x = "Index", y = "RLE") +
    theme_minimal()

}
