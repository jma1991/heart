theme_custom <- function(base_size = 12, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
  theme(
    aspect.ratio = 1,
    axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
    axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"), angle = 90)
  )
}
