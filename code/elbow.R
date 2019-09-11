elbowFinder <- function(var.exp) {

  var.exp <- sort(var.exp, decreasing=TRUE)

  dy <- -diff(range(var.exp))
  dx <- length(var.exp) - 1
  l2 <- sqrt(dx^2 + dy^2)
  dx <- dx/l2
  dy <- dy/l2

  dy0 <- var.exp - var.exp[1]
  dx0 <- seq_along(var.exp) - 1

  parallel.l2 <- sqrt((dx0 * dx)^2 + (dy0 * dy)^2)
  normal.x <- dx0 - dx * parallel.l2
  normal.y <- dy0 - dy * parallel.l2
  normal.l2 <- sqrt(normal.x^2 + normal.y^2)

  which.max(normal.l2)
}
