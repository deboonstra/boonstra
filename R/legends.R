#' Automatic placement of legends for plots
#'
#' Produce a legend either above or to the right of the plot window.
#' @name legends
#'
#' @examples
#' g <- stats::rbinom(10, 1, 0.5)
#' x <- 1:10
#' y <- 3 * x + 5
#' plot(x, y, col = boonstra::pal(2)[g + 1], pch = 19, bty = "n")
#' legend_top(legend = c("A", "B"), col = boonstra::pal(2), pch = 19)
#'
#'
#' @rdname legends
#' @export legend_top
#'
#' @param horiz logical; if TRUE, set the legend horizontally rather than
#' vertically (specifying horiz overrides the ncol specification)
#' @param ... Further arguments to `legend()`
legend_top <- function(horiz = TRUE, ...) {
  if (graphics::par("oma")[3] == 0) {
    x <- mean(graphics::par("usr")[1:2])
    yy <- transform_coord(graphics::par("usr")[3:4], graphics::par("plt")[3:4])
    y  <- mean(c(yy[2], graphics::par("usr")[4]))
    graphics::legend(
      x, y, xpd = NA, bty = "n", xjust = 0.5, yjust = 0.5, horiz = horiz, ...
    )
  } else {
    g <- graphics::par("mfrow")
    xx <- transform_coord(graphics::par("usr")[1:2], graphics::par("plt")[1:2])
    yy <- transform_coord(graphics::par("usr")[3:4], graphics::par("plt")[3:4])
    xxx <- transform_coord(xx, c(g[2] - 1, g[2]) / g[2])
    yyy <- transform_coord(yy, c(g[1] - 1, g[1]) / g[1])
    yyyy <- transform_coord(yyy, graphics::par("omd")[3:4])
    graphics::legend(
      mean(xxx), mean(c(yyy[2], yyyy[2])), xpd = NA, bty = "n",
      xjust = 0.5, yjust = 0.5, horiz = horiz, ...
    )
  }
}

#' @rdname legends
#' @export legend_bottom
legend_bottom <- function(horiz = TRUE, ...) {
  if (graphics::par("oma")[1] == 0) {
    x <- mean(graphics::par("usr")[1:2])
    yy <- transform_coord(
      x = graphics::par("usr")[4:3] - 1,
      p = graphics::par("plt")[4:3]
    )
    y  <- mean(c(yy[1] - 1, graphics::par("usr")[3]))
    graphics::legend(
      x, y, xpd = NA, bty = "n", xjust = 0.5, yjust = 1, horiz = horiz, ...
    )
  } else {
    g <- graphics::par("mfrow")
    xx <- transform_coord(graphics::par("usr")[1:2], graphics::par("plt")[1:2])
    yy <- transform_coord(
      x = graphics::par("usr")[4:3] - 1,
      p = graphics::par("plt")[4:3]
    )
    xxx <- transform_coord(xx, c(g[2] - 1, g[2]) / g[2])
    yyy <- transform_coord(yy, c(g[1] - 1, g[1]) / g[1])
    yyyy <- transform_coord(yyy, graphics::par("omd")[4:3])
    graphics::legend(
      mean(xxx), mean(c(yyy[2] - 1, yyyy[2])), xpd = NA, bty = "n",
      xjust = 0.5, yjust = 1, horiz = horiz, ...
    )
  }
}

#' @rdname legends
#' @export legend_right
legend_right <- function(...) {
  if (graphics::par("oma")[4] == 0) {
    y <- mean(graphics::par("usr")[3:4])
    xx <- transform_coord(graphics::par("usr")[1:2], graphics::par("plt")[1:2])
    x <- mean(c(xx[2], graphics::par("usr")[2]))
    graphics::legend(x, y, xpd = NA, bty = "n", xjust = 0.5, yjust = 0.5, ...)
  } else {
    g <- graphics::par("mfrow")
    xx <- transform_coord(graphics::par("usr")[1:2], graphics::par("plt")[1:2])
    yy <- transform_coord(graphics::par("usr")[3:4], graphics::par("plt")[3:4])
    xxx <- transform_coord(xx, c(g[2] - 1, g[2]) / g[2])
    yyy <- transform_coord(yy, c(g[1] - 1, g[1]) / g[1])
    xxxx <- transform_coord(xxx, graphics::par("omd")[1:2])
    graphics::legend(
      mean(c(xxx[2], xxxx[2])), mean(yyy), xpd = NA, bty = "n",
      xjust = 0.5, yjust = 0.5, ...
    )
  }
}

# Helper function ####
transform_coord <- function(x, p) {
  ba <- (x[2] - x[1]) / (p[2] - p[1])
  a <- x[1] - p[1] * ba
  b <- a + ba
  c(a, b)
}