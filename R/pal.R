#' Generate a nice palette of colors
#'
#' @param n Number of colors to generate, evenly spaced on hcl scale
#' @param alpha  Partial transparency
#'
#' @examples
#' pal(3)
#'
#' @export pal
pal <- function(n, alpha = 1) {
  if (n == 2) {
    sq <- seq(15, 375, len = 4)
    val <- grDevices::hcl(sq, l = 60, c = 150, alpha = alpha)[c(1, 3)]
  } else {
    sq <- seq(15, 375, len = n + 1)
    val <- grDevices::hcl(sq, l = 60, c = 150, alpha = alpha)[1:n]
  }
  return(val)
}