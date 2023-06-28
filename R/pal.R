#' Generate a nice palette of colors
#'
#' Generate a character vector of colors by specifying the specifying hue,
#' chroma and luminance.
#'
#' @param n Number of colors to generate, evenly spaced on `grDevice::hcl` scale
#' @param alpha  Partial transparency inherited from `grDevice::hcl`
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