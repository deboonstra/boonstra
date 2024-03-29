% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legends.R
\name{legends}
\alias{legends}
\alias{legend_top}
\alias{legend_bottom}
\alias{legend_right}
\title{Automatic placement of legends for plots}
\usage{
legend_top(horiz = TRUE, ...)

legend_bottom(horiz = TRUE, ...)

legend_right(...)
}
\arguments{
\item{horiz}{logical; if TRUE, set the legend horizontally rather than
vertically (specifying horiz overrides the ncol specification)}

\item{...}{Further arguments to \code{legend()}}
}
\description{
Produce a legend either above or to the right of the plot window.
}
\examples{
g <- stats::rbinom(10, 1, 0.5)
x <- 1:10
y <- 3 * x + 5
plot(x, y, col = boonstra::pal(2)[g + 1], pch = 19, bty = "n")
legend_top(legend = c("A", "B"), col = boonstra::pal(2), pch = 19)


}
