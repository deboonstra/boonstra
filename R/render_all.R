#' Automatic rendering of Rmarkdown files
#'
#' Silent rendering of a collection of Rmarkdown files with the current working
#' directory as the knitting directory.
#'
#' @param files A vector of character values listing the .Rmd, .R, or .md files
#' to render.
#' @param knit_root_dir A character string specifying the knitting directory.
#' Default is the current working directory with [getwd()].
#' @param quiet An logical option to suprress printing during rendering from
#' knitr, pandoc command line and others. Default is TRUE.
#' @param ... Any other [rmarkdown::render()] parameters.
#'
#' @details If `files` is not specified, all .Rmd files in the current directory
#' are located and rendered.
#'
#' @seealso [rmarkdown::render()]
#'
#' @export render_all
render_all <- function(files, knit_root_dir = getwd(), quiet = TRUE, ...) {
  # check parameter values ####
  if (!missing(files) && class(files) != "character") {
    stop("files is not a character vector of .Rmd, .R, or .md files.")
  }

  if (missing(files)) {
    files <- list.files(
      path = ".", pattern = "*.Rmd", full.names = TRUE,
      recursive = TRUE, include.dirs = TRUE, ignore.case = TRUE
    )
  }

  if (class(knit_root_dir) != "character") {
    stop("knit_root_dir is not a character vector of directories.")
  }

  if ((length(knit_root_dir) > 1) && (length(knit_root_dir) != length(files))) {
    stop("Length of knit_root_dir does not match the length of files.")
  }

  # rendering files ####
  if (length(knit_root_dir) == 1) {
    for (j in seq_along(files)) {
      cat(
        "Rendering:", files[j],
        "with a working directory of", knit_root_dir,
        "\n"
      )
      rmarkdown::render(
        input = files[j], knit_root_dir = knit_root_dir,
        quiet = quiet, ...
      )
    }
  } else {
    for (j in seq_along(files)) {
      cat(
        "Rendering:", files[j],
        "with a working directory of", knit_root_dir[j],
        "\n"
      )
      rmarkdown::render(
        input = files[j], knit_root_dir = knit_root_dir[j],
        quiet = quiet, ...
      )
    }
  }
}