#' @name analyse_length
#' @title Analyse length-frequency data from annual VEFMAP surveys
#' @description Series of functions to analyse length-frequency data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname analyse_length
#'
#' @export
#'
#' @param x
#'
#' @details
#'
#' @examples
#'
#'
#'
analyse_length <- function(x, species, years, col, ...) {

  # prepare data (filtering etc.)

  # generate plots

  NULL

}


# internal function: set class for analyse_age outputs
as_length_analysis <- function(x) {
  as_class(x, name = "length_analysis", type = "list")
}
