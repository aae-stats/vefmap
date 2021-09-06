#' @name analyse_trends
#' @title Analyse catch data from annual VEFMAP surveys
#' @description Series of functions to analyse catch data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname analyse_trends
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
analyse_trends <- function(x, species, years, col, ...) {

  # prepare data (filtering etc.)

  # generate plots

  NULL

}


# internal function: set class for analyse_trends outputs
as_trend_analysis <- function(x) {
  as_class(x, name = "trend_analysis", type = "list")
}
