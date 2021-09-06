#' @name analyse_recruits
#' @title Analyse recruitment data from annual VEFMAP surveys
#' @description Series of functions to analyse recruitment data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname analyse_recruits
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
analyse_recruits <- function(x, species, years, col, ...) {

  # prepare data (filtering etc.)

  # generate plots

  NULL

}


# internal function: set class for analyse_recruits outputs
as_recruit_analysis <- function(x) {
  as_class(x, name = "recruit_analysis", type = "list")
}
