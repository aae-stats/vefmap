#' @name examine_species
#' @title Return species table from VEFMAP data
#' @description Series of functions to analyse length-frequency data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname examine_species
#'
#' @importFrom dplyr distinct group_by summarise ungroup
#'
#' @export
#'
#' @param x df
#'
#' @details
#'
#' @examples
#'
#'
#'
examine_species <- function(x) {

  # add system-by-reach id
  x$sys_reach <- paste(x$system, x$reach, sep = ": reach ")

  # create species catch table
  y <- x %>%
    filter(species_name != "null") %>%
    group_by(sys_reach, water_year, species_name) %>%
    summarise(
      catch = n()
    ) %>%
    ungroup %>%
    pivot_wider(
      id_cols = c("species_name", "water_year"),
      names_from = "sys_reach",
      values_from = "catch"
    ) %>%
    mutate(across(.fns = ~ifelse(is.na(.), 0, .)))

  # tidy and return
  as_species_summary(y)

}

# internal function: set class for examine_species outputs
as_species_summary <- function(x) {
  as_class(x, name = "species_summary", type = "tbl_df")
}
