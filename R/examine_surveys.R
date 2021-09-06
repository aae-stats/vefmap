#' @name examine_surveys
#' @title Return survey table from VEFMAP data
#' @description Series of functions to analyse length-frequency data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname examine_surveys
#'
#' @importFrom dplyr distinct group_by summarise ungroup
#'
#' @export
#'
#' @param x kdj
#' @param \dots kdjf
#'
#' @details
#'
#' @examples
#'
#'
#'
examine_surveys <- function(x) {

  # add system-by-reach id
  x$sys_reach <- paste(x$system, x$reach, sep = ": reach ")

  # create survey tables
  y <- table(x$system, x$water_year)
  w <- table(x$sys_reach, x$water_year)
  w2 <- table(x$sys_reach, x$water_year, x$site)
  w2 <- ifelse(w2 > 0, 1, 0)
  w2 <- apply(w2, c(1, 2), sum)
  z <- table(x$site, x$water_year)

  # convert to binary values
  y <- ifelse(y > 0, 1, 0)
  w <- ifelse(w > 0, 1, 0)
  z <- ifelse(z > 0, 1, 0)

  # grab some extra info on the systems
  details <- data.frame(
    nreach = apply(table(x$system, x$reach), 1, function(x) sum(x > 0)),
    nsite = apply(table(x$system, x$site), 1, function(x) sum(x > 0)),
    nyear = apply(y, 1, sum),
    row.names = rownames(y)
  )

  # calculate survey effort
  effort <- x %>%
    distinct(date, system, site, reach, id_site, id_survey, id_surveyevent, .keep_all = TRUE) %>%
    group_by(date, system, reach, site, id_survey) %>%
    summarise(effort = sum(seconds)) %>%
    ungroup

  # tidy and return
  as_survey_summary(
    list(
      system = y,
      reach = w,
      site = z,
      nsite = w2,
      effort = effort,
      details = details
    )
  )

}

# print method for survey table
#' @export
print.survey_summary <- function(x, ...) {

  cat("Number of reaches, sites, and years surveyed per river system\n")
  print(x$details)

  cat("\nSampling events by reach")
  print(x$reach)

}

# internal function: set class for examine_surveys outputs
as_survey_summary <- function(x) {
  as_class(x, name = "survey_summary", type = "list")
}
