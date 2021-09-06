#' @name examine_length
#' @title Examine length-frequency data from annual VEFMAP surveys
#' @description Series of functions to examine length-frequency data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

# TODO: add informative summaries for each function to justify classes (e.g. table of min/max/mean/median/SD of lengths)

#' @rdname examine_length
#'
#' @export
#'
#' @importFrom dplyr filter `%>%`
#'
#' @param x
#' @param species dfd
#' @param system dfd
#' @param reach dfd
#' @param start dfd
#' @param end dfd
#' @param \dots dfd
#' @param y ignored
#' @param col
#'
#' @details
#'
#' @examples
#'
#'
#'
examine_length <- function(x, species = NULL, system = NULL, reach = NULL, start = NULL, end = NULL, ...) {

  # focus on data subset
  x <- filter_survey(x, system = system, species = species, reach = reach, start = start, end = end)

  # return
  as_length_summary(x)

}

# default plotting function for length_summary data
#' @export
#'
#' @importFrom dplyr filter
plot.length_summary <- function(
  x, y, ..., species, system, reach = NULL, col = NULL, freq = FALSE
) {

  # set default colour palette
  if (is.null(col))
    col <- blues_hex

  # set default reach (all reaches combined)
  if (is.null(reach))
    reach <- unique(x$reach)

  # check that one and only one system and species are provided (multiple reaches can be used)
  if (missing(species) | missing(system))
    stop("species, system, and reach must be specified when plotting a length_summary", call. = FALSE)
  check_single(species, system)

  # pull out target reach
  sys_id <- system
  rch_id <- reach
  x <- x %>% filter(species_name == species, system == sys_id, reach %in% rch_id)

  # check years have complete data, remove if not
  length_ok <- tapply(x$length_mm, x$water_year, function(x) any(!is.na(x)))
  if (any(!length_ok))
    x <- x %>% filter(water_year %in% names(length_ok)[length_ok])

  # how many years?
  start <- min(x$water_year)
  end <- max(x$water_year)
  unique_years <- sort(unique(x$water_year))
  nyear <- length(unique_years)

  # define shared breaks for all plots
  breaks <- seq(min(x$length_mm, na.rm = TRUE), max(x$length_mm, na.rm = TRUE), length = 35)

  # define a 2 x 1 plot layout, tweak heights to keep plotting regions equal
  layout(mat = matrix(seq_len(nyear), ncol = 1), heights = c(rep(1, nyear - 1), 1.45))

  # no x-axis on upper plots
  old_mar <- par()$mar
  par(mar = c(1.1, 5.2, 2.5, 1.1))

  # plot all years except final one
  for (i in seq_len(nyear - 1L)) {

    xsub <- x %>% filter(water_year == unique_years[i])
    plot_length_freq(
      xsub,
      col = col[i],
      xaxis = FALSE,
      breaks = breaks,
      freq = freq
    )

    # add a label with year and count
    n_obs <- sum(!is.na(x$length_mm[x$water_year == unique_years[i]]))
    mtext(
      paste0(unique_years[i], " (n = ", n_obs, ")"),
      side = 3,
      adj = 0.9,
      line = 1,
      cex = 1.05)

  }

  #  add room for x-axis on lower plot
  par(mar = c(5.3, 5.2, 2.5, 1.1))

  # plot final year
  xsub <- x %>% filter(water_year == unique_years[nyear])
  plot_length_freq(
    xsub,
    col = col[end],
    xaxis = TRUE,
    breaks = breaks,
    freq = freq
  )

  # add a label with year and count
  n_obs <- sum(!is.na(x$length_mm[x$water_year == unique_years[n_year]]))
  mtext(
    paste0(unique_years[nyear], " (n = ", n_obs, ")"),
    side = 3,
    adj = 0.9,
    line = 1,
    cex = 1.05)

  # reset plotting margins
  layout(matrix(1))
  par(mar = old_mar)

  # return
  out <- NULL

}

# internal function: plot histogram of lengths for a single year
plot_length_freq <- function(x, col, xaxis, breaks, freq) {

  # generate histogram of length data
  hist(
    x$length_mm,
    freq = freq,
    breaks = breaks,
    col = col,
    xaxt = "n",
    las = 1,
    main = "",
    xlab = "",
    ylab = ""
  )

  # add a neat date axis
  if (xaxis) {
    axis(1, las = 1)
    mtext("Length (mm)", side = 1, adj = 0.5, line = 2.1, cex = 1.1)
  } else {
    axis(1, tick = FALSE, labels = FALSE)
  }

  # add y-axis label
  mtext("Frequency", side = 2, adj = 0.5, line = 3.5, cex = 1.1)

  # return
  out <- NULL

}

# internal function: set class for examine_length outputs
as_length_summary <- function(x) {
  as_class(x, name = "length_summary", type = "tbl_df")
}
