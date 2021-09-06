#' @name examine_hydro
#' @title Examine hydrological data associated with annual VEFMAP surveys
#' @description Series of functions to examine hydrological data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname examine_hydro
#'
#' @export
#'
#' @importFrom aae.hydro fetch_hydro
#' @importFrom lubridate parse_date_time
#' @importFrom dplyr filter select mutate `%>%`
#' @importFrom tidyr pivot_wider
#' @importFrom tibble tibble
#'
#' @param x djf
#' @param start df
#' @param end df
#' @param y ignored, for consistency with S3 plot method
#' @param \dots kjfd
#' @param gauge kdjfd
#' @param scale_factor kd
#' @param col dkjf
#'
#' @details
#'
#' @examples
#'
#'
#'
examine_hydro <- function(x, start = NULL, end = NULL) {

  # link reaches in data to flow gauges
  gauges <- fetch_gauges(x$system, x$reach)
  gauges <- unique(gauges)
  gauges <- gauges[!is.na(gauges)]

  # work out dates
  if (is.null(start))
    start <- min(x$water_year)
  start_date <- paste0(start, "-01-01")
  if (is.null(end))
    end <- max(x$water_year)
  end_date <- paste0(end, "-12-31")

  # fetch hydrological data
  hydro <- aae.hydro::fetch_hydro(
    sites = gauges,
    start = start_date,
    end = end_date,
    options = list(
      varfrom = c("100.00", "100.00", "141.00", "450.00"),
      varto = c("100.00", "141.00", "141.00", "450.00")
    ),
    include_missing = TRUE
  )

  # convert to tibble for convenience in plotting
  hydro <- tibble(hydro)

  # clean it up, splitting each variable into its own column
  hydro <- hydro %>%
    filter(!is.na(value)) %>%
    select(date_formatted, value, variable_name, site_name, site_code) %>%
    pivot_wider(
      id_cols = c("date_formatted", "site_name", "site_code"),
      names_from = "variable_name",
      values_from = "value"
    ) %>%
    mutate(
      water_temperature_c = ifelse(water_temperature_c > 50, NA, water_temperature_c)
    )

  # return
  as_hydrology(hydro)

}

# default plotting function for hydrological data
#' @export
#'
#' @importFrom dplyr filter
plot.hydrology <- function(
  x, y, ..., gauge, start = NULL, end = NULL, scale_factor = 0.9, col = NULL
) {

  # set default colour palette
  if (is.null(col))
    col <- blues_hex

  # set default year range
  if (is.null(start))
    start <- min(year(x$date_formatted))
  if (is.null(end))
    end <- max(year(x$date_formatted))

  # filter to a given gauge and years
  x <- x %>% filter(site_code == gauge, year(date_formatted) %in% c(start:end))

  # define a 2 x 1 plot layout, tweak heights to keep plotting regions equal
  layout(mat = matrix(1:2, nrow = 2), heights = c(1, 1.45))

  # no x-axis on top plot
  old_mar <- par()$mar
  par(mar = c(0, 5.2, 1.1, 5.2))

  # plot gauge height
  plot_hydro(
    x,
    var = "stream_water_level_m",
    scale_factor = scale_factor,
    col = col,
    ylab = "Gauge height (m)",
    xaxis = FALSE
  )

  #  add room for x-axis on lower plot
  par(mar = c(5.3, 5.2, 1.1, 5.2))

  # plot discharge
  plot_hydro(
    x,
    var = "stream_discharge_mld",
    scale_factor = scale_factor,
    col = col,
    ylab = "Stream discharge (ML/day)",
    xaxis = TRUE
  )

  # reset plotting margins
  layout(matrix(1))
  par(mar = old_mar)

  # return
  out <- NULL

}

# internal function: plotting a single response variable
plot_hydro <- function(
  x,
  var,
  scale_factor,
  col,
  ylab,
  xaxis
) {

  # pull out main y varaible
  y <- x[[var]]
  y2 <- x$water_temperature_c

  # initialise plot
  plot(
    y ~ x$date_formatted,
    type = "n",
    bty = "u",
    las = 1,
    xlab = "",
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    ylim = c(0, max(y, na.rm = TRUE))
  )

  # add shading for the spawning months
  years <- unique(year(x$date_formatted))
  for (i in seq_along(years)) {
    start <- lubridate::parse_date_time(paste0(years[i], "-10-01"), orders = c("ymd"))
    end <- lubridate::parse_date_time(paste0(years[i], "-12-31"), orders = c("ymd"))
    polygon(
      x = c(start, start, end, end),
      y = c(0, 1.5 * max(y, na.rm = TRUE), 1.5 * max(y, na.rm = TRUE), 0),
      col = "gray90",
      border = NA
    )
  }

  # add a neat date axis
  if (xaxis) {
    x_reduced <- x %>% filter(month(date_formatted) %in% c(1, 3, 5, 7, 9, 11))
    axis(
      1,
      at = x_reduced$date_formatted,
      format(x_reduced$date_formatted, "%b %y"),
      las = 2,
      tick = FALSE
    )
  } else {
    axis(1, tick = FALSE, labels = FALSE)
  }

  # scale temperature relative to main y axis
  rescale <- scale_factor * max(y, na.rm = TRUE) / max(y2, na.rm = TRUE)

  # add lines for both variables
  lines(
    y ~ x$date_formatted,
    col = col[2],
    lwd = 2
  )
  lines(
    y2 * rescale ~ x$date_formatted,
    col = col[1],
    lwd = 2
  )

  if (var == "stream_discharge_mld") {
    yseq <- seq(0, round(max(y, na.rm = TRUE), -4), by = 10000)
  } else {
    yseq <- seq(0, round(max(y, na.rm = TRUE)), length = 6)
  }
  axis(2, at = yseq, labels = yseq, las = 2, tick = TRUE)
  axis(4, at = yseq, labels = round(yseq / rescale), las = 2, tick = TRUE)

  lab <- c(ifelse(xaxis, "Date", ""), ylab, "", expression("Water temperature ("*degree*C*")"))
  line <- c(3.9, 3.6, 4.5, 3.3)
  for (i in seq_along(lab))
    mtext(lab[i], side = i, adj = 0.5, line = line[i], cex = 1.15)

  # return
  out <- NULL

}

# internal function: specify discharge gauges for each system and reach
fetch_gauges <- function(system, reach) {

  gauge_list <- c(

    # broken river
    "broken_r2" = 404224,
    "broken_r3" = 404224,
    "broken_r4" = 404224,
    "broken_r5" = 404210,

    # campaspe river
    "campaspe_r2" = 406201,
    "campaspe_r3" = 406202,
    "campaspe_r4" = 406202,

    # goulburn river
    "goulburn_r4" = 405232,
    "goulburn_r5" = 405232,

    # loddon
    "loddon_r2" = 407202,
    "loddon_r3" = 407202,
    "loddon_r4" = 407202,
    "loddon_r5" = 407202,
    "pyramid_ck_rNA" = 407294,

    # wimmera
    "wimmera_rNA" = NA,
    "wimmera_r3" = NA,
    "mackenzie_rNA" = NA,
    "burnt_ck_rNA" = NA,

    # others
    "litte_murray_rNA" = NA,
    "mt_william_ck_rNA" = NA,
    "moorabool_rNA" = NA,

    # barwon river
    "barwon_r1" = NA,

    # cardinia creek
    "cardinia_ck_r1" = NA,

    # glenelg river
    "glenelg_r1" = NA,
    "glenelg_r2" = NA,
    "glenelg_r3" = NA,

    # tarwin river
    "tarwin_r1" = NA,

    # taylors creek
    "taylors_ck_r1" = NA,

    # thomson river
    "thomson_r2" = NA,
    "thomson_r3" = NA,
    "thomson_r4" = NA,
    "thomson_r5" = NA,

    # werribee river
    "werribee_r1" = NA,

    # yarra river
    "yarra_r2" = NA,
    "yarra_r3" = NA,
    "yarra_r4" = NA,
    "yarra_r5" = NA,
    "yarra_r6" = NA

  )

  gauge_list[paste(system, reach, sep = "_r")]

}

# internal function: set class for examine_hydro outputs
as_hydrology <- function(x) {
  as_class(x, name = "hydrology", type = "tbl_df")
}
