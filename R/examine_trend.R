#' @name examine_trend
#' @title Examine catch data from annual VEFMAP surveys
#' @description Series of functions to examine catch data from VEFMAP surveys
#'   and define a class based on these outputs
NULL

#' @rdname examine_trend
#'
#' @export
#'
#' @importFrom dplyr left_join summarise ungroup group_by mutate n `%>%`
#' @importFrom tidyr complete nesting
#'
#' @param x df
#' @param system d
#' @param species df
#' @param reach df
#' @param start dkjf
#' @param end dfd
#' @param \dots dk
#' @param y ignored
#' @param col djf
#'
#' @details
#'
#' @examples
#'
#'
#'
examine_trend <- function(x, system = NULL, species = NULL, reach = NULL, start = NULL, end = NULL, ...) {

  # focus on data subset
  x <- x %>% mutate(species_name = ifelse(species_name %in% species, species_name, "null"))
  x <- filter_survey(x, system = system, species = species, reach = reach, start = start, end = end)

  # calculate mean CPUE by species and reach, including standard error
  x <- x %>%
    mutate(catch = ifelse(length_mm > 0, 1, 0)) %>%
    group_by(date, water_year, system, reach, site, id_survey, species_name) %>%
    summarise(catch = sum(catch)) %>%
    left_join(info$effort, by = c("date", "system", "reach", "site", "id_survey")) %>%
    ungroup() %>%
    complete(nesting(date, water_year, system, reach, site, id_survey, effort), species_name, fill = list(catch = 0)) %>%
    filter(species_name != "null") %>%
    mutate(cpue = 1000 * catch / effort) %>%
    group_by(water_year, system, reach, species_name) %>%
    summarise(
      cpue_se = sd(cpue) / n(),
      cpue = mean(cpue, na.rm = TRUE)
    ) %>%
    ungroup %>%
    mutate(cpue_se = ifelse(is.na(cpue_se), 0, cpue_se))

  # return
  as_trend_summary(x)

}

# default plotting function for trend_summary data
#' @export
#'
#' @importFrom dplyr filter
plot.trend_summary <- function(
  x, y, ..., system, species = NULL, reach = NULL, col = NULL
) {

  # set default colour palette
  if (is.null(col))
    col <- blues_hex[1]

  # set default reach (all available reaches)
  if (is.null(reach))
    reach <- unique(x$reach)

  # set default species (all available species)
  if (is.null(species))
    species <- unique(x$species_name)

  # check that one and only one system, species, and reach are provided
  if (missing(species) | missing(system) | missing(reach))
    stop("species, system, and reach must be specified when plotting a trend_summary", call. = FALSE)
  check_single(system)

  # pull out target system and reach, accounting for possible NA reaches
  sys_id <- system
  rch_id <- reach
  if (any(is.na(reach))) {
    x <- x %>% filter(species_name %in% species, system == sys_id, reach %in% rch_id | is.na(reach))
  } else {
    x <- x %>% filter(species_name %in% species, system == sys_id, reach %in% rch_id)
  }

  # set up layout for plot of all species and reaches
  spp <- unique(x$species_name)
  nspp <- length(spp)
  reaches <- unique(x$reach)
  nreach <- length(reaches)
  laymat <- matrix(c(seq_len(nspp * nreach)), nrow = nspp)
  layout(laymat)

  # set plot margins
  old_mar <- par()$mar
  par(mar = c(4.1, 5.7, 3.1, 0.2))

  # plot trends for each species and reach
  for (i in seq_len(nreach)) {
    for (j in seq_len(nspp)) {
      main <- j == 1
      sub <- i == 1
      bottom <- j == nspp
      plot_trend(x, species = spp[j], reach = reaches[i], col = col, main = main, sub = sub, bottom = bottom, ...)
    }
  }

  # reset layout
  layout(matrix(1))

  # reset plotting margins
  par(mar = old_mar)

  # return
  out <- NULL

}

# internal function: plot trend for a single species and reach
plot_trend <- function(x, species, reach, col, main, sub, bottom, ...) {

  # subset data
  rch_id <- reach
  if (is.na(rch_id)) {
    x <- x %>% filter(is.na(reach), species_name == species)
  } else {
    x <- x %>% filter(reach == rch_id, species_name == species)
  }

  # plot main barplot
  upper <- x$cpue + x$cpue_se
  lower <- x$cpue - x$cpue_se
  xax <- barplot(
    x$cpue ~ x$water_year,
    col = col,
    ylim = range(c(0, x$cpue, upper, lower)),
    las = 1,
    xlab = "",
    ylab = ""
  )

  # add SE bars
  for (i in seq_len(nrow(x))) {
    if (lower[i] != upper[i])
      arrows(
        x0 = xax[i],
        x1 = xax[i],
        y0 = lower[i],
        y1 = upper[i],
        angle = 90,
        code = 3,
        col = "black",
        length = 0.04
      )
  }

  # add a system label
  if (main) {
    if (sub) {
    sys_lab <- unique(x$system)
    sys_lab <- paste0(toupper(substr(sys_lab, 1, 1)), substr(sys_lab, 2, nchar(sys_lab)))
    sys_lab <- gsub("_", " ", sys_lab)
    sys_lab <- gsub("ck", "Creek", sys_lab)
    if (!is.na(reach))
      sys_lab <- paste0(sys_lab, " River: Reach ", reach)
    } else {
      sys_lab <- paste0("Reach ", reach)
    }
    mtext(sys_lab, side = 3, adj = 0, line = 1.5, cex = 1.1)
  }

  # add a sub-label for species
  if (sub) {
    spp_lab <- get_common_names(unique(x$species_name))
    spp_lab <- paste0(toupper(substr(spp_lab, 1, 1)), substr(spp_lab, 2, nchar(spp_lab)))
    mtext(spp_lab, side = 2, adj = 0.5, line = 4.1, cex = 1)
  }

  # add axis labels
  if (sub)
    mtext("CPUE", side = 2, adj = 0.5, line = 2.5, cex = 1)
  if (bottom)
    mtext("Water year", side = 1, adj = 0.5, line = 2.6, cex = 1)

}

# internal function: set class for examine_trends outputs
as_trend_summary <- function(x) {
  as_class(x, name = "trend_summary", type = "tbl_df")
}
