#' @name examine_distribution
#' @title Examine distributions of species at sites in annual VEFMAP surveys
#' @description Series of functions to examine distributions of species at sites
#'   from VEFMAP surveys and define a class based on these outputs
NULL

#' @rdname examine_distribution
#'
#' @export
#'
#' @importFrom dplyr group_by summarise filter `%>%`
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble as_tibble
#' @importFrom tidyselect contains
#'
#' @param x ddf
#' @param survey dfd
#' @param system df
#' @param species dj
#' @param reach dkfj
#' @param start djf
#' @param end kjdf
#' @param \dots kdf
#' @param y ignored
#' @param col df
#'
#' @details
#'
#' @examples
#'
#'
#'
examine_distribution <- function(x, survey, system, species = NULL, reach = NULL, start = NULL, end = NULL, ...) {

  # survey table is required
  if (missing(survey))
    stop("survey table must be provided to calculate reporting rates in examine_distribution", call. = FALSE)

  # focus on data subset
  x <- filter_survey(x, system = system, species = species, reach = reach, start = start, end = end)

  # remove "null" species
  x <- x %>% filter(species_name != "null")

  # add a sys_reach identifier to simplify calcs below
  x$sys_reach <- paste(x$system, x$reach, sep = "_r")

  # create table of site records by reach
  x <- table(x$sys_reach, x$water_year, x$species_name, x$site)
  x <- apply(x, c(1, 2, 3), function(.x) sum(.x > 0))

  # extract survey and reach info from survey table
  survey_info <- parse_survey(survey)

  # subset to target systems
  idx <- survey_info$system %in% system
  survey <- survey[idx, ]
  survey_info <- survey_info[idx, ]

  # pull out relevant reaches from survey table
  idx <- match(rownames(x), survey_info$sys_reach)
  survey <- survey[idx, ]

  # pull out relevant years from survey table
  idx <- match(colnames(x), colnames(survey))
  survey <- survey[, idx]

  # calculate reporting rate
  x <- sweep(x, c(1, 2), survey, "/")
  x[survey == 0] <- 0

  # wrap up in a neat, long-form data table
  x <- data.frame(
    sys_reach = rep(rownames(x), times = dim(x)[3]),
    species = rep(dimnames(x)[[3]], each = nrow(x)),
    rr = do.call(rbind, lapply(seq_len(dim(x)[3]), function(i, .x) .x[, , i], .x = x))
  )
  x <- as_tibble(x)
  x <- x %>% pivot_longer(
    cols = contains("rr"),
    names_to = "water_year",
    values_to = "reporting_rate",
    names_prefix = "rr\\."
  )

  # fix up system and reach columns
  x <- x %>% mutate(
    system = sapply(strsplit(sys_reach, split = "_r"), function(x) x[1]),
    reach = sapply(strsplit(sys_reach, split = "_r"), function(x) x[2])
  )

  # return
  as_distribution_summary(x)

}

# default plotting function for distribution_summary data
#' @export
#'
#' @importFrom dplyr filter
plot.distribution_summary <- function(
  x, y, ..., system, reach = NULL, col = NULL
) {

  # set default colour palette
  if (is.null(col))
    col <- blues_hex

  # set default reach (all available reaches)
  if (is.null(reach))
    reach <- unique(x$reach)

  # check that one and only one system and reach are provided
  if (missing(system) | missing(reach))
    stop("system and reach must be specified when plotting a distribution_summary", call. = FALSE)
  check_single(system)

  # reformat data for basic barplot
  sys_id <- system
  rch_id <- reach
  x <- x %>%
    filter(system == sys_id, reach %in% rch_id) %>%
    pivot_wider(
      id_cols = c("system", "reach", "species"),
      names_from = "water_year",
      values_from = "reporting_rate"
    )

  # add a layout that fits each reach and a legend
  reaches <- unique(x$reach)
  nreach <- length(reaches)
  laymat <- matrix(c(seq_len(nreach), rep(nreach + 1, nreach)), ncol = 2)
  layout(laymat, widths = c(1, 0.25))

  # set plot margins
  old_mar <- par()$mar
  par(mar = c(4.5, 4.1, 2.1, 0.5))

  # plot reporting rates for each reach
  for (i in seq_len(nreach))
    plot_distribution(x, reach = reaches[i], col = col, ...)

  # set plot margins for legend panel and add legend
  par(mar = rep(0, 4))
  plot(c(0, 1) ~ 1, bty = "n", type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  legend(x = "center", fill = col, legend = get_common_names(unique(x$species)), cex = 0.8, bty = "n")

  # reset layout
  layout(matrix(1))

  # reset plotting margins
  par(mar = old_mar)

  # return
  out <- NULL

}

# internal function: plot reporting rates for a single reach
#' @importFrom dplyr filter `%>%`
plot_distribution <- function(x, reach, col, ...) {

  # subset data
  rch_id <- reach
  x <- x %>% filter(reach == rch_id)

  # plot main barplot
  barplot(
    as.matrix(x[, -c(1:3)]),
    beside = TRUE,
    space = c(0, 2),
    col = col,
    las = 1,
    xlab = "Water year",
    ylab = "Proportional site occupancy"
  )

  # add a label
  sys_lab <- unique(x$system)
  sys_lab <- paste0(toupper(substr(sys_lab, 1, 1)), substr(sys_lab, 2, nchar(sys_lab)))
  sys_lab <- gsub("_", " ", sys_lab)
  sys_lab <- gsub("ck", "Creek", sys_lab)
  if (reach != "NA")
    sys_lab <- paste0(sys_lab, " River: Reach ", reach)
  mtext(sys_lab, side = 3, adj = 0, line = 0.4, cex = 1.05)

  # return nothing
  NULL

}


# internal function: set class for examine_distribution outputs
as_distribution_summary <- function(x) {
  as_class(x, name = "distribution_summary", type = "tbl_df")
}
