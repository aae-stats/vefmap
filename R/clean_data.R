#' @name clean_data
#' @title Clean survey data associated with annual VEFMAP surveys
#' @description Series of functions to clean the data from VEFMAP surveys
#'   and standardise all column names and values
NULL

#' @rdname clean_data
#'
#' @export
#'
#' @importFrom dplyr filter select relocate `%>%`
#' @importFrom lubridate parse_date_time year month
#'
#' @param x djf
#' @param systems df
#' @param species df
#' @param years sf
#' @param gear d
#'
#' @details
#'
#' @examples
#'
#'
#'
clean_data <- function(x, systems = NULL, species = NULL, years = NULL, gear = NULL) {

  # record column names for checks below
  columns <- colnames(x)

  ## TODO: add check for standard column names to make sure everything is
  ##   provided
  ## Need: sdate or date, notes, site_name, waterbody, scientific_name,
  ##    gear_type, common_name, seconds, length_mm, id_taxon,
  ##    id_survey,

  # create a site column
  x$site <- x$site_name

  # extract reach info
  if (!("reach" %in% columns) & !is.null(x$notes))
    x$reach <- as.numeric(substr(x$notes, 8, 8))

  # fix dates and add a year id
  if ("sdate" %in% columns)
    x$date <- x$sdate
  x$date <- lubridate::parse_date_time(x$date, orders = c("ymd", "dmy"))
  x$water_year <- lubridate::year(x$date)
  x$water_year <- ifelse(lubridate::month(x$date) > 6, x$water_year + 1, x$water_year)

  # fix species names
  if ("scientific_name" %in% columns)
    x$species_name <- x$scientific_name
  x$species_name <- clean_species(x$species_name)
  x$common_name <- get_common_names(x$species_name)

  # and systems
  if ("waterbody" %in% columns)
    x$system <- x$waterbody
  x$system <- clean_systems(x$system)

  # filter data to relevant species, systems, years
  if (!is.null(systems))
    x <- x %>% filter(.$system %in% systems)
  if (!is.null(species))
    x <- x %>% filter(.$species_name %in% c(species, "null"))
  if (!is.null(years))
    x <- x %>% filter(.$water_year %in% years)
  if (!is.null(gear))
    x <- x %>% filter(.$gear_type %in% gear)

  # check data types
  if (!is.numeric(x$seconds))
    x$seconds <- as.numeric(x$seconds)

  # drop some columns for ease of use
  if ("sdate" %in% columns)
    x <- x %>% select(-sdate, -common_name, -scientific_name)

  # reorder remaining columns
  x <- x %>%
    relocate(date, water_year, system, site, reach) %>%
    relocate(species_name, .after = id_taxon)

  # return
  x

}

# internal function: tidy up species names
clean_species <- function(x) {

  # tidy it up and standardise a few common patterns
  x <- tolower(x)
  x <- gsub(" ", "_", x)
  x <- gsub("spp", "sp", x)
  x <- gsub("\\.", "", x)

  # return
  x

}

# internal function: tidy up system names
clean_systems <- function(x) {

  # tidy it up and standardise a few common patterns
  x <- tolower(x)
  x <- gsub(" ", "_", x)
  x <- gsub("creek", "ck", x)

  # return
  x

}
