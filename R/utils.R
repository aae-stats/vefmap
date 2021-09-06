# internal function: filter a data set by system, species, or year
#' @importFrom dplyr filter `%>%`
filter_survey <- function(x, system = NULL, species = NULL, reach = NULL, start = NULL, end = NULL) {

  # default to all levels of each var
  if (is.null(system))
    system <- unique(x$system)
  if (is.null(species))
    species <- unique(x$species_name)
  if (is.null(reach))
    reach <- unique(x$reach)

  # set default year range
  if (is.null(start))
    start <- min(x$water_year)
  if (is.null(end))
    end <- max(x$water_year)

  # rename system and reach to avoid naming conflicts in dplyr::filter
  sys_id <- system
  rch_id <- reach

  # return reduced data set
  x %>% filter(species_name %in% c(species, "null"), .$system %in% sys_id, .$reach %in% rch_id, water_year %in% c(start:end))

}

# internal function: parse survey table to give system and reach IDs
parse_survey <- function(x) {
  x <- rownames(x)
  x <- strsplit(x, ": reach ")
  system <- sapply(x, function(x) x[1])
  reach <- sapply(x, function(x) x[2])
  data.frame(system = system, reach = reach, sys_reach = paste(system, reach, sep = "_r"))
}

# internal function: check that all args have length == 1
check_single <- function(...) {
  args <- list(...)
  lens <- sapply(args, length)
  if (any(lens > 1))
    stop("only one species, system, and reach can be specified when plotting a length_summary", call. = FALSE)
  out <- NULL
}

# generate a unique ID for an object based on hex code
# function adapted from: https://github.com/greta-dev/greta
hex_id <- function() {
  paste(as.raw(sample.int(256L, 4, TRUE) - 1L), collapse = "")
}

# set an object class
as_class <- function(
  object,
  name,
  type = c("function", "list", "matrix", "array", "dynamics", "tbl_df")
) {

  type <- match.arg(type)
  stopifnot(inherits(object, type))
  class(object) <- c(name, class(object))

  object

}

# convert colours from RGB to hex codes
rgb_to_hex <- function(x) {
  rgb(x[1], x[2], x[3], maxColorValue = 255)
}

# get hex codes
blues_hex <- sapply(
  list(
    c(0, 169, 206),
    c(0, 49, 60),
    c(117, 117, 121),
    c(218, 219, 220),
    c(0, 0, 0)
  ),
  rgb_to_hex
)

# internal function: return common names from formatted scientific name
get_common_names <- function(x) {
  common_name_lookup <- c(
    "maccullochella_peelii" = "Murray cod",
    "macquaria_ambigua" = "Golden perch",
    "melanotaenia_fluviatilis" = "Murray-Darling rainbowfish",
    "bidyanus_bidyanus" = "Silver perch",
    "maccullochella_macquariensis" = "Trout cod",
    "macquaria_colonorum" = "Estuary perch",
    "gadopsis_marmoratus" = "River blackfish",
    "prototroctes_maraena" = "Australian grayling",
    "macquaria_novemaculeata" = "Australian bass",
    "pseudaphritis_urvillii" = "Tupong"
  )
  common_name_lookup[x]
}
