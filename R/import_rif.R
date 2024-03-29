#' Import a RIF formatted file into a tibble
#'
#' Reads a file that is stored in RIF format and returns a tibble. The function
#' validates the presence of required fields, and checks whether the variable
#' names in the data match the specified distribution.
#'
#' @inheritParams readr::read_lines
#' @return An object of class `reference`, basically a tibble with an attribute
#' called `study`.
#' @author Stef van Buuren 2021
#' @examples
#' fn <- system.file("testdata/nl_2009_hgt_male_nl.txt", package = "centile")
#' myref <- import_rif(fn)
#' @export
import_rif <- function(file) {

  # find line of data block directive
  lines <- parse_character(read_lines(file, n_max = 25L))
  start_data <- which(tolower(lines) %in% "[data]")[1L]
  if (is.na(start_data)) stop("Directive `[data]` not found in first 25 lines.")

  # read and parse meta data into a named character vector study
  df <- read_delim(file,
    delim = "=",
    skip = 0L, n_max = start_data - 1L,
    col_names = c("key", "value"), col_types = "cc"
  )
  study <- parse_character(df$value)
  names(study) <- parse_character(df$key)
  study <- study[!is.na(study)]

  # check required study keys
  required <- c("name", "year", "yname", "distribution")
  found <- required %in% names(study)
  if (any(!found)) {
    stop("Required key(s) not found: ", paste(required[!found], collapse = ", "), ".")
  }

  # read data block
  data <- read_tsv(file,
    col_types = cols(.default = col_double()),
    skip = start_data
  )

  # validate variable names
  pos <- which("x" %in% tolower(names(data)))
  if (!pos) stop("Variable 'x' not found.")
  ok <- validate_distribution(names(data)[-pos], study["distribution"])
  if (!ok) stop("Variable names for distribution ", study["distribution"], " not found.")

  # save
  attr(data, "study") <- study
  data
}
