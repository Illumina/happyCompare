## I/O methods


#' Load data from happyCompare samplesheets
#' 
#' Load hap.py results into a `happy_compare` object using `happyR`.
#' 
#' @param samplesheet_path Path to a `happyCompare` samplesheet. Required fields:
#'   `Group.Id`, `Sample.Id`, `Replicate.Id`, `happy_prefix`.
#' @param lazy Do not load larger hap.py results until needed. Default: `TRUE`.
#'   
#' @return A `happy_compare` object, with the following fields: 
#' \itemize{ 
#'   \item{`samplesheet`: the original samplesheet, stored as a `data.frame`}. 
#'   \item{`happy_results`: a `happy_result_list`
#'         object that contains individual `happy_result` objects as defined in
#'         `happyR`}. 
#'   \item{`ids`: a vector of unique identifiers for each result.}
#' }
#'   
#' @examples
#' 
#' \dontrun{
#' happy_compare <- read_samplesheet(samplesheet_path = 'happyCompare_samplesheet.csv')
#' }
#' 
#' @export
read_samplesheet <- function(samplesheet_path, lazy = TRUE) {
  
  message("Reading happyCompare samplesheet")
  if (!file.exists(samplesheet_path)) {
    stop(sprintf("Cannot find samplesheet: %s", samplesheet_path))
  }
  
  samplesheet <- readr::read_csv(samplesheet_path)
  happy_compare <- read_samplesheet_(samplesheet, lazy = lazy)  
  return(happy_compare)
  
}


#' Load data from happyCompare samplesheets
#' 
#' Load hap.py results, build metrics and custom metadata into a `happy_compare` object using `happyR`.
#' 
#' @param samplesheet A `happyCompare` samplesheet (`data.frame`). Required fields:
#'   `Group.Id`, `Sample.Id`, `Replicate.Id`, `happy_prefix`. Optional fields: `build_metrics`. Also supports 
#'   additional metadata fields supplied as extra columns.
#' @param lazy Do not load larger hap.py results until needed. Default: `TRUE`.
#'   
#' @return A `happy_compare` object, with the following fields: 
#' \itemize{ 
#'   \item{`samplesheet`: the original samplesheet, stored as a `data.frame`}. 
#'   \item{`happy_results`: a `happy_result_list`
#'         object that contains individual `happy_result` objects as defined in
#'         `happyR`}. 
#'   \item{`build_metrics`: a `build_metrics_list` that contains a list of `data.frames` created from the csv 
#'         files provided under the `build_metrics` column of the samplesheet}. 
#'   \item{`ids`: a vector of unique identifiers for each result.}
#' }
#'   
#' @examples
#' 
#' \dontrun{
#' happy_compare <- read_samplesheet(samplesheet_path = 'happyCompare_samplesheet.csv')
#' }
#' 
#' @export
read_samplesheet_ <- function(samplesheet, lazy = TRUE) {
  
  # validate input
  if (! "data.frame" %in% class(samplesheet)) {
    stop("samplesheet must be a data.frame")
  }
  
  required_cols <- c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix")
  if (!all(required_cols %in% colnames(samplesheet))) {
    stop("The provided samplesheet is missing required columns, see docs")
  }
  
  # define ids
  ids <- samplesheet$happy_prefix
  
  # load happy results
  happy_results <- lapply(seq_along(ids), function(i) {
    message(sprintf("Processing %s", ids[i]))
    happyR::read_happy(happy_prefix = ids[i], lazy = lazy)
  })
  names(happy_results) <- ids
  class(happy_results) <- c("happy_result_list", class(happy_results))
  
  # load build metrics if available
  build_metrics <- tibble::tibble()
  if ("build_metrics" %in% colnames(samplesheet)) {
    paths <- samplesheet$build_metrics
    build_metrics <- lapply(seq_along(paths), function(i) {
      message(sprintf("Loading build metrics from %s", paths[i]))
      suppressMessages(r <- readr::read_csv(file = paths[i], col_names = TRUE))
      r$from <- as.character(ids[i])
      class(r) <- c("build_metrics", class(r))
      r
    })
    names(build_metrics) <- ids
    class(build_metrics) <- c("build_metrics_list", class(build_metrics))
  }
  .validate_metrics_headers(build_metrics_list = build_metrics)
  
  # create happy_compare object
  happy_compare <- list(samplesheet = samplesheet, happy_results = happy_results, 
                        build_metrics = build_metrics, ids = ids)
  happy_compare <- structure(happy_compare, class = "happy_compare")
  
  return(happy_compare)
  
}


.validate_metrics_headers <- function(build_metrics_list) {
  colnames <- lapply(build_metrics_list, colnames)
  freq <- table(unlist(colnames))
  discordant_names <- names(freq[freq < length(build_metrics_list)])
  if (length(discordant_names) > 0) {
    warning(sprintf("Invalid build metrics input - found %s discordant column names: %s", 
                 length(discordant_names), paste(discordant_names, collapse = ", ")))
    return(invisible(discordant_names))
  }
}
