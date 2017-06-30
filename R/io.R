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
#' happy_compare = read_samplesheet(samplesheet_path = 'happyCompare_samplesheet.csv')
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
#' Load hap.py results into a `happy_compare` object using `happyR`.
#' 
#' @param samplesheet A `happyCompare` samplesheet (`data.frame`). Required fields:
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
#' happy_compare = read_samplesheet(samplesheet_path = 'happyCompare_samplesheet.csv')
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
  
  # load happy results
  ids <- samplesheet$happy_prefix
  happy_results <- lapply(seq_along(ids), function(i) {
    message(sprintf("Processing %s", ids[i]))
    happyR::read_happy(happy_prefix = ids[i], lazy = lazy)
  })
  names(happy_results) <- ids
  happy_results <- structure(happy_results, class = "happy_result_list")
  
  # create happy_compare object
  happy_compare <- list(samplesheet = samplesheet, happy_results = happy_results, ids = ids)
  happy_compare <- structure(happy_compare, class = "happy_compare")
  
  return(happy_compare)
  
}