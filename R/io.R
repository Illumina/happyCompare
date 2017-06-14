## I/O methods


#' Load data from happyCompare samplesheets
#' 
#' Load hap.py results into a \code{happy_compare} object as specified in
#' the \code{happyCompare} samplesheet provided. Depends on \code{happyR}.
#' 
#' @param samplesheet_path Path to a happyCompare samplesheet. Required fields:
#'   \code{Group.Id}, \code{Sample.Id}, \code{Replicate.Id},
#'   \code{happy_prefix}.
#' @param lazy Do not load larger hap.py results until needed. Default: \code{TRUE}.
#'   
#' @return A \code{happy_compare} object, with the following fields: 
#' \itemize{ 
#'   \item{\code{samplesheet}: the original samplesheet, stored as a
#'         \code{data.frame}}. 
#'   \item{\code{happy_results}: a \code{happy_results_list}
#'         object that contains individual \code{happy_result} objects as defined in
#'         \code{happyR}}. 
#' }
#'   
#' @examples
#' 
#' \dontrun{
#' happy_compare = read_samplesheet(samplesheet_path = "happyCompare_samplesheet.csv")
#' }
#' 
#' @author Mar Gonzalez-Porta
#' 
#' @export
read_samplesheet = function(samplesheet_path, lazy = TRUE) {
  
  # check input
  if (!file.exists(samplesheet_path)) {
    stop("Cannot find samplesheet")
  }

  # prepare samplesheet
  samplesheet = read.csv(samplesheet_path)
  
  # prepare ids
  ids = samplesheet %>% 
    mutate(.Id = paste(Group.Id, Sample.Id, Replicate.Id, sep = "-")) %>% 
    select(.Id) %>% 
    unlist()
  
  # load happy results
  happy_results = lapply(seq_along(ids), function(i) {
    message(sprintf("Processing %s", ids[i]))
    
    happy_prefix = samplesheet %>% 
      slice(i) %>% 
      select(happy_prefix) %>% 
      unlist() %>% 
      as.character()
    happyR::read_happy(happy_prefix = happy_prefix, lazy = lazy)
  })
  names(happy_results) = ids
  happy_results = structure(happy_results, class = "happy_results_list")
  
  # create happy_compare
  happy_compare = read_samplesheet_(samplesheet = samplesheet, 
                                        happy_results = happy_results, ids = ids)
  
  return(happy_compare)
  
}


#' Manually create happy_compare objects
#' 
#' Create a happy_compare object from its individual components.
#' 
#' @param samplesheet A happyCompare samplesheet (\code{data.frame}). Required fields:
#'   \code{Group.Id}, \code{Sample.Id}, \code{Replicate.Id},
#'   \code{happy_prefix}.
#' @param happy_results A \code{happy_results_list} object obtained with \code{happyR}.
#' @param ids A \code{vector} of unique ids to map samplesheet entries with happy results. 
#'   
#' @return A \code{happy_compare} object, with the following fields: 
#' \itemize{ 
#'   \item{\code{samplesheet}: the original samplesheet, stored as a
#'         \code{data.frame}}. 
#'   \item{\code{happy_results}: a \code{happy_results_list}
#'         object that contains individual \code{happy_result} objects as defined in
#'         \code{happyR}}. 
#' }
#'   
#' @examples
#' 
#' \dontrun{
#' happy_compare = read_samplesheet_(samplesheet = samplesheet, 
#' happy_results = happy_results, ids = ids)
#' }
#' 
#' @export
read_samplesheet_ = function(samplesheet, happy_results, ids) {
  
  # validate input
  if (!"happy_results_list" %in% class(happy_results)) {
    stop("happy_results must be a happy_results_list object")
  }
  
  if (length(ids) != dim(samplesheet)[1] && length(ids) != length(happy_results)) {
    stop("Must provide a unique id for each result")
  }
  
  required_cols = c("Group.Id", "Sample.Id", "Replicate.Id", "happy_prefix")
  if (!all(required_cols %in% colnames(samplesheet))) {
    stop("The provided samplesheet is missing required columns, see docs")
  }  
  
  # add id mappings  
  samplesheet = samplesheet %>% 
    mutate(.Id = ids)
  names(happy_results) = ids
  
  # create happy_compare
  happy_compare = list(
    samplesheet = samplesheet,
    happy_results = happy_results
  )
  happy_compare = structure(happy_compare, class = "happy_compare")
  
  return(happy_compare)
  
}
