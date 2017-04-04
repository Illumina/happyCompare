## happyCompare_list methods


#' Load data from happyCompare samplesheets
#' 
#' Load hap.py results into a \code{happyCompare_list} object as specified in
#' the \code{happyCompare} samplesheet provided. Depends on \code{happyR}.
#' 
#' @param samplesheet_path Path to a happyCompare samplesheet. Required fields:
#'   \code{Group.Id}, \code{Sample.Id}, \code{Replicate.Id},
#'   \code{happy_prefix}.
#' @param lazy Do not load larger hap.py results until needed. Default: \code{TRUE}.
#'   
#' @return A \code{happyCompare_list} object, with the following fields: 
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
#' happyCompare_list = read_samplesheet(samplesheet_path = "happyCompare_samplesheet.csv")
#' }
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
  
  # create happyCompare_list
  happyCompare_list = read_samplesheet_(samplesheet = samplesheet, 
                                        happy_results = happy_results, ids = ids)
  
  return(happyCompare_list)
  
}


#' Manually create happyCompare_list objects
#' 
#' Create a happyCompare_list object from its individual components.
#' 
#' @param samplesheet A happyCompare samplesheet (\code{data.frame}). Required fields:
#'   \code{Group.Id}, \code{Sample.Id}, \code{Replicate.Id},
#'   \code{happy_prefix}.
#' @param happy_results A \code{happy_results_list} object obtained with \code{happyR}.
#' @param ids A \code{vector} of unique ids to map samplesheet entries with happy results. 
#'   
#' @return A \code{happyCompare_list} object, with the following fields: 
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
#' happyCompare_list = read_samplesheet_(samplesheet = samplesheet, 
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
  
  # create happyCompare_list
  happyCompare_list = list(
    samplesheet = samplesheet,
    happy_results = happy_results
  )
  happyCompare_list = structure(happyCompare_list, class = "happyCompare_list")
  
  return(happyCompare_list)
  
}


#' Extract tables from a happyCompare_list object
#' 
#' Extract tables from a `happyCompare_list object` and combine into a single
#' \code{data.frame}. Appends samplesheet metadata to the original happy
#' results.
#' 
#' @param happyCompare_list A \code{happyCompare_list} object.
#' @param table Table of data to extract from each \code{happy_result} object.
#'   Must be one of: \code{summary}, \code{extended}, \code{pr_curve.all},
#'   \code{pr_curve.SNP},  \code{pr_curve.SNP_PASS}, \code{pr_curve.SNP_SEL},
#'   \code{pr_curve.INDEL},  \code{pr_curve.INDEL_PASS},
#'   \code{pr_curve.INDEL_SEL}.
#'   
#' @return A \code{data.frame} with combined information from the selected
#'   \code{happy_result} and its \code{samplesheet}.
#'   
#' @examples
#' 
#' \dontrun{
#' summary = extract(happyCompare_list, table = "summary")
#' roc = extract(happyCompare_list, table = "pr_curve.all")
#' }
#' @export
extract = function(happyCompare_list, ...) {
  UseMethod("extract", happyCompare_list)
}
#' @export
extract.happyCompare_list <- function(happyCompare_list, 
                                      table = c("summary", "extended", "pr_curve.all", 
                                                "pr_curve.SNP", "pr_curve.SNP_PASS", "pr_curve.SNP_SEL",
                                                "pr_curve.INDEL", "pr_curve.INDEL_PASS", "pr_curve.INDEL_SEL")) {
  
  # validate input
  if (!"happyCompare_list" %in% class(happyCompare_list)) {
    stop("Must provide a happyCompare_list object")
  }
  
  table = match.arg(table)
  
  # extract results into a data.frame
  ids = happyCompare_list$samplesheet$.Id
  
  if (table %in% c("summary", "extended")) {
    item_list = lapply(ids, function(id) {
      if (!table %in% names(happyCompare_list$happy_results[[id]])) {
        stop(sprintf("Could not find %s for happy_results_list with id %s", table, id))
      }
      table_out = merge(
        happyCompare_list$samplesheet %>% 
          filter(.Id == id),
        happyCompare_list$happy_results[[id]][[table]] %>% 
          data.frame() %>% 
          mutate(.Id = id),
        by = ".Id"
      )
      table_out
    })
    df <- dplyr::bind_rows(item_list)
  }
  
  if (grepl("pr_curve", table)) {
    item_list = lapply(ids, function(id) {
      table_split = unlist(strsplit(table, "\\."))
      if (!table_split[1] %in% names(happyCompare_list$happy_results[[id]])) {
        stop(sprintf("Could not find %s for happy_results_list with id %s", table_split[1], id))
      }
      table_out = merge(
        happyCompare_list$samplesheet %>% 
          filter(.Id == id),
        happyCompare_list$happy_results[[id]][[table_split[1]]][[table_split[2]]] %>% 
          data.frame() %>% 
          mutate(.Id = id),
        by = ".Id"
      )
      table_out  
    })
    df <- dplyr::bind_rows(item_list)
  }
  
  # set class
  if (table == "summary") {
    class(df) <- c("happy_summary", class(df))
  }
  if (table == "extended") {
    class(df) <- c("happy_extended", class(df))
  }
  if (grepl("pr_curve", table)) {
    class(df) <- c("happy_roc", class(df))
  }  
  
  return(df)
  
}
