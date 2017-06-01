## analysis methods


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
