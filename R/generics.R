#' @export
extract = function(happyCompare_list, ...) {
  UseMethod("extract", happyCompare_list)
}

#' @export
tabulate = function(happy_summary, ...) {
  UseMethod("tabulate", happy_summary)
}

#' @export
plot = function(happy_roc, ...) {
  UseMethod("plot", happy_roc)
}