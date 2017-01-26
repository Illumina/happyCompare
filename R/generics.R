## generic functions

#' @export
tidy = function(obj, ...) {
    UseMethod("tidy", obj)
}

#' @export
add_credible_intervals = function(obj, ...) {
    UseMethod("add_credible_intervals", obj)
}
