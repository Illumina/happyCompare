## generic functions

#' tidy
#' 
#' Generic for tidy().
#' 
#' @param x Input object.
#' @param ... Extra arguments.
#' @export
tidy = function(x, ...) {
    UseMethod("tidy", x)
}