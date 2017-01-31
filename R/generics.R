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

#' add_credible_intervals
#' 
#' Generic for add_credible_intervals().
#' 
#' @param x Input object.
#' @param ... Extra arguments.
#' @export
add_credible_intervals = function(x, ...) {
    UseMethod("add_credible_intervals", x)
}

#' plot_subset
#' 
#' Generic for plot_subset().
#' 
#' @param x Input object.
#' @param ... Extra arguments.
#' @export
plot_subset = function(x, ...) {
    UseMethod("plot_subset", x)
}

#' plot_af
#' 
#' Generic for plot_af().
#' 
#' @param x Input object.
#' @param ... Extra arguments.
#' @export
plot_af = function(x, ...) {
    UseMethod("plot_af", x)
}