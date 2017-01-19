## happy_extended methods

library(ggplot2)
library(dplyr)

#' @export
is_happy_extended = function(obj) {
    inherits(obj, "happy_extended")
}

#' @export
print.happy_extended = function(obj) {
    print(lapply(obj, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `happy_extended` object by converting into a single `data.table`.
#' 
#' @param obj A `happy_extended` object.
#' @export
tidy.happy_extended = function(obj) {
    df = plyr::ldply(obj, data.frame)
    return(df)
}
