## haplocompare methods

#' is_haplocompare
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_haplocompare = function(x) {
    inherits(x, "haplocompare")
}

#' print.haplocompare
#' 
#' Print a haplocompare object.
#' 
#' @param x A `haplocompare` object.
#' @param ... Extra arguments.
#' @export
print.haplocompare = function(x, ...) {
    print(sapply(x, class))
}