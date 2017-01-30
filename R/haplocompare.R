## haplocompare methods

#' @export
is_haplocompare = function(obj) {
    inherits(obj, "haplocompare")
}

#' @export
print.haplocompare = function(obj, ...) {
    print(sapply(obj, class))
}