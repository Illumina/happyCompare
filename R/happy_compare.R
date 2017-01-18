## happy_compare generics and methods

#' @export
is_happy_compare = function(obj) {
    inherits(obj, "happy_compare")
}

#' @export
print.happy_compare = function(obj) {
    print(sapply(obj, class))
}