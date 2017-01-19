## generic functions

#' @export
tidy = function(obj) {
    UseMethod("tidy", obj)
}
