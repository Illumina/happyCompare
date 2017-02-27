## happy_roc methods

#' is_happy_roc
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_happy_roc = function(x) {
    inherits(x, "happy_roc")
}

#' print.happy_roc
#' 
#' Print a happy_roc object.
#' 
#' @param x A `happy_roc` object.
#' @param ... Extra arguments.
#' @export
print.happy_roc = function(x, ...) {
    print(lapply(x, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `happy_roc` object by converting into a single `data.table`.
#' 
#' @param x A `happy_roc` object.
#' @param ... Extra arguments.
#' @export
tidy.happy_roc = function(x, ...) {
    x %>% dplyr::bind_rows()
}

#' plot
#' 
#' Plot ROC curves.
#' 
#' @param x A `happy_roc` object.
#' @param ... Extra arguments.
plot.happy_roc = function(x, ...) {
    # re-format data
    data = tidy(x)
    
    # axis limits
    xlim = c(max(0, data %>% filter(Filter == "CONN") %>% select(METRIC.Recall) %>% min() - 0.02),
             min(1, data %>% filter(Filter == "CONN") %>% select(METRIC.Recall) %>% max() + 0.02))
    ylim = c(max(0, data %>% filter(Filter == "CONN") %>% select(METRIC.Precision) %>% min() - 0.01),
             min(1, data %>% filter(Filter == "CONN") %>% select(METRIC.Precision) %>% max() + 0.01))
    
    # plot
    ggplot(data, aes(x = METRIC.Recall, y = METRIC.Precision, color = group, group = group)) +
        coord_cartesian(xlim = xlim, ylim = ylim) +
        # roc lines
        geom_line(data = data %>% filter(Filter == "ROC"), size = 1) +
        # connector between ALL and start of ROC
        geom_line(data = data %>% filter(Filter == "CONN"), size = 1, lty = 2) +
        # pass and all points
        geom_point(data = data %>% filter(Filter == "CONN"), size = 4)
}