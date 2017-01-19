## happy_summary methods

#' @export
is_happy_summary = function(obj) {
    inherits(obj, "happy_summary")
}

#' @export
print.happy_summary = function(obj) {
    print(lapply(obj, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `happy_summary` object by converting into a single `data.table`.
#' 
#' @param obj A `happy_summary` object.
#' @export
tidy.happy_summary = function(obj) {
    df = plyr::ldply(obj, data.frame)
    return(df)
}

#' plot.happy_summary
#'
#' Plot happy summary results.
#' 
#' @param obj A `happy_summary` object.
#' @param type Variant type. One of: SNP, INDEL.
#' @param filter Variant filter. One of: PASS (default), ALL.
#' @param xlim.low Lower bound for x axis. Default: NA.
#' @param xlim.high Upper bound for x axis. Default: 1.
#' @param ylim.low Lower bound for y axis. Default: NA.
#' @param ylim.high Upper bound for y axis. Default: 1.
#' @param point.size Point size in scatterplot. Default: 3.
#' @param font.size Font size. Default: 12.
#' @export
plot.happy_summary = function(obj, type, filter = 'PASS', 
                              xlim.low = NA, xlim.high = 1, ylim.low = NA, ylim.high = 1,
                              point.size = 3, font.size = 12, ...) {
    
    ## validate input
    if (missing(type)) {
        stop("Must specify a variant type.")
    }
    
    ## plot
    data = tidy(obj) %>%
        filter(Type == type, Filter == filter)
    
    p1 = ggplot(data, aes(x = METRIC.Recall, y = METRIC.Precision)) +
        geom_point(aes(color = Group, shape = Sample.Id), size = point.size) +
        xlim(xlim.low, xlim.high) +
        ylim(ylim.low, ylim.high) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "left")
        
    p2 = ggplot(data, aes(x = Group, y = METRIC.Precision)) +
        geom_boxplot(aes(fill = Group)) +
        geom_jitter() +
        ylim(ylim.low, ylim.high) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "none")
        
    p3 = ggplot(data, aes(x = Group, y = METRIC.Recall)) +
        geom_boxplot(aes(fill = Group)) +
        geom_jitter() +
        ylim(ylim.low, ylim.high) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "none")
        
    p4 = ggplot(data, aes(x = Group, y = METRIC.Frac_NA)) +
        geom_boxplot(aes(fill = Group)) +
        geom_jitter() +
        ylim(0, NA) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "none")

    title = paste0(filter, " ",type)
    
    gridExtra::grid.arrange(p1, p2, p3, p4, 
                            nrow = 1, 
                            widths = c(3, 1, 1, 1),
                            top = grid::textGrob(title)
                            )
    
}