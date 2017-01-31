## happy_extended methods

#' is_happy_extended_ci
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_happy_extended_ci = function(x) {
    inherits(x, "happy_extended_ci")
}

#' plot_subset
#' 
#' Plot performance metrics for the specified subset.
#' 
#' @param x A `happy_extended_ci` object.
#' @param type Variant type. One of: SNP, INDEL.
#' @param filter Variant filter. One of: PASS (default), ALL.
#' @param subset A subset id.
#' @param metric The performance metric to evaluate. One of: METRIC.Recall.
#' @param xlim.low Lower bound for x axis. Default: 0.
#' @param xlim.high Upper bound for x axis. Default: 1.
#' @param ... Extra arguments.
#' @export
plot_subset.happy_extended_ci = function(x, type, filter = 'PASS', subset, metric,
                                  xlim.low = 0, xlim.high = 1, ...) {
    ## validate input
    if (missing(type) || missing(subset) || missing(metric)) {
        stop("Must specify a variant type.")
    }
    
    ## subset data
    data = x %>%
        filter(Type == type, Filter == filter, Subset == subset) %>%
        select_(.dots = c("Type", "Filter", "Subset", "Group.Id", names(.)[grepl(metric, names(.))])) %>%
        rename_(.dots = setNames(names(.), gsub(paste0(metric, '.'), '', names(.))))

    if (dim(data)[1] == 0) {
        stop('No data available for plotting, revise arguments.')
    }
    
    ## plot
    p1 = data %>%
        select(Group.Id, successes, totals, replicate) %>%
        reshape2::melt() %>%
        ggplot(aes(x = '', y = replicate)) +
        geom_tile(aes(fill = value)) +
        geom_text(aes(label = value), color = 'white') +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              strip.background = element_blank(),
              strip.text.y = element_blank(),
              legend.position = "none",
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.border = element_blank()) +
        xlab("") +
        ylab("") +
        facet_grid(Group.Id ~ variable, scales = "free_y", switch = "y")        
        
    p2 = data %>%
        select(Group.Id, replicate, low, theta1, high, theta0) %>%
        reshape2::melt() %>%
        mutate(distribution = ifelse(grepl('0', variable), 'prior', 'posterior'),
               title = rep(metric, n())) %>%
        ggplot(aes(x = value, y = replicate, group = replicate)) +
        geom_point(aes(color = distribution), size = 2) +
        geom_line() +
        theme(axis.title.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              legend.position = "none") +
        xlim(xlim.low, xlim.high) +
        xlab("") +
        ylab("") +
        facet_grid(Group.Id ~ title, scales = "free_y")
    
    gridExtra::grid.arrange(p1, p2, nrow = 1,
                            widths = c(2.5, 3),
                            top = sprintf("%s - %s %s", subset, filter, type))
}
