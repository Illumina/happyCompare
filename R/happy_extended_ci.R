## happy_extended methods

#' @export
is_happy_extended_ci = function(obj) {
    inherits(obj, "happy_extended_ci")
}

#' plot_subset
#' 
#' Plot performance metrics for the specified subset
#' 
#' @param obj A `happy_extended_ci` object.
#' @param type Variant type. One of: SNP, INDEL.
#' @param filter Variant filter. One of: PASS (default), ALL.
#' @param subset A subset id.
#' @param xlim.low Lower bound for x axis. Default: NA.
#' @param xlim.high Upper bound for x axis. Default: 1.
#' @export
plot_subset.happy_extended_ci = function(obj, type, filter = 'PASS', subset, metric,
                                  xlim.low = 0, xlim.high = 1, ...) {
    ## validate input
    if (missing(type) || missing(subset) || missing(metric)) {
        stop("Must specify a variant type.")
    }
    
    ## subset data
    data = obj %>%
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
