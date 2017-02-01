## sompy_stats methods

#' is_sompy_stats
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_sompy_stats = function(x) {
    inherits(x, "sompy_stats")
}

#' print.sompy_stats
#' 
#' Print a sompy_stats object.
#' 
#' @param x A `sompy_stats` object.
#' @param ... Extra arguments.
#' @export
print.sompy_stats = function(x, ...) {
    print(lapply(x, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `sompy_stats` object by converting into a single `data.table`.
#' 
#' @param x A `sompy_stats` object.
#' @param ... Extra arguments.
#' @export
tidy.sompy_stats = function(x, ...) {
    x %>% dplyr::bind_rows()
}

#' plot.sompy_stats
#'
#' Plot sompy stats results.
#' 
#' @param x A `sompy_stats` object.
#' @param type Variant type. One of: SNVs, indels.
#' @param filter Variant filter, only used in the plot title.
#' @param xlim.low Lower bound for x axis. Default: 0.
#' @param xlim.high Upper bound for x axis. Default: 1.
#' @param ylim.low Lower bound for y axis. Default: 0.
#' @param ylim.high Upper bound for y axis. Default: NA.
#' @param point.size Point size in scatterplot. Default: 3.
#' @param font.size Font size. Default: 12.
#' @param ... Extra arguments.
#' @export
plot.sompy_stats = function(x, type, filter = 'PASS', 
                            xlim.low = 0, xlim.high = 1,
                            ylim.low = 0, ylim.high = NA,
                            point.size = 3, font.size = 12, ...) {
    
    ## validate input
    if (missing(type)) {
        stop("Must specify a variant type.")
    }
    
    valid_types = c('SNVs', 'indels')
    if (!type %in% valid_types) {
        stop("Invalid variant type.")
    }
    
    ## plot
    type_opt = type
    
    p1 = tidy(x) %>% 
        filter(type == type_opt) %>% 
        select(Group.Id, Sample.Id, recall, fp.rate, ambiguous) %>% 
        data.table::data.table() %>% 
        ggplot(aes(x = recall, y = fp.rate)) +
        geom_point(aes(color = Group.Id, shape = Sample.Id), size = point.size) +
        xlim(xlim.low, xlim.high) +
        ylim(ylim.low, ylim.high) +
        theme(legend.position = 'left')
    
    p2 = tidy(x) %>% 
        filter(type == type_opt) %>% 
        ggplot(aes(x = Group.Id, y = recall)) +
        geom_boxplot(aes(fill = Group.Id)) +
        geom_jitter() +
        ylim(ylim.low, ylim.high) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "none")
    
    p3 = tidy(x) %>% 
        filter(type == type_opt) %>% 
        ggplot(aes(x = Group.Id, y = fp.rate)) +
        geom_boxplot(aes(fill = Group.Id)) +
        geom_jitter() +
        ylim(ylim.low, ylim.high) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "none")
    
    p4 = tidy(x) %>% 
        filter(type == type_opt) %>% 
        ggplot(aes(x = Group.Id, y = ambiguous)) +
        geom_boxplot(aes(fill = Group.Id)) +
        geom_jitter() +
        ylim(ylim.low, ylim.high) +
        theme(text = element_text(size = font.size)) +
        theme(legend.position = "none")
    
    title = paste0(filter, " ",type)
    
    gridExtra::grid.arrange(p1, p2, p3, p4, 
                            nrow = 1, 
                            widths = c(3, 1, 1, 1),
                            top = grid::textGrob(title)
    )
       
}
    
#' plot_af.sompy_stats
#'
#' Plot sompy stats results stratified by allele frequency.
#' 
#' @param x A `sompy_stats` object.
#' @param type Variant type. One of: SNVs, indels.
#' @param filter Variant filter, only used in the plot title.
#' @param xlim.low Lower bound for x axis. Default: 0.
#' @param xlim.high Upper bound for x axis. Default: 1.
#' @param point.size Point size in scatterplot. Default: 3.
#' @param font.size Font size. Default: 12.
#' @param ... Extra arguments.
#' @export
plot_af.sompy_stats = function(x, type, filter = 'PASS',
                                  xlim.low = 0, xlim.high = 1,
                                  point.size = 3, font.size = 12, ...) {
    ## validate input
    if (missing(type)) {
        stop("Must specify a variant type.")
    }
    
    valid_types = c('SNVs', 'indels')
    if (!type %in% valid_types) {
        stop("Invalid variant type.")
    }
    
    type_opt = type
    
    p1 = tidy(x) %>% 
        filter(grepl(pattern = paste0(type_opt, '.'), x = type)) %>% 
        mutate(AF_bin = gsub(pattern = paste0(type_opt, '.'), replacement = '', x = type)) %>%
        select(Group.Id, Sample.Id, AF_bin, recall, fp.rate, ambiguous) %>% 
        data.table::data.table() %>% 
        reshape2::melt(id.vars = c('Group.Id', 'Sample.Id', 'AF_bin')) %>% 
        ggplot(aes(x = value, y = AF_bin)) +
        geom_point(aes(color = Group.Id, shape = Sample.Id), size = point.size) +
        xlim(0, NA) +
        theme(legend.position = 'left') +
        facet_grid(Sample.Id ~ variable, scales = "free_x")
    
    p2 = tidy(x) %>% 
        filter(grepl(pattern = paste0(type_opt, '.'), x = type)) %>% 
        mutate(AF_bin = gsub(pattern = paste0(type_opt, '.'), replacement = '', x = type)) %>%
        group_by(Group.Id, Sample.Id, AF_bin) %>% 
        summarise(TOTAL.QUERY = mean(total.query),
                  sd = sd(total.query)) %>%
        ggplot(aes(x = AF_bin, y = TOTAL.QUERY, fill = Group.Id)) +
        geom_bar(stat = 'identity', position = position_dodge()) +
        geom_errorbar(aes(ymin = TOTAL.QUERY - sd, ymax = TOTAL.QUERY + sd), width = 0.2, position = position_dodge(0.9)) +
        coord_flip() +
        theme(axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks.y = element_blank(), 
              legend.position = 'none') +
        facet_grid(Sample.Id ~ .) +
        ggtitle('')
    
    gridExtra::grid.arrange(p1, p2, nrow = 1, widths = c(3, 1),
                            top = paste(filter, type))
    
}

#' summary.sompy_stats
#'
#' Summarise happy summary results into tabular format.
#' 
#' @param object A `sompy_stats` object.
#' @param type Variant type. One of: SNVs, indels.
#' @param filter Variant filter, only used in the table caption.
#' @param digits Number of significant digits in summary statistics. Default: 4.
#' @param kable_format a character string to pass to `knitr::kable()`. Default: markdown.
#' @param ... Extra arguments.
#' @export
summary.sompy_stats = function(object, type, filter = 'PASS', digits = 4,
                                 kable_format = 'markdown', ...) {
    
    ## validate input
    if (missing(type)) {
        stop("Must specify a variant type.")
    }
    
    ## sumamrise
    type_opt = type
    caption = paste(filter, type, collapse = '-')
    data = tidy(object) %>%
        filter(type == type_opt) %>%
        group_by(Group.Id) %>%
        summarise(
            N = n(),
            recall.mean = round(mean(recall), digits = digits),
            recall.sd = round(sd(recall), digits = digits),
            fp.rate.mean = round(mean(fp.rate),digits = digits),
            fp.rate.sd = round(sd(fp.rate), digits = digits),
            ambiguous.mean = round(mean(ambiguous), digits = digits),
            ambiguous.sd = round(sd(ambiguous), digits = digits)
        ) %>%
        mutate(
            recall = paste(recall.mean, '&plusmn;', recall.sd),
            fp.rate = paste(fp.rate.mean, '&plusmn;', fp.rate.sd),
            ambiguous = paste(ambiguous.mean, '&plusmn;', ambiguous.sd)
        ) %>%
        select(Group.Id, N, recall, fp.rate, ambiguous) %>%
        knitr::kable(format = kable_format, caption = caption)
    return(data)
    
}