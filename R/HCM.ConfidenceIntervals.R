#' HCM.ConfidenceIntervals
#'
#' Plot average and confident intervals for performance metrics in a given subset.
#'
#' @param subsetted_happy_metrics A `data.frame` with happy metrics obtained with `load_happy_metrics()`, 
#' subsetted to contain a single subset id.
#' @param variant_type Variant type. Default = NULL.
#' @param filter Variant filter. Default = PASS.
#' @return A `list` of results.
#' @examples
#' \dontrun{HCM.ConfidenceIntervals(subsetted_happy_metrics = subsetted_happy_metrics, variant_type = 'SNP')}
#' @export

HCM.ConfidenceIntervals = function(subsetted_happy_metrics, variant_type, filter = 'PASS') {
    ## checks
    if (dim(subsetted_happy_metrics)[1] == 0) {
        stop("Input dataframe is empty.")
    }
    
    if (length(unique(subsetted_happy_metrics$Subset)) > 1) {
        stop("Input dataframe contains multiple subset ids.")
    }
    
    if (!variant_type %in% unique(subsetted_happy_metrics)$Type) {
        stop("Invalid variant type.")
    }
    
    if (!filter %in% unique(subsetted_happy_metrics)$Filter) {
        stop("Invalid filter.")
    }

    # functions
    .prepare_data = function(happy_metrics, subset_id, variant_type, filter) {
        id.vars = c("Id", "Group", "Type", "Subset", "Subset.Size", "TRUTH.TOTAL", "QUERY.TOTAL")
        m = c("METRIC.Precision", "METRIC.Recall", "METRIC.Frac_Assessed")
        m.lower = paste0(m, ".Lower")
        m.upper = paste0(m, ".Upper")
        measure.vars = c(m, m.lower, m.upper)
        
        metrics_subset = happy_metrics[happy_metrics$Filter == filter &
                                         happy_metrics$Genotype == "*" &
                                         happy_metrics$Subtype == "*" &
                                         happy_metrics$Subset.Level == 2 &
                                         happy_metrics$Type == variant_type,
                                       c(id.vars, measure.vars)]
        
        if (dim(metrics_subset)[1] == 0) {
            stop("Cannot proceed with an empty data frame.")
        }
        
        # add average per group
        avg = data.frame(t(sapply(unique(metrics_subset$Group), function(x) {
          apply(metrics_subset[metrics_subset$Group == x, c(measure.vars, "QUERY.TOTAL")], 2, function(y) mean(y, na.rm = T))
        })))
        avg$Id = rep(".mean", 2)
        avg$Group = rownames(avg)
        avg$Type = rep(variant_type, 2)
        avg$Subset = rep(subset_id, 2)
        avg$Subset.Size = unique(metrics_subset$Subset.Size)
        avg$TRUTH.TOTAL = unique(metrics_subset$TRUTH.TOTAL)
        rownames(avg) = NULL
        avg = avg[,c(id.vars, measure.vars)]
        metrics_subset = rbind(metrics_subset, avg)

        # melt
        id.vars = c("Id", "Group", "Type", "Subset", "Subset.Size", "TRUTH.TOTAL", "QUERY.TOTAL")
        m = c("METRIC.Precision", "METRIC.Recall", "METRIC.Frac_Assessed")
        m.lower = paste0(m, ".Lower")
        m.upper = paste0(m, ".Upper")
        measure.vars = c(m, m.lower, m.upper)
        mdata = reshape2::melt(metrics_subset, id.vars = id.vars, measure.vars = measure.vars)
        return(mdata)
    }
    
    .plot_query_total = function(mdata) {
        mdata = unique(mdata[,c("Group", "Id", "QUERY.TOTAL")])
        mdata$log10_query_total = log10(mdata$QUERY.TOTAL)
        ggplot2::ggplot(mdata, aes(y = Id, x = "", group = Id)) +
          geom_tile(aes(fill = log10_query_total)) +
          geom_text(aes(label = QUERY.TOTAL), color = "white") +
          theme(axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks = element_blank()) +
          theme(strip.background = element_blank(),
                strip.text.y = element_blank()) +
          theme(legend.position = "none") +
          theme(panel.background = element_rect(fill = "white")) +
          xlab("") +
          ylab("") +
          ggtitle("Q.TOTAL") +
          facet_grid(Group ~ ., scales = "free_y", switch = "y")
    }
    
    .plot_ci = function(metric, mdata) {
        ggplot2::ggplot(mdata[grep(metric, mdata$variable),], aes(y = Id, x = value, group = Id)) +
          geom_point(aes(color = variable), size = 2) +
          geom_line() +
          theme_bw() +
          theme(axis.title.x = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) +
          theme(legend.position = "none") +
          xlim(0,1) +
          xlab("") +
          ylab("") +
          ggtitle(metric) +
          facet_grid(Group ~ ., scales = "free_y")
    }
    
    # data
    mdata = .prepare_data(subsetted_happy_metrics, subset_id, variant_type, filter)

    # plot
    plot = gridExtra::grid.arrange(
        .plot_query_total(mdata),
        .plot_ci(mdata, metric = "METRIC.Precision"),
        .plot_ci(mdata, metric = "METRIC.Recall"),
        .plot_ci(mdata, metric = "METRIC.Frac_Assessed"),
        nrow = 1,
        widths = c(2.5, 3, 3, 3),
        top = sprintf("SUBSET.ID: %s  VARIANT.TYPE: %s  SUBSET.SIZE: %d  TRUTH.TOTAL: %d",
                      subset_id,
                      variant_type,
                      unique(mdata$Subset.Size),
                      unique(mdata$TRUTH.TOTAL))
    )
    
    result = list(
        data = list(raw = mdata),
        plots = list(combined = plot)
    )
    return(result)
}
