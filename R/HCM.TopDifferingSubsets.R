#' HCM.TopDifferingSubsets
#'
#' Evaluate differences in performance metrics for subsets in `happy_metrics` and flag top differing ones.
#'
#' @param happy_metrics A `data.frame` with happy metrics obtained with `load_happy_metrics()`.
#' @param variant_type Variant type. Default = NULL.
#' @param filter Variant filter. Default = PASS.
#' @return A `list` of results.
#' @examples
#' \dontrun{HCM.TopDifferingSubsets(happy_metrics)}
#' @export

HCM.TopDifferingSubsets = function(happy_metrics, filter = "PASS", variant_type = NULL) {
    ## checks
    if (dim(happy_metrics)[1] == 0) {
        stop("Input dataframe is empty.")
    }
    
    if (is.null(variant_type) || !variant_type %in% unique(happy_metrics)$Type) {
        stop("Invalid variant type.")
    }
    
    if (!filter %in% unique(happy_metrics)$Filter) {
        stop("Invalid filter.")
    }

    ## functions
    .prepare_data = function(happy_metrics, filter, variant_type) {
        # for each subset, calculate average confidence intervals for each group using replicate data
        # then, if confidence intervals do not overlap across groups, label as significant
        id.vars = c("Id", "Group", "Type", "Subset", "Subset.Size", "TRUTH.TOTAL", "QUERY.TOTAL")
        performance_metrics = c("METRIC.Precision", "METRIC.Recall", "METRIC.Frac_Assessed")
        m.lower = paste0(performance_metrics, ".Lower")
        m.upper = paste0(performance_metrics, ".Upper")
        measure.vars = c(performance_metrics, m.lower, m.upper)
        
        metrics_subset = happy_metrics[happy_metrics$Filter == filter &
                                           happy_metrics$Genotype == "*" &
                                           happy_metrics$Subtype == "*" &
                                           happy_metrics$Subset.Level == 2 &
                                           happy_metrics$Type == variant_type,
                                       c(id.vars, measure.vars)]
        
        if (dim(metrics_subset)[1] == 0) {
            stop("Cannot proceed with an empty data frame.")
        }
        
        data = data.frame(
            Subset = unique(metrics_subset$Subset),
            Significant.Differences = rep(FALSE, length(unique(metrics_subset$Subset)))
        )
        
        smetrics_subset = split(metrics_subset, f = metrics_subset$Subset, drop = TRUE)
        data = lapply(seq_along(smetrics_subset), function(i) {
            subset_id = names(smetrics_subset)[i]
            ssmetrics_subset = split(smetrics_subset[[i]], f = smetrics_subset[[i]]$Group, drop = TRUE)
            if (length(ssmetrics_subset) == 2) {
                r = data.frame(t(sapply(performance_metrics, function(m) {
                    avg_lower_g1 = mean(ssmetrics_subset[[1]][,paste0(m, ".Lower")], na.rm = T)
                    avg_upper_g1 = mean(ssmetrics_subset[[1]][,paste0(m, ".Upper")], na.rm = T)
                    avg_lower_g2 = mean(ssmetrics_subset[[2]][,paste0(m, ".Lower")], na.rm = T)
                    avg_upper_g2 = mean(ssmetrics_subset[[2]][,paste0(m, ".Upper")], na.rm = T)
                    r = avg_lower_g1 > avg_upper_g2 | avg_upper_g1 < avg_lower_g2
                })))
                is_significant = any(as.logical(r))
                if (is_significant) {
                    r$subset_id = subset_id
                    return(r)
                }
            }
        })
        data = plyr::ldply(data[sapply(data, function(x) !is.null(x))], data.frame)
        return(data)
    }
    
    ## main
    data = .prepare_data(happy_metrics, filter, variant_type)

    ## result
    result = list(
        data = list(raw = data),
        plots = list()
    )
    return(result)
    
}
