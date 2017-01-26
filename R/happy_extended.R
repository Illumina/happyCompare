## happy_extended methods

#' @export
is_happy_extended = function(obj) {
    inherits(obj, "happy_extended")
}

#' @export
print.happy_extended = function(obj) {
    print(lapply(obj, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `happy_extended` object by converting into a single `data.table`.
#' 
#' @param obj A `happy_extended` object.
#' @return A `data.table` object.
#' @export
tidy.happy_extended = function(obj) {
    df = plyr::ldply(obj, data.frame)
    dt = data.table::data.table(df)
    return(dt)
}

#' add_credible_intervals
#' 
#' Add credible intervals to the observed performance estimates using a 
#' beta-binomial model. The uncertainty measurement takes into account both the total number of events in each subset
#' (e.g. `TRUTH.TOTAL`) and the variability across replicates.
#' 
#' @param obj A `happy_extended` object.
#' @return A `happy_extended_ci` object.
#' @export
add_credible_intervals.happy_extended = function(obj, metric, sig = 0.05, samplesize = 1e6) {
    ## validate input
    supported_metrics = c('METRIC.Recall')
    if (!metric %in% supported_metrics) {
        stop("Metric not supported, see docs.")
    }
    
    ## add credible intervals
    if (metric == 'METRIC.Recall') {
        
        input_data = tidy(obj) %>%
            filter(Subset.Level == 2) %>%
            select(Type, Filter, Group.Id, Subset, Sample.Id, Replicate.Id, TRUTH.TP, TRUTH.TOTAL)
        
        n_zeros = input_data %>%
            filter(TRUTH.TOTAL == 0) %>%
            select(Subset) %>%
            n_distinct()
        if (n_zeros > 0) {
            warning(sprintf("Discarding %d subsets where TRUTH.TOTAL == 0.", n_zeros))
            input_data = input_data %>%
                filter(TRUTH.TOTAL > 0)
        }
        
        credible_intervals = input_data %>%
            group_by(Type, Filter, Group.Id, Subset) %>%
            do(data.frame(.add_ci(successes = .$TRUTH.TP, totals = .$TRUTH.TOTAL, sig = sig, samplesize = samplesize))) %>%
            rename_(.dots = setNames(names(.), 
                                     c(names(.)[1:4],
                                       paste(metric, names(.)[-c(1:4)], sep = '.')))) %>%
            ungroup() %>%
            data.table::data.table()
    }
    
    class(credible_intervals) = append(class(credible_intervals), "happy_extended_ci", after = 0)
    return(credible_intervals)
}

.posterior_ci = function(samplesize, sig, alpha1, beta1) {
    theta = rbeta(samplesize, alpha1, beta1)
    ci = quantile(theta, c(sig/2, 0.5, 1 - sig/2), na.rm = T) %>%
        t() %>%
        data.table::data.table()
    colnames(ci) = c('low', 'theta1', 'high')
    return(ci)
}

.add_ci = function(successes, totals, sig, samplesize) {
    
    ## validate input
    if (any(totals == 0)) {
        stop("Cannot compute empirical Bayes estimates over zero trials.")
    }
    
    ## estimate parameters for beta prior
    v = var(successes / totals)
    mu = mean(successes / totals)
    if (!is.na(v) && v < mu * (1 - mu)) {
        kappa = (mu * (1 - mu) / v - 1 )
        alpha0 = mu * kappa
        beta0 = (1 - mu) * kappa
    } else {
        alpha0 = 1
        beta0 = 1
    }

    ## calculate credible intervals by sampling from the posterior
    credible_intervals = data.table::data.table(
        successes = successes,
        totals = totals,
        theta0 = successes/totals,
        alpha0 = rep(alpha0, length(totals)),
        beta0 = rep(beta0, length(totals))) %>%
        mutate(alpha1 = successes + alpha0,
               beta1 = totals - successes + beta0,
               replicate = 1:n()) %>%
        group_by_(.dots = names(.)) %>%
        do(data.frame(.posterior_ci(samplesize, sig, .$alpha1, .$beta1))) %>%
        ungroup()
    
    ## combine replicate information
    combined_ci = data.table::data.table(
        successes = mean(credible_intervals$successes),
        totals = mean(credible_intervals$totals),
        theta0 = mean(credible_intervals$successes/credible_intervals$totals),
        alpha0 = NA,
        beta0 = NA,
        alpha1 = NA,
        beta1 = NA,
        replicate = '.combined',
        low = min(credible_intervals$low),
        theta1 = mean(credible_intervals$theta1),
        high = max(credible_intervals$high)
    )
    
    result = rbind(credible_intervals, combined_ci)
    
    return(result)
    
}

