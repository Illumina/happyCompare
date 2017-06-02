## extract methods


#' Extract tables from a happyCompare_list object
#' 
#' Extract tables from a `happyCompare_list object` and combine into a single
#' \code{data.frame}. Appends samplesheet metadata to the original happy
#' results.
#' 
#' @param happyCompare_list A \code{happyCompare_list} object.
#' @param table Table of data to extract from each \code{happy_result} object.
#'   Must be one of: \code{summary}, \code{extended}, \code{pr_curve.all},
#'   \code{pr_curve.SNP},  \code{pr_curve.SNP_PASS}, \code{pr_curve.SNP_SEL},
#'   \code{pr_curve.INDEL},  \code{pr_curve.INDEL_PASS},
#'   \code{pr_curve.INDEL_SEL}.
#'   
#' @return A \code{data.frame} with combined information from the selected
#'   \code{happy_result} and its \code{samplesheet}.
#'   
#' @examples
#' 
#' \dontrun{
#' summary = extract(happyCompare_list, table = "summary")
#' roc = extract(happyCompare_list, table = "pr_curve.all")
#' }
#' 
#' @export
extract = function(happyCompare_list, ...) {
  UseMethod("extract", happyCompare_list)
}
#' @export
extract.happyCompare_list <- function(happyCompare_list, 
                                      table = c("summary", "extended", "pr_curve.all", 
                                                "pr_curve.SNP", "pr_curve.SNP_PASS", "pr_curve.SNP_SEL",
                                                "pr_curve.INDEL", "pr_curve.INDEL_PASS", "pr_curve.INDEL_SEL")) {
  
  # validate input
  if (!"happyCompare_list" %in% class(happyCompare_list)) {
    stop("Must provide a happyCompare_list object")
  }
  
  table = match.arg(table)
  
  # extract results into a data.frame
  ids = happyCompare_list$samplesheet$.Id
  
  if (table %in% c("summary", "extended")) {
    item_list = lapply(ids, function(id) {
      if (!table %in% names(happyCompare_list$happy_results[[id]])) {
        stop(sprintf("Could not find %s for happy_results_list with id %s", table, id))
      }
      table_out = merge(
        happyCompare_list$samplesheet %>% 
          filter(.Id == id),
        happyCompare_list$happy_results[[id]][[table]] %>% 
          data.frame() %>% 
          mutate(.Id = id),
        by = ".Id"
      )
      table_out
    })
    df <- dplyr::bind_rows(item_list)
  }
  
  if (grepl("pr_curve", table)) {
    item_list = lapply(ids, function(id) {
      table_split = unlist(strsplit(table, "\\."))
      if (!table_split[1] %in% names(happyCompare_list$happy_results[[id]])) {
        stop(sprintf("Could not find %s for happy_results_list with id %s", table_split[1], id))
      }
      table_out = merge(
        happyCompare_list$samplesheet %>% 
          filter(.Id == id),
        happyCompare_list$happy_results[[id]][[table_split[1]]][[table_split[2]]] %>% 
          data.frame() %>% 
          mutate(.Id = id),
        by = ".Id"
      )
      table_out  
    })
    df <- dplyr::bind_rows(item_list)
  }
  
  # set class
  if (table == "summary") {
    class(df) <- c("happy_summary", class(df))
  }
  if (table == "extended") {
    class(df) <- c("happy_extended", class(df))
  }
  if (grepl("pr_curve", table)) {
    class(df) <- c("happy_roc", class(df))
  }  
  
  return(df)
  
}


#' Estimate HDIs using Beta-Binomial inference
#' 
#' Estimate HDIs and success rates from hap.py counts using a Beta-Binomial 
#' model and empirical Bayes
#' 
#' @param happy_extended A \code{happy_extended} object.
#' @param successes_col Name of the column that contains success counts. 
#' @param totals_col Name of the column that contains total counts.
#' @param group_cols Vector of columns to group counts by. Observations 
#' within the same group will be treated as replicates.
#' @param significance Significance for HDI estimation. Default: 0.05 (= 95\% HDIs).
#' @param sample_size Number of observations to draw from the Beta posterior 
#' to estimate HDIs. Default: 1e5.
#' @param max_alpha1 Upper bound for alpha hyperparameter in the aggregate Beta posterior.
#' @param aggregate_only Estimate HDIs for aggregate replicate only 
#' (speeds up execution). Default: TRUE.
#'   
#' @return A \code{data.frame} with performance counts, model hyperparameters, 
#' success rate and HDI estimates.
#'   
#' @examples
#' 
#' \dontrun{
#' hdi = estimate_hdi(happy_extended, successes_col = "TRUTH.TP", totals_col = "TRUTH.TOTAL", 
#'                    group_cols = c("Group.Id", "Subset", "Type", "Subtype"))
#' }
#' 
#' @export
estimate_hdi = function(happy_extended, ...) {
  UseMethod("estimate_hdi", happy_extended)
}
#' @export
estimate_hdi = function(happy_extended, successes_col, totals_col, group_cols, aggregate_only = TRUE,
                        significance = 0.05, sample_size = 1e5, max_alpha1 = 1000) {

  # validate input
  if (class(happy_extended)[1] != "happy_extended") {
    stop("Must provide a happy_extended object")
  }
  if (dim(happy_extended)[1] == 0) {
    stop("Input data.frame is empty")
  }
  if (!successes_col %in% colnames(happy_extended)) {
    stop(sprintf("Could not find column \"%s\" in input data.frame", successes_col))
  }
  if (!totals_col %in% colnames(happy_extended)) {
    stop(sprintf("Could not find column \"%s\" in input data.frame", totals_col))
  }
  if (!all(group_cols %in% colnames(happy_extended))) {
    stop("Could not find all specified group columns in input data.frame")
  }
  
  # exclude records where totals == 0
  n_exclude = sum(happy_extended[, totals_col] == 0)
  if (n_exclude > 0) {
    warning(sprintf("Excluding %d records where \"%s\" == 0", n_exclude, totals_col))
    happy_extended = happy_extended %>% 
      filter_(.dots = lazyeval::interp(~ .$totals_col > 0, .values = list(totals_col = totals_col)))
  }

  # add hdi estimates
  do_dots = lazyeval::interp(~ .estimate_hdi(successes = .$successes_col, totals = .$totals_col, 
                       significance = significance, sample_size = sample_size, 
                       max_alpha1 = max_alpha1, aggregate_only = aggregate_only), 
                   .values = list(successes_col = successes_col, totals_col = totals_col))
  
  estimates = happy_extended %>% 
    ungroup() %>% 
    group_by_(.dots = lapply(group_cols, as.symbol)) %>% 
    do_(.dots = do_dots) %>% 
    ungroup()
  
  class(estimates) = c("happy_hdi", class(estimates))
  
  return(estimates)
}

.estimate_hdi = function(successes, totals, significance = 0.05, sample_size = 1e5, 
                         max_alpha1 = 1000, aggregate_only = TRUE) {
  
  # validate input
  if (any(totals == 0)) {
    stop("Cannot obtain HDI estimates over zero trials")
  }
  if (length(unique(totals)) > 1) {
    warning("The replicates supplied have different totals - is this expected?")
  }
  if (length(successes) != length(totals)) {
    stop("Number of successes and totals must match")
  }
  
  # internal functions
  per_replicate_estimates = function(successes, totals, significance = 0.05, 
                                     sample_size = 1e5, aggregate_only = TRUE) {
    
    # estimate parameters for beta prior (subset-specific)
    mu = mean(successes / totals)
    sigma = sd(successes / totals)
    if (!is.na(sigma) && sigma > 0) {
      alpha0 = mu / sigma
      beta0 = (1 - mu) / sigma
    } else {
      # jeffrey's prior
      alpha0 = 0.5
      beta0 = 0.5
    }    
    
    # estimate parameters for beta posterior for each replicate separately
    replicate_params = lapply(seq_along(totals), function(i) {
      
      # counts for current replicate
      s = successes[i]
      t = totals[i]
      
      # calculate posterior beta params
      alpha1 = s + alpha0
      beta1 = t - s + beta0
      
      # calculate hdis and estimated success rate
      if (! aggregate_only) {
        hdi = adjusted_hdi(alpha = alpha1, beta = beta1, sample_size = sample_size, 
                           significance = significance, s = s, t = t) 
      } else {
        hdi = rep(NA, 3)
        names(hdi) = c("lower", "upper", "estimated_p")
      }
      
      # format and return
      r = data.frame(
        replicate_id = paste0("replicate_", i),
        successes = s,
        totals = t,
        mu = mu,
        sigma = sigma,
        alpha0 = alpha0,
        beta0 = beta0,
        alpha1 = alpha1,
        beta1 = beta1,
        lower = hdi["lower"],
        observed_p = s / t,      
        estimated_p = hdi["estimated_p"],
        upper = hdi["upper"],
        hdi_range = hdi["upper"] - hdi["lower"]
      )
      r$replicate_id = as.character(r$replicate_id)
      r
      
    }) %>% dplyr::bind_rows()
    
    return(replicate_params)
    
  }
  
  aggregate_estimates = function(replicate_params, significance = 0.05, sample_size = 1e5, 
                                 max_alpha1 = 1000) {
    
    # params for beta prior are subset specific and remain constant across replicates
    alpha0 = unique(replicate_params$alpha0)
    beta0 = unique(replicate_params$beta0)
    
    # mu and sigma are also subset specific
    mu = unique(replicate_params$mu)
    sigma = unique(replicate_params$sigma)
    
    # counts for aggregate replicate
    s = mean(replicate_params$successes)
    t = mean(replicate_params$totals)
    
    # average posterior parameters across replicates
    alpha1 = aggregate_hyperparam(x = replicate_params$alpha1)
    beta1 = aggregate_hyperparam(x = replicate_params$beta1)
    
    # limit alpha1 and scale beta1 accordingly
    if (alpha1 > max_alpha1) {
      beta1 = beta1 * max_alpha1 / alpha1
      alpha1 = max_alpha1
    }
    
    # calculate hdis and estimated success rate
    aggregate_hdi = adjusted_hdi(alpha = alpha1, beta = beta1, sample_size = sample_size, 
                                  significance = significance, s = s, t = t) 
    
    # format data
    aggregate_params = data.frame(
      replicate_id = ".aggregate",
      successes = s,
      totals = t,
      mu = mu,
      sigma = sigma,
      alpha0 = alpha0,
      beta0 = beta0,
      alpha1 = alpha1,
      beta1 = beta1,
      lower = aggregate_hdi["lower"],
      observed_p = s / t,      
      estimated_p = aggregate_hdi["estimated_p"],
      upper = aggregate_hdi["upper"],
      hdi_range = aggregate_hdi["upper"] - aggregate_hdi["lower"]
    )
    aggregate_params$replicate_id = as.character(aggregate_params$replicate_id)
    
    return(aggregate_params)
  }
  
  adjusted_hdi = function(alpha, beta, s, t, sample_size = 1e5, significance = 0.05) {
    
    # calculate hdis by random sampling
    sample = rbeta(sample_size, alpha, beta)
    hdi = HDInterval::hdi(sample, credMass = 1 - significance)
    
    # add estimated success rate
    hdi["estimated_p"] = alpha / (alpha + beta)
    
    # adjust edge cases
    if (s <= 1) {
      hdi["lower"] = 0
      hdi["upper"] = max(hdi["upper"], 1 - (significance/2)^(1/t))
    }
    if (s >= (t - 1)) {
      hdi["upper"] = 1
      hdi["lower"] = min(hdi["lower"], (significance/2)^(1/t))
    }
    hdi["lower"] = max(hdi["lower"], 0)
    hdi["upper"] = min(hdi["upper"], 1)
    
    return(hdi)
  }
  
  aggregate_hyperparam = function(x) {
    
    # Gamma-weighted mean across replicates
    gamma_probs = dgamma(x, 0.01, 0.01)
    if (all(gamma_probs == 0)) {
      aggregate_hyperparam = mean(x)
    } else {
      aggregate_hyperparam = weighted.mean(
        x = x,
        w = dgamma(x, 0.01, 0.01) / max(dgamma(x, 0.01, 0.01))
      )
    }
    return(aggregate_hyperparam)
  }
  
  # main
  replicate_params = per_replicate_estimates(successes, totals, significance, sample_size, aggregate_only)
  aggregate_params = aggregate_estimates(replicate_params, significance, sample_size, max_alpha1)
  r = rbind(replicate_params, aggregate_params)
  rownames(r) = NULL
  return(r)
  
}