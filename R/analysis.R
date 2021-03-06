## downstream analysis of hap.py results


#' Extract tables from a happy_compare object
#' 
#' Extract tables from a `happy_compare` object and combine into a single
#' `data.frame`. Appends samplesheet metadata to the original happy
#' results.
#' 
#' @param happy_compare A `happy_compare` object.
#' @param table Table of data to extract from each `happy_result` object.
#'   Must be one of: `summary`, `extended`, `pr.all`,
#'   `pr.snp.all`, `pr.snp.pass`, `pr.snp.sel`,
#'   `pr.indel.all`, `pr.indel.pass`, `pr.indel.sel`, `build_metrics`.
#'   
#' @return A `data.frame` with combined information from the selected
#'   `happy_result` and its `samplesheet`.
#'   
#' @examples
#' 
#' \dontrun{
#' summary <- extract_metrics(happy_compare, table = 'summary')
#' roc <- extract_metrics(happy_compare, table = 'pr.snp.pass')
#' build_metrics <- extract_metrics(happy_compare, table = 'build.metrics')
#' }
#' 
#' @export
extract_metrics <- function(happy_compare, table = c("summary", "extended", 
                                                     "pr.all", 
                                                     "pr.snp.all", "pr.snp.pass", "pr.snp.sel",
                                                     "pr.indel.all", "pr.indel.pass", "pr.indel.sel",
                                                     "build.metrics")) {
  
  # validate input
  if (!"happy_compare" %in% class(happy_compare)) {
    stop("Must provide a happy_compare object")
  }
  
  table <- match.arg(table)
  if (table == "build.metrics" && !"build_metrics" %in% names(happy_compare)) {
    stop("No build metrics found in the provided happy_compare object, won't extract")
  }
  
  # extract
  samplesheet <- happy_compare$samplesheet
  if (table != "build.metrics") {
    happy_results <- happyR::extract_results(happy_compare$happy_results, table)
    df <- dplyr::inner_join(samplesheet, happy_results, by = c("happy_prefix" = "from"))
    class(df) <- c(unique(c(class(happy_results), class(df))))
  } else {
    df <- dplyr::inner_join(samplesheet, c(happy_compare$build_metrics) %>% dplyr::bind_rows(), 
                            by = c("happy_prefix" = "from"))
    class(df) <- c(unique(c("build_metrics", class(df))))
  }
  
  return(df)
  
}


#' Estimate highest density intervals (HDIs) from performance counts
#' 
#' Estimate highest density intervals and success rates from hap.py counts using a
#' Binomial model and empirical Bayes. See package docs for details on method
#' implementation.
#' 
#' @param df A `data.frame`. Required columns: `Replicate.Id`, `Subset`, columns specified 
#' in `group_cols` argument.
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
#' @return A `data.frame` with performance counts, model hyperparameters, 
#' success rate and HDI estimates.
#'   
#' @examples
#' 
#' \dontrun{
#' hdi <- estimate_hdi(df, successes_col = 'TRUTH.TP', totals_col = 'TRUTH.TOTAL', 
#'                    group_cols = c('Group.Id', 'Subset', 'Type', 'Subtype'))
#' }
#' 
#' @export
estimate_hdi <- function(df, successes_col, totals_col, group_cols, aggregate_only = TRUE, 
                         significance = 0.05, sample_size = 1e+05, max_alpha1 = 1000) {
  
  # validate input
  if (!"data.frame" %in% class(df)) {
    stop("Must provide a data.frame object")
  }
  if (dim(df)[1] == 0) {
    stop("Input data.frame is empty")
  }
  if (!successes_col %in% colnames(df)) {
    stop(sprintf("Could not find column \"%s\" in input data.frame", successes_col))
  }
  if (!totals_col %in% colnames(df)) {
    stop(sprintf("Could not find column \"%s\" in input data.frame", totals_col))
  }
  required_cols <- c("Replicate.Id", "Subset", group_cols)
  if (!all(required_cols %in% colnames(df))) {
    stop(sprintf("Missing one or more required columns from input data.frame: %s", paste(required_cols, collapse = ", ")))
  }
  
  # check that all replicates have the same subsets
  n_subsets_expected <- length(unique(df$Subset)) * length(unique(df$Replicate.Id))
  n_subsets_seen <- dim(unique(df[,c("Replicate.Id", "Subset")]))[1]
  if (n_subsets_seen != n_subsets_expected) {
    warning("Subsets in input.data frame are not consistent across replicates")
  }
  
  # exclude records where totals == 0
  nonzero <- df[totals_col] > 0
  n_exclude <- sum(!nonzero)
  if (n_exclude > 0) {
    warning(sprintf("Excluding %d records where \"%s\" == 0", n_exclude, totals_col))
    df <- df[nonzero,]
  }
  
  # add hdi estimates: call .estimate_hdi() for each user-defined group in df
  do_dots <- lazyeval::interp(
    ~.estimate_hdi(successes = .$successes_col, totals = .$totals_col, 
                   significance = significance, sample_size = sample_size, 
                   max_alpha1 = max_alpha1, aggregate_only = aggregate_only), 
    .values = list(successes_col = successes_col, totals_col = totals_col)
  )
  
  estimates <- df %>% ungroup() %>% 
    group_by_(.dots = lapply(group_cols, as.symbol)) %>% 
    do_(.dots = do_dots) %>% ungroup()
  
  class(estimates) <- c("happy_hdi", class(estimates))
  
  return(estimates)
}

.estimate_hdi <- function(successes, totals, significance = 0.05, sample_size = 1e+05, 
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
  per_replicate_estimates <- function(successes, totals, significance = 0.05, sample_size = 1e+05, 
    aggregate_only = TRUE) {
    
    # estimate parameters for beta prior (subset-specific)
    mu <- mean(successes/totals)
    sigma <- sd(successes/totals)
    if (!is.na(sigma) && sigma > 0) {
      alpha0 <- mu/sigma
      beta0 <- (1 - mu)/sigma
    } else {
      # jeffrey's prior
      alpha0 <- 0.5
      beta0 <- 0.5
    }
    
    # estimate parameters for beta posterior for each replicate separately
    replicate_params <- lapply(seq_along(totals), function(i) {
      
      # counts for current replicate
      s <- successes[i]
      t <- totals[i]
      
      # calculate posterior beta params
      alpha1 <- s + alpha0
      beta1 <- t - s + beta0
      
      # calculate hdis and estimated success rate
      if (!aggregate_only) {
        hdi <- adjusted_hdi(alpha = alpha1, beta = beta1, sample_size = sample_size, 
          significance = significance, s = s, t = t)
      } else {
        hdi <- rep(NA, 3)
        names(hdi) <- c("lower", "upper", "estimated_p")
      }
      
      # format and return
      r <- data.frame(replicate_id = paste0("replicate_", i), successes = s, 
        totals = t, mu = mu, sigma = sigma, alpha0 = alpha0, beta0 = beta0, 
        alpha1 = alpha1, beta1 = beta1, lower = hdi["lower"], observed_p = s/t, 
        estimated_p = hdi["estimated_p"], upper = hdi["upper"], hdi_range = hdi["upper"] - 
          hdi["lower"])
      r$replicate_id <- as.character(r$replicate_id)
      r
      
    }) %>% dplyr::bind_rows()
    
    return(replicate_params)
    
  }
  
  aggregate_estimates <- function(replicate_params, significance = 0.05, sample_size = 1e+05, 
    max_alpha1 = 1000) {
    
    # params for beta prior are subset specific and remain constant across replicates
    alpha0 <- unique(replicate_params$alpha0)
    beta0 <- unique(replicate_params$beta0)
    
    # mu and sigma are also subset specific
    mu <- unique(replicate_params$mu)
    sigma <- unique(replicate_params$sigma)
    
    # counts for aggregate replicate
    s <- mean(replicate_params$successes)
    t <- mean(replicate_params$totals)
    
    # average posterior parameters across replicates
    alpha1 <- aggregate_hyperparam(x = replicate_params$alpha1)
    beta1 <- aggregate_hyperparam(x = replicate_params$beta1)
    
    # limit alpha1 and scale beta1 accordingly
    if (alpha1 > max_alpha1) {
      beta1 <- beta1 * max_alpha1/alpha1
      alpha1 <- max_alpha1
    }
    
    # calculate hdis and estimated success rate
    aggregate_hdi <- adjusted_hdi(alpha = alpha1, beta = beta1, sample_size = sample_size, 
      significance = significance, s = s, t = t)
    
    # format data
    aggregate_params <- data.frame(replicate_id = ".aggregate", successes = s, 
      totals = t, mu = mu, sigma = sigma, alpha0 = alpha0, beta0 = beta0, alpha1 = alpha1, 
      beta1 = beta1, lower = aggregate_hdi["lower"], observed_p = s/t, estimated_p = aggregate_hdi["estimated_p"], 
      upper = aggregate_hdi["upper"], hdi_range = aggregate_hdi["upper"] - 
        aggregate_hdi["lower"])
    aggregate_params$replicate_id <- as.character(aggregate_params$replicate_id)
    
    return(aggregate_params)
  }
  
  adjusted_hdi <- function(alpha, beta, s, t, sample_size = 1e+05, significance = 0.05) {
    
    # calculate hdis by random sampling
    sample <- rbeta(sample_size, alpha, beta)
    hdi <- HDInterval::hdi(sample, credMass = 1 - significance)
    
    # add estimated success rate
    hdi["estimated_p"] <- alpha/(alpha + beta)
    
    # adjust edge cases
    if (s <= 1) {
      hdi["lower"] <- 0
      hdi["upper"] <- max(hdi["upper"], 1 - (significance/2)^(1/t))
    }
    if (s >= (t - 1)) {
      hdi["upper"] <- 1
      hdi["lower"] <- min(hdi["lower"], (significance/2)^(1/t))
    }
    hdi["lower"] <- max(hdi["lower"], 0)
    hdi["upper"] <- min(hdi["upper"], 1)
    
    return(hdi)
  }
  
  aggregate_hyperparam <- function(x) {
    
    # Gamma-weighted mean across replicates
    gamma_probs <- dgamma(x, 0.01, 0.01)
    if (all(gamma_probs == 0)) {
      aggregate_hyperparam <- mean(x)
    } else {
      aggregate_hyperparam <- weighted.mean(x = x, w = dgamma(x, 0.01, 0.01)/max(dgamma(x, 
        0.01, 0.01)))
    }
    return(aggregate_hyperparam)
  }
  
  # main
  replicate_params <- per_replicate_estimates(successes, totals, significance, 
    sample_size, aggregate_only)
  aggregate_params <- aggregate_estimates(replicate_params, significance, sample_size, 
    max_alpha1)
  r <- rbind(replicate_params, aggregate_params)
  rownames(r) <- NULL
  return(r)
  
}


#' Compare subset-specific Beta distributions across two groups
#' 
#' For each subset, calculate the difference in Beta distributions across two groups, and label it as significant if 
#' we can confidently claim that the HDI of the difference is other than zero. Do not attempt tests
#' in subsets for which we do not have a good estimate of the success rate to start with (HDI range = 1).
#' 
#' @param happy_hdi A `happy_hdi` object obtained with `estimate_hdi()`.
#' @param sample_size Number of observations to draw from the Beta posterior 
#' to estimate HDIs for the difference in Beta distributions. Default: 1e5.
#' @param significance Significance levels for HDIs of the difference. Default: 0.05 (= 95\% HDIs).
#'   
#' @return A `data.frame` with estimated differences across groups, HDIs for the difference 
#' and significance labels.
#'   
#' @examples
#' 
#' \dontrun{
#' hdi <- estimate_hdi(df, successes_col = 'TRUTH.TP', totals_col = 'TRUTH.TOTAL', 
#'                    group_cols = c('Group.Id', 'Subset', 'Type', 'Subtype'))
#' hdi_diff <- compare_hdi(happy_hdi = hdi)                 
#' }
#' 
#' @export
compare_hdi <- function(happy_hdi, sample_size = 1e+05, significance = 0.05) {
  
  # validate input
  if (!"happy_hdi" %in% class(happy_hdi)) {
    stop("Must provide a happy_hdi object - see happyCompare::estimate_hdi")
  }
  if (length(unique(happy_hdi$Group.Id)) != 2) {
    stop("Will only perform compare HDIs across two groups")
  }
  
  # reformat input data
  df <- happy_hdi %>% 
    filter(replicate_id == ".aggregate") %>% 
    mutate(Subset = paste(Type, Subset, sep=":")) %>% 
    select(Group.Id, Subset, alpha1, beta1, hdi_range) %>% 
    tidyr::gather(variable, value, -(Group.Id:Subset)) %>% 
    tidyr::unite(.variable, variable, Group.Id, sep = ".") %>% 
    tidyr::spread(.variable, value)
  
  # calculate difference
  hdi_diff <- df %>% 
    group_by(Subset) %>%
    do(.compare_hdi(alpha_a = .[[2]], beta_a = .[[4]],
                 alpha_b = .[[3]], beta_b = .[[5]],
                 sample_size = sample_size, significance = significance)) %>% 
    arrange(-significant, -estimated_diff) %>% 
    separate(Subset, into = c("Type", "Subset"), sep = ":")

  return(hdi_diff)
  
}


.compare_hdi <- function(alpha_a, beta_a, alpha_b, beta_b, range_a = NA, range_b = NA,
                         sample_size = 1e5, significance = 0.05) {
  
  if (!is.na(range_a) && !is.na(range_b) && (range_a == 1 || range_b == 1)) {
    # cannot make an assessment
    df <- data.frame(
      estimated_diff = NA,
      lower = NA,
      upper = NA,
      significant = NA
    )
    
  } else {
    # evaluate difference in beta distributions
    sample_a <- stats::rbeta(sample_size, alpha_a, beta_a)
    sample_b <- stats::rbeta(sample_size, alpha_b, beta_b)
    diff <- sample_a - sample_b
    hdi <-HDInterval::hdi(diff, credMass = 1 - significance)
    
    df <- data.frame(
      estimated_diff = mean(diff),
      lower = hdi["lower"],
      upper = hdi["upper"],
      significant = ifelse(hdi["lower"] <= 0 && hdi["upper"] >= 0, FALSE, TRUE)
    )
  }

  rownames(df) = NULL

  # warn about skipped records
  if (any(is.na(df$significant))) {
    warning(sprintf("Skipped %d record(s) with HDI range == 1", length(df$significant)))
  }
  
  return(df)
}

