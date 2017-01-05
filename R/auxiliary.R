#' load_happy_metrics
#'
#' Load happy csv output into a `data.frame`.
#'
#' @param config_path Path to configuration file. Required columns:
#' group (group name), id (results id),
#' happy_csv (path to happy *.summary.csv or *.extended.csv files).
#' @param L1_subsets A `vector` containing subsets to be classified as level 1.
#' @return A combined `data.frame`.
#' @examples
#' \dontrun{load_happy_metrics(config_path = config_path)}
#' @export

load_happy_metrics <- function(config_path, L1_subsets = NULL) {
    ## functions
    .calculate_frac_assessed = function(metrics_df) {
        # METRIC.Frac_Assessed
        metrics_df$METRIC.Frac_Assessed = 1 - metrics_df$METRIC.Frac_NA
        
        # METRIC.Frac_Assessed.Upper
        upper = metrics_df$METRIC.Frac_Assessed + (metrics_df$METRIC.Frac_NA.Upper - metrics_df$METRIC.Frac_NA)
        upper[upper > 1] = 1
        metrics_df$METRIC.Frac_Assessed.Upper = upper
        
        # METRIC.Frac_Assessed.Lower
        lower = metrics_df$METRIC.Frac_Assessed - (metrics_df$METRIC.Frac_NA - metrics_df$METRIC.Frac_NA.Lower)
        lower[lower < 0] = 0
        metrics_df$METRIC.Frac_Assessed.Lower = lower
        
        return(metrics_df)
    }
    
    ## main
    if (is.null(L1_subsets)) {
        stop("Must provide a list of subsets to be classified as level 1.")
    }
    
    if (!file.exists(config_path)) {
        stop("Invalid config path.")
    }
    
    config = data.table::fread(config_path)
    
    required_cols = c('group', 'id', 'happy_csv')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    known_filetype = FALSE
    if (all(grepl('summary.csv', config$happy_csv))) {
        message('Loading summary.csv metrics...')
        known_filetype = TRUE
    }
    if (all(grepl('extended.csv', config$happy_csv))) {
        message('Loading extended.csv metrics...')
        known_filetype = TRUE
    }
    if (!known_filetype) {
        stop('Unknown happy csv file. Supported files are summary.csv and extended.csv.')
    }
    
    metrics_list = lapply(seq_along(config$happy_csv), function(i) {
        this_path = config$happy_csv[i]
        message(sprintf('Reading %s', this_path))
        this_metrics = data.table::fread(this_path)
        
        # add metadata
        this_metrics$Group = rep(as.character(config$group[i]), dim(this_metrics)[1])
        this_metrics$Id = rep(as.character(config$id[i]), dim(this_metrics)[1])
        
        # add Subset_Level (to be deprecated soon)
        # 1) initialise all subset levels to the lowest, i.e. 2
        this_metrics$Subset.Level = rep(2, dim(this_metrics)[1])
        # 2) over-write * to the highest level, i.e. 0
        this_metrics[this_metrics$Subset == '*', ]$Subset.Level = '0'
        # 3) over-write level 1 subsets based on the user-provided list
        this_metrics[this_metrics$Subset %in% L1_subsets, ]$Subset.Level = '1'
        
        return(this_metrics)
    })
    metrics_df = plyr::ldply(metrics_list, data.frame)
    
    # add METRIC.Frac_Assessed
    metrics_df = .calculate_frac_assessed(metrics_df)
    
    return(metrics_df)
}


#' calculate_average_performance
#'
#' Calculate summary stats for the specified metrics.
#'
#' @param happy_metrics A `data.frame` with happy metrics obtained with `load_happy_metrics()`.
#' @param metrics A `vector` with the metrics to consider.
#' @param filter Variant filter. Default = PASS.
#' @return A summarised `data.frame`.
#' @examples
#' \dontrun{calculate_average_performance(happy_metrics)}
#' @export

calculate_average_performance <- function(happy_metrics,
                                          metrics=c("METRIC.Recall", "METRIC.Precision",
                                                    "METRIC.Frac_Assessed", "METRIC.F1_Score"),
                                          filter='PASS') {

    ## checks    
    if (dim(happy_metrics)[1] == 0) {
        stop("Input dataframe is empty.")
    }
    if (!filter %in% unique(happy_metrics)$Filter) {
        stop("Invalid filter.")
    }
    
    
    ## prepare data
    constant_cols = c("Group", "Id", "Type", "Subset", "Subset.Level", 
                      "Subset.Size", "TRUTH.TOTAL")
    metrics_subset = happy_metrics[happy_metrics$Filter == filter &
                                       happy_metrics$Genotype == "*" &
                                       happy_metrics$Subtype == "*",
                                   c(constant_cols, metrics)]
    metrics_subset$.id = paste(metrics_subset$Group,
                               metrics_subset$Type,
                               metrics_subset$Subset)
    
    # constant values
    metrics_subset_1 = unique(metrics_subset[, replace(constant_cols, constant_cols == 'Id', '.id')])
    
    # summarise performance across group replicates for each metric
    metrics_subset_2 = lapply(seq_along(metrics), function(i) {
        y = metrics[i]
        
        metrics_subset_2 = .summary(metrics_subset, measurevar = y, groupvars = ".id")
        metrics_subset_2$ymin = metrics_subset_2[, y] - metrics_subset_2[, 'se']
        metrics_subset_2$ymax = metrics_subset_2[, y] + metrics_subset_2[, 'se']
        
        colnames(metrics_subset_2)[3] = paste0(y, ".mean")
        colnames(metrics_subset_2)[4:dim(metrics_subset_2)[2]] = paste0(y, ".", colnames(metrics_subset_2)[4:dim(metrics_subset_2)[2]])
        
        if (i == 1) {
            return(metrics_subset_2)
        } else {
            return(metrics_subset_2[,grep("METRIC", colnames(metrics_subset_2))])
        }
    })
    metrics_subset_2 = do.call(cbind, metrics_subset_2)
    
    # merge
    metrics_subset = merge(metrics_subset_1, metrics_subset_2, by = ".id")
    metrics_subset$Group = factor(metrics_subset$Group)
    metrics_subset$Type = factor(metrics_subset$Type, levels = sort(unique(metrics_subset$Type), decreasing = T))
    metrics_subset$Subset = factor(metrics_subset$Subset, levels = sort(unique(metrics_subset$Subset), decreasing = T))
    
    return(metrics_subset)
}


#' .summary
#'
#' Calculate summary stats for a numerical column in a `data.frame`.
#' Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
#' Adapted from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)
#' @param data: A `data frame`.
#' @param measurevar: The name of a column that contains the variable to be summariezed
#' @param groupvars: A vector containing names of columns that contain grouping variables
#' @param na.rm: A boolean that indicates whether to ignore NA's
#' @param conf.interval: The percent range of the confidence interval (default is 95%)
#' @return A combined `data.frame`.
#' @examples
#' \dontrun{.summary(data, measurevar = y, groupvars = ".id")}
#' @export

.summary = function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function(x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }

  # For each group's data frame, return a vector with N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop = .drop,
                 .fun = function(xx, col) {
                   c(N = length2(xx[[col]], na.rm = na.rm),
                     mean = mean(xx[[col]], na.rm = na.rm),
                     sd = sd(xx[[col]], na.rm = na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- plyr::rename(datac, c("mean" = measurevar))

  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
  datac$ci <- datac$se * ciMult

  return(datac)
}
