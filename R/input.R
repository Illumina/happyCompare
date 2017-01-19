## data input

#' load_data
#'
#' Load csv inputs.
#'
#' @param config_path Path to configuration file. Required columns:
#' group_id, sample_id, happy_summary, happy_extended.
#' 
#' @return A `happy_compare` object.
#' @examples
#' \dontrun{load_data(config_path = config_path)}
#' @export
load_data = function(config_path) {
    
    ## validate input
    if (!file.exists(config_path)) {
        stop("Cannot find config path.")
    }
    
    config = data.table::fread(config_path)
    
    required_cols = c('group_id', 'sample_id', 'replicate_id', 'happy_summary', 'happy_extended')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    ## happy_summary
    if (any(is.na(config$happy_summary))) {
        warning("NAs found among happy summary paths, will not load any data.")
        happy_summary = NULL
    } else {
        if (!all(grepl('summary.csv', config$happy_summary))) {
            stop("happy summary file does not match the expected *.summary.csv")
        } else {
            message("Loading summary.csv metrics...")
            sconfig = config %>% select(group_id, sample_id, replicate_id, happy_summary)
            happy_summary = .load_happy_summary(config = sconfig)
        }
    }

    ## happy_extended
    # TODO: implement
    happy_extended = NULL
    
    ## return
    data = list(
        config = config,
        happy_summary = happy_summary,
        happy_extended = happy_extended
    )
    class(data) = append(class(data), "happy_compare", after = 0)
    return(data)
    
}

#' .load_happy_summary
#'
#' Load happy summary.csv files.
.load_happy_summary = function(config) {
    
    ## validate input
    required_cols = c('group_id', 'sample_id', 'replicate_id', 'happy_summary')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    ## load data
    data = lapply(seq_along(config$happy_summary), function(i) {
        this_path = config$happy_summary[i]
        message(sprintf('Reading %s', this_path))
        this_metrics = data.table::fread(this_path)
        this_metrics$Group = rep(as.character(config$group_id[i]), dim(this_metrics)[1])
        this_metrics$Sample.Id = rep(as.character(config$sample_id[i]), dim(this_metrics)[1])
        this_metrics$Replicate.Id = rep(as.character(config$replicate_id[i]), dim(this_metrics)[1])
        return(this_metrics)
    })
    names(data) = paste(config$group_id, config$sample_id, config$replicate_id, collapse = '-')
    class(data) = append(class(data), "happy_summary", after = 0)
    return(data)
    
}