## data input

#' load_data
#'
#' Load csv inputs.
#'
#' @param config_path Path to configuration file. Required columns:
#' group_id, sample_id, replicate_id.
#' 
#' @return A `haplocompare` object.
#' @export
load_data = function(config_path) {
    
    ## validate input
    if (!file.exists(config_path)) {
        stop("Cannot find config path.")
    }
    
    config = data.table::fread(config_path)
    
    required_cols = c('group_id', 'sample_id', 'replicate_id')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    ## happy_summary
    happy_summary = NULL
    if ('happy_summary' %in% colnames(config)) {
        if (any(is.na(config$happy_summary))) {
            warning("NAs found among happy summary paths, will not load any data.")
        } else {
            if (!all(grepl('summary.csv', config$happy_summary))) {
                stop("happy summary file does not match the expected *.summary.csv")
            } else {
                message("Loading summary.csv metrics...")
                sconfig = config %>% select(group_id, sample_id, replicate_id, happy_summary)
                happy_summary = .load_happy_summary(config = sconfig)
            }
        }
    }

    ## happy_extended
    happy_extended = NULL
    if ('happy_extended' %in% colnames(config)) {
        if (any(is.na(config$happy_extended))) {
            warning("NAs found among happy extended paths, will not load any data.")
        } else {
            if (!all(grepl('extended.csv', config$happy_extended))) {
                stop("happy extended file does not match the expected *.extended.csv")
            } else {
                message("Loading extended.csv metrics...")
                sconfig = config %>% select(group_id, sample_id, replicate_id, happy_extended)
                happy_extended = .load_happy_extended(config = sconfig)
            }
        }
    }
    
    ## sompy_stats
    sompy_stats = NULL
    if ('sompy_stats' %in% colnames(config)) {
        if (any(is.na(config$sompy_stats))) {
            warning("NAs found among sompy stats paths, will not load any data.")
        } else {
            if (!all(grepl('stats.csv', config$sompy_stats))) {
                stop("sompy stats file does not match the expected *.stats.csv")
            } else {
                message("Loading stats.csv metrics...")
                sconfig = config %>% select(group_id, sample_id, replicate_id, sompy_stats)
                sompy_stats = .load_sompy_stats(config = sconfig)
            }
        }
    }
    
    ## build_metrics
    build_metrics = NULL
    if ('build_metrics' %in% colnames(config)) {
        if (any(is.na(config$build_metrics))) {
            warning("NAs found among happy summary paths, will not load any data.")
        } else {
            if (!all(grepl('csv', config$build_metrics))) {
                stop("build metrics file does not match the expected *.csv")
            } else {
                message("Loading build metrics...")
                sconfig = config %>% select(group_id, sample_id, replicate_id, build_metrics)
                build_metrics = .load_build_metrics(config = sconfig)
            }
        }
    }
    
    ## return
    data = list(
        config = config,
        happy_summary = happy_summary,
        happy_extended = happy_extended,
        sompy_stats = sompy_stats,
        build_metrics = build_metrics
    )
    class(data) = append(class(data), "haplocompare", after = 0)
    return(data)
    
}

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
        this_metrics$Group.Id = rep(as.character(config$group_id[i]), dim(this_metrics)[1])
        this_metrics$Sample.Id = rep(as.character(config$sample_id[i]), dim(this_metrics)[1])
        this_metrics$Replicate.Id = rep(as.character(config$replicate_id[i]), dim(this_metrics)[1])
        return(this_metrics)
    })
    names(data) = sapply(seq_along(config$happy_extended), function(i) {
        paste(config$group_id[i], config$sample_id[i], config$replicate_id[i], sep = '-')
    })
    class(data) = append(class(data), "happy_summary", after = 0)
    return(data)
    
}

.load_happy_extended = function(config) {
    
    ## validate input
    required_cols = c('group_id', 'sample_id', 'replicate_id', 'happy_extended')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    ## load data
    data = lapply(seq_along(config$happy_extended), function(i) {
        this_path = config$happy_extended[i]
        message(sprintf('Reading %s', this_path))
        this_metrics = data.table::fread(this_path)
        this_metrics$Group.Id = rep(as.character(config$group_id[i]), dim(this_metrics)[1])
        this_metrics$Sample.Id = rep(as.character(config$sample_id[i]), dim(this_metrics)[1])
        this_metrics$Replicate.Id = rep(as.character(config$replicate_id[i]), dim(this_metrics)[1])
        
        # temporary: L0 = first row, L1 = second row, L2 = all the rest
        # will be provided by happy in the future
        subset_level_0 = head(this_metrics$Subset, n = 2)[1]
        subset_level_1 = head(this_metrics$Subset, n = 2)[2]
        this_metrics$Subset.Level = rep(2, dim(this_metrics)[1])
        this_metrics[this_metrics$Subset == subset_level_0,]$Subset.Level = 0
        this_metrics[this_metrics$Subset == subset_level_1,]$Subset.Level = 1
        
        return(this_metrics)
    })
    names(data) = sapply(seq_along(config$happy_extended), function(i) {
        paste(config$group_id[i], config$sample_id[i], config$replicate_id[i], sep = '-')
    })
    class(data) = append(class(data), "happy_extended", after = 0)
    return(data)
    
}

.load_sompy_stats = function(config) {
    
    ## validate input
    required_cols = c('group_id', 'sample_id', 'replicate_id', 'sompy_stats')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    ## load data
    data = lapply(seq_along(config$sompy_stats), function(i) {
        this_path = config$sompy_stats[i]
        message(sprintf('Reading %s', this_path))
        this_metrics = data.table::fread(this_path)
        this_metrics$Group.Id = rep(as.character(config$group_id[i]), dim(this_metrics)[1])
        this_metrics$Sample.Id = rep(as.character(config$sample_id[i]), dim(this_metrics)[1])
        this_metrics$Replicate.Id = rep(as.character(config$replicate_id[i]), dim(this_metrics)[1])
        
        return(this_metrics)
    })
    names(data) = sapply(seq_along(config$sompy_stats), function(i) {
        paste(config$group_id[i], config$sample_id[i], config$replicate_id[i], sep = '-')
    })
    class(data) = append(class(data), "sompy_stats", after = 0)
    return(data)
    
}

.load_build_metrics = function(config) {
    
    ## validate input
    required_cols = c('group_id', 'sample_id', 'replicate_id', 'build_metrics')
    if (!all(required_cols %in% colnames(config))) {
        stop("Missing required columns in input config, see docs.")
    }
    
    ## load data
    data = lapply(seq_along(config$build_metrics), function(i) {
        this_path = config$build_metrics[i]
        message(sprintf('Reading %s', this_path))
        this_metrics = data.table::fread(this_path)
        this_metrics$Group.Id = rep(as.character(config$group_id[i]), dim(this_metrics)[1])
        this_metrics$Sample.Id = rep(as.character(config$sample_id[i]), dim(this_metrics)[1])
        this_metrics$Replicate.Id = rep(as.character(config$replicate_id[i]), dim(this_metrics)[1])
        return(this_metrics)
    })
    names(data) = sapply(seq_along(config$build_metrics), function(i) {
        paste(config$group_id[i], config$sample_id[i], config$replicate_id[i], sep = '-')
    })
    class(data) = append(class(data), "build_metrics", after = 0)
    return(data)
    
}