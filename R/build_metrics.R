## build_metrics methods

#' is_build_metrics
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_build_metrics = function(x) {
    inherits(x, "build_metrics")
}

#' print.build_metrics
#' 
#' Print a build_metrics object.
#' 
#' @param x A `build_metrics` object.
#' @param ... Extra arguments.
#' @export
print.build_metrics = function(x, ...) {
    print(lapply(x, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `build_metrics` object by converting into a single `data.table`.
#' 
#' @param x A `build_metrics` object.
#' @param metrics A vector with the metric names to extract.
#' @param ... Extra arguments.
#' @export
tidy.build_metrics = function(x, metrics, ...) {
    
    ## validate input
    replicates_have_metrics = sapply(x, function(y) all(metrics %in% colnames(y)))
    if (!all(replicates_have_metrics)) {
        stop(sprintf('Requested metrics not found for the following replicates: %s', 
                     paste(names(replicates_have_metrics[!replicates_have_metrics]), collapse = ', ')
                     ))
    }
    
    ## subset columns and tidy 
    cols_to_keep = c('Group.Id', 'Sample.Id', 'Replicate.Id', metrics)
    tidy = lapply(x, function(y) {
        y %>% select(match(cols_to_keep, names(.))) 
    }) %>% bind_rows()
    
    return(tidy)
    
}

#' rename
#' 
#' Rename metric names in a `build_metrics` object.
#' 
#' @param x A `build_metrics` object.
#' @param metrics_map A `data.frame` or `data.table`describing the mapping
#'   between metric names. Required columns: old_name, new_name.
#' @param ... Extra arguments.
#' @export
rename.build_metrics = function(x, metrics_map, ...) {
    
    ## validate input
    required_cols = c('old_name', 'new_name')
    if (!all(required_cols %in% colnames(metrics_map))) {
        stop('Missing required columns in metrics map.')
    }
    
    ## rename
    mm = c(metrics_map$new_name)
    names(mm) = metrics_map$old_name
    renamed_x = lapply(x, function(y) {
        plyr::rename(y, replace = mm)
    })
    class(renamed_x) = append(class(renamed_x), "build_metrics", after = 0)
    return(renamed_x)
    
}