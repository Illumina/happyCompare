## happy_summary methods

#' is_happy_summary
#' 
#' Check if the class of the provided object matches the expected one.
#' 
#' @param x An object to inspect.
#' @export
is_happy_summary = function(x) {
    inherits(x, "happy_summary")
}

#' print.happy_summary
#' 
#' Print a happy_summary object.
#' 
#' @param x A `happy_summary` object.
#' @param ... Extra arguments.
#' @export
print.happy_summary = function(x, ...) {
    print(lapply(x, function(x) dplyr::trunc_mat(x)))
}

#' tidy
#'
#' Tidy a `happy_summary` object by converting into a single `data.table`.
#' 
#' @param x A `happy_summary` object.
#' @param ... Extra arguments.
#' @export
tidy.happy_summary = function(x, ...) {
    x %>% dplyr::bind_rows()
}

#' summary.happy_summary
#'
#' Summarise happy summary results into tabular format.
#' 
#' @param object A `happy_summary` object.
#' @param type Variant type. One of: SNP, INDEL.
#' @param filter Variant filter. One of: PASS (default), ALL.
#' @param digits Number of significant digits in summary statistics. Default: 4.
#' @param kable_format A character string to pass to `knitr::kable()`. Default: markdown.
#' @param aggregate Summarise performance across groups. Default: TRUE.
#' @param colnames Column names for output table. Default: happy_summary headers.
#' @param ... Extra arguments.
#' @export
summary.happy_summary = function(object, type, filter = 'PASS', digits = 4,
                                 kable_format = 'markdown', aggregate = TRUE, colnames = NULL, ...) {
    
    ## validate input
    if (missing(type)) {
        stop("Must specify a variant type.")
    }
    
    ## create result table
    caption = paste(filter, type, collapse = '-')
    
    if (aggregate) {
        data = tidy(object) %>%
            filter(Type == type, Filter == filter) %>%
            group_by(Group.Id) %>%
            summarise(
                N = n(),
                METRIC.Precision.mean = round(mean(METRIC.Precision), digits = digits),
                METRIC.Precision.sd = round(sd(METRIC.Precision), digits = digits),
                METRIC.Recall.mean = round(mean(METRIC.Recall),digits = digits),
                METRIC.Recall.sd = round(sd(METRIC.Recall), digits = digits),
                METRIC.Frac_NA.mean = round(mean(METRIC.Frac_NA), digits = digits),
                METRIC.Frac_NA.sd = round(sd(METRIC.Frac_NA), digits = digits)
            ) %>%
            mutate(
                METRIC.Precision = paste(METRIC.Precision.mean, '&plusmn;', METRIC.Precision.sd),
                METRIC.Recall = paste(METRIC.Recall.mean, '&plusmn;', METRIC.Recall.sd),
                METRIC.Frac_NA = paste(METRIC.Frac_NA.mean, '&plusmn;', METRIC.Frac_NA.sd)
            ) %>%
            select(Group.Id, N, METRIC.Precision, METRIC.Recall, METRIC.Frac_NA)
    } else {
        data = tidy(object) %>% 
            filter(Type == type, Filter == filter) %>% 
            select(Replicate.Id, METRIC.Recall, METRIC.Precision, METRIC.Frac_NA,
                   QUERY.TOTAL, TRUTH.TP, TRUTH.FN, QUERY.FP, QUERY.UNK) %>% 
            mutate(METRIC.Recall = round(METRIC.Recall, digits = digits),
                   METRIC.Precision = round(METRIC.Precision, digits = digits),
                   METRIC.Frac_NA = round(METRIC.Frac_NA, digits = digits))
            
    }
    
    # rename columns
    if (!is.null(colnames)) {
        if (length(colnames) != dim(data)[2]) {
            warning("Provided colnames do not match result dimensions - will not rename")
        }
        data = data %>% setNames(colnames)
    }
    
    ## format output
    data = data %>% 
        knitr::kable(format = kable_format, caption = caption)
    return(data)
    
}
