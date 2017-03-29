## happy_results methods


#' Tabulate hap.py summary results
#' 
#' Display hap.py summary results into tabular format using \code{knitr}.
#' 
#' @param happy_summary A \code{happy_summary} object.
#' @param type Variant type. One of: \code{SNP}, \code{INDEL}.
#' @param filter Variant filter. One of: \code{PASS}, \code{ALL}.
#' @param cols Columns to keep.
#' @param colnames Column names to use in the output table. Defaults to
#'   \code{cols}.
#' @param caption Table caption.
#' @param significant_digits Number of significant digits in performance
#'   metrics. Default: \code{4}.
#' @param kable_format A character string to pass to \code{knitr::kable()}.
#'   Default: \code{markdown}.
#' @param aggregate Summarise performance across groups. Default: \code{FALSE}.
#'   
#' @return A \code{knitr_kable} object with the selected hap.py performance
#'   metrics.
#'   
#' @examples
#' 
#' \dontrun{
#' cols = c("Group.Id", "Replicate.Id", "METRIC.Recall", "METRIC.Precision")
#' colnames = c("Group.Id", "Sample", "Recall", "Precision")
#' t = tabulate(happy_summary = happy_summary, cols = cols, colnames = colnames, 
#'              filter = "PASS", vartype = "SNP", aggregate = FALSE)
#' }
#' 
#' @export
tabulate.happy_summary = function(happy_summary, type = c("SNP", "INDEL"), 
                                  filter = c("PASS", "ALL"), cols, colnames = cols, caption = NULL,
                                  significant_digits = 4, kable_format = "markdown", aggregate = FALSE, ...) {
  
  # validate input
  if (class(happy_summary)[1] != "happy_summary") {
    stop("Must provide a happy_summary object")
  }
  if (!all(cols %in% names(happy_summary))) {
    stop("Undefined columns selected")
  }
  if (!is.null(colnames) && length(cols) != length(colnames)) {
    stop("colnames length must match cols")
  }
  if (aggregate && !"Group.Id" %in% names(happy_summary)) {
    stop("happy_summary must contain Group.Id in order to aggregate results")
  }
  if (aggregate && !"Group.Id" %in% cols) {
    cols = c("Group.Id", cols)
  }
  if (aggregate && !"Group.Id" %in% colnames) {
    colnames = c("Group.Id", colnames)
  }
  type = match.arg(type)
  filter = match.arg(filter)
  
  # subset data
  data = happy_summary %>% 
    filter(Type == type, Filter == filter) %>%
    select_(.dots = cols) %>% 
    setNames(colnames)
  
  # aggregate
  if (aggregate) {
    numeric_colnames = suppressWarnings(apply(data[1,], 2, as.numeric))
    numeric_colnames = names(numeric_colnames[!is.na(numeric_colnames)])
    
    summary = bind_cols(
      data %>% 
        group_by(Group.Id) %>% 
        summarise(N = n()),
      lapply(numeric_colnames, function(m) {
        data %>% 
          group_by(Group.Id) %>% 
          select_(.dots = c("Group.Id", m)) %>% 
          summarise_each(funs(mean, sd)) %>% 
          mutate(formatted_str = sprintf("%s &plusmn; %s", 
                               round(mean, digits = significant_digits), 
                               round(sd, digits = significant_digits))) %>% 
          select_(.dots = c("formatted_str")) %>% 
          setNames(m)
      })
      )
    data = summary
  }
  
  # fomat table
  caption = ifelse(!is.null(caption), caption, paste(filter, type, collapse = '-'))
  data = data %>% 
    knitr::kable(format = kable_format, caption = caption)
  return(data)
  
}


#' Plot a ROC curve for precision vs recall
#' 
#' Plot precision vs recall ROC curves from the selected filters.
#' 
#' @param happy_roc A \code{happy_roc} object.
#' @param filter Variant filter. Default: \code{PASS}.
#' @param type Variant type. One of: \code{SNP}, \code{INDEL}.
#' @param subtype Variant subtype. Default: \code{*}.
#' @param subset Variant subset. Default: \code{*}.
#' @param legend_position A character string to pass to \code{ggplot} to specify
#'   the legend position. Default: \code{bottom}.
#'   
#' @return A \code{ggplot2} plot.
#'   
#' @examples
#' 
#' \dontrun{
#' roc = extract(happyCompare_list, table = "pr_curve.all")
#' plot(roc, type = "SNP", filter = "ALL")  
#' }
#' @export
plot.happy_roc = function(happy_roc, filter = "PASS", type, subtype = "*", 
                          subset = "*", legend_position = "bottom", ...) {
  
  # validate
  if (class(happy_roc)[1] != "happy_roc") {
    stop("Must provide a happy_roc object")
  }
  if (missing(type)) {
    stop("Must specify a variant type")
  }
  if (!type %in% unique(happy_roc$Type)) {
    stop("Invalid type")
  }
  if (!subtype %in% unique(happy_roc$Subtype)) {
    stop("Invalid subtype")
  }
  if (!filter %in% unique(happy_roc$Filter)) {
    stop("Invalid filter")
  }
  if (!subset %in% unique(happy_roc$Subset)) {
    stop("Invalid subset")
  }  
  
  # format data
  sel_filter = ifelse(filter %in% c("PASS", "SEL"), filter, "PASS")
  data = rbind(
    happy_roc %>% 
      filter(Type == type, Subtype == subtype, Filter == filter, Subset == subset) %>% 
      mutate(Filter = "ROC"),
    happy_roc %>%
      filter(Type == type, Subtype == subtype, Filter %in% c(filter, sel_filter), Subset == subset) %>% 
      group_by(Filter) %>% 
      filter(QQ == min(QQ)) %>% 
      ungroup() %>% 
      mutate(Filter = "SEL")
  )
  
  # axis limits
  xlim = c(max(0, data %>% filter(Filter == "SEL") %>% select(METRIC.Recall) %>% min(na.rm = TRUE) - 0.01),
           min(1, data %>% filter(Filter == "SEL") %>% select(METRIC.Recall) %>% max(na.rm = TRUE) + 0.01))
  ylim = c(max(0, data %>% filter(Filter == "SEL") %>% select(METRIC.Precision) %>% min(na.rm = TRUE) - 0.01),
           min(1, data %>% filter(Filter == "SEL") %>% select(METRIC.Precision) %>% max(na.rm = TRUE) + 0.01))
  
  # title
  title = paste(filter, type, "- Subset:", subset)
  
  # plot
  ggplot(data, aes(x = METRIC.Recall, y = METRIC.Precision, color = Group.Id, group = Group.Id)) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    # roc lines
    geom_line(data = data %>% filter(Filter == "ROC"), size = 1) +
    # connector line
    geom_line(data = data %>% filter(Filter == "SEL"), size = 1, lty = 2) +
    # pass and all points
    geom_point(data = data %>% filter(Filter == "SEL"), size = 4) +
    ggtitle(title) +
    theme(legend.position = legend_position)

}
