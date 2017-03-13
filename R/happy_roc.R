## happy_roc methods

#' plot
#' 
#' Plot ROC curves.
#' 
#' @param x A `happy_roc` object.
#' @param filter Variant filter. Default = "PASS".
#' @param type Variant type. One of: "SNP", "INDEL".
#' @param subtype Variant subtype. Default = "*".
#' @param ... Extra arguments.
#' 
#' @export
plot.happy_roc = function(x, filter = "PASS", type, subtype = "*", ...) {
  # validate
  if (class(x)[1] != "happy_roc") {
    stop("Must provide a happy_roc object.")
  }
  if (missing(type)) {
    stop("Must specify a variant type.")
  }
  if (!type %in% unique(x$Type)) {
    stop("Invalid type.")
  }
  if (!subtype %in% unique(x$Subtype)) {
    stop("Invalid subtype.")
  }
  if (!filter %in% unique(x$Filter)) {
    stop("Invalid filter.")
  }
  
  
  # format data
  data = rbind(
    x %>% 
      filter(Type == type, Subtype == subtype, Filter == filter) %>% 
      mutate(Filter = "ROC"),
    x %>%
      filter(Type == type, Subtype == subtype, Filter != "PASS") %>% 
      group_by(Filter) %>% 
      filter(QQ == min(QQ)) %>% 
      ungroup() %>% 
      mutate(Filter = "SEL")
  )
    
  # axis limits
  xlim = c(max(0, data %>% filter(Filter == "SEL") %>% select(METRIC.Recall) %>% min(na.rm = TRUE) - 0.02),
           min(1, data %>% filter(Filter == "SEL") %>% select(METRIC.Recall) %>% max(na.rm = TRUE) + 0.02))
  ylim = c(max(0, data %>% filter(Filter == "SEL") %>% select(METRIC.Precision) %>% min(na.rm = TRUE) - 0.01),
           min(1, data %>% filter(Filter == "SEL") %>% select(METRIC.Precision) %>% max(na.rm = TRUE) + 0.01))
  
  # title
  title = paste(filter, type)
  
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
    theme(legend.position = "bottom")
}