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
#' @param significant_digits Number of significant digits in performance
#'   metrics. Default: \code{4}.
#' @param aggregate Summarise performance across groups. Default: \code{FALSE}.
#'   
#' @return A \code{data.frame} object with the selected hap.py performance
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
#' @export
tabulate = function(happy_summary, ...) {
  UseMethod("tabulate", happy_summary)
}
#' @export
tabulate.happy_summary = function(happy_summary, type = c("SNP", "INDEL"), 
                                  filter = c("PASS", "ALL"), cols, colnames = cols, 
                                  significant_digits = 4, aggregate = FALSE, ...) {
  
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
plot = function(happy_roc, ...) {
  UseMethod("plot", happy_roc)
}
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


#' Plot HDI
#' 
#' Plot distribution of success rates and HDI estimates for 
#' a given subset from a \code{data.frame}.
#' 
#' @param happy_hdi A \code{data.frame} obtained with \code{estimate_hdi()}.
#' @param title Title. Default: \code{NULL}.
#'   
#' @return A \code{ggplot2} plot.
#'   
#' @examples
#' 
#' \dontrun{
#' hdi = estimate_hdi(happy_extended, successes_col = "TRUTH.TP", totals_col = "TRUTH.TOTAL", 
#'                    group_cols = c("Group.Id", "Subset", "Type", "Subtype"))
#' plot(happy_hdi = hdi %>% filter(Subset == "*"))  
#' }
#' @export
plot = function(happy_hdi, ...) {
  UseMethod("plot", happy_hdi)
}
#' @export
plot.data.frame = function(happy_hdi, title = NULL) {
  
  # validate input
  if (length(unique(happy_hdi$Subset)) > 1) {
    stop("Must plot one subset at a time")
  }

  # plot
  n_groups = length(unique(happy_hdi$Group.Id))
  if (n_groups == 1) {
    .plot_hdi_one_group(happy_hdi, title)
  } else {
    .plot_hdi_multiple_groups(happy_hdi, title)
  }
  
}

.plot_hdi_one_group = function(happy_hdi, title = NULL) {
  # validate input
  if (dim(happy_hdi)[1] != length(unique(happy_hdi$replicate_id))) {
    stop("Too many data points provided; revise input data.frame")
  }
  if (length(unique(happy_hdi$Group.Id))> 1) {
    stop("More than one group provided")
  }
  
  # plot
  x = seq(0, 1, length = 100)
  line_data = lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id = happy_hdi$replicate_id[i]
    d = data.frame(
      x = x, 
      density = dbeta(x, happy_hdi$alpha1[i], happy_hdi$beta1[i]), 
      group = replicate_id,
      color = ifelse(replicate_id == ".aggregate", "red", "black"),
      lwd = ifelse(replicate_id == ".aggregate", 1, 0.5)
    )
    d$group = as.character(d$group)
    d$color = as.character(d$color)
    d
  }) %>% bind_rows()
  
  upper_ylim = max(line_data$density[!is.infinite(line_data$density)])
  lower_ylim = -max(line_data$density[!is.infinite(line_data$density)]) / 3
  padding = abs(lower_ylim / (dim(happy_hdi)[1] + 1))
  
  segment_data = lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id = happy_hdi$replicate_id[i]
    d = data.frame(
      x = happy_hdi$lower[i],
      xend = happy_hdi$upper[i], 
      y = 0 - i*padding,
      yend = 0 - i*padding,
      group = replicate_id,
      color = ifelse(replicate_id == ".aggregate", "red", "black"),
      lwd = 1
    )
    d$group = as.character(d$group)
    d$color = as.character(d$color)
    d
  }) %>% bind_rows()
  segment_data$color = factor(segment_data$color)
  
  point_data_observed = lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id = happy_hdi$replicate_id[i]
    d = data.frame(
      x = happy_hdi$observed_p[i],
      y = 0 - i*padding,
      group = replicate_id,
      color = ifelse(replicate_id == ".aggregate", "red", "black")
    )
    d$group = as.character(d$group)
    d$color = as.character(d$color)
    d
  }) %>% bind_rows()    
  
  point_data_estimated = lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id = happy_hdi$replicate_id[i]
    d = data.frame(
      x = happy_hdi$estimated_p[i],
      y = 0 - i*padding,
      group = replicate_id,
      color = ifelse(replicate_id == ".aggregate", "red", "black")
    )
    d$group = as.character(d$group)
    d$color = as.character(d$color)
    d
  }) %>% bind_rows()   
  
  colors = rep("black", dim(happy_hdi)[1])
  names(colors) = happy_hdi$replicate_id
  colors[names(colors) == ".aggregate"] = "red"
  
  title = title
  s = happy_hdi[happy_hdi$replicate_id != ".aggregate", ]$successes
  t = happy_hdi[happy_hdi$replicate_id != ".aggregate", ]$totals
  subtitle = paste0(
    "successes: ", paste(s, collapse = ", "), "\n",
    "totals: ", paste(t, collapse = ", "), "\n",
    "sigma: ", round(unique(happy_hdi$sigma), 4)
  )
  
  ggplot() +
    geom_line(data = line_data[line_data$group != ".aggregate", ], 
              aes(x = x, y = density, group = group, color = group), lwd = 0.5, lty = 2) +
    geom_line(data = line_data[line_data$group == ".aggregate", ], 
              aes(x = x, y = density, group = group, color = group), lwd = 1, lty = 1) +
    geom_segment(data = segment_data, 
                 aes(x = x, y = y, xend = xend, yend = yend, color = group), lwd = 1) +
    geom_point(data = point_data_estimated,
               aes(x = x, y = y, color = group)) +
    geom_point(data = point_data_observed, pch = 4,
               aes(x = x, y = y, color = group)) +
    scale_colour_manual(values = colors) +
    annotate("rect", xmin = segment_data[segment_data$group == ".aggregate", ]$x, 
             xmax = segment_data[segment_data$group == ".aggregate", ]$xend, 
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") +
    theme(legend.position = "none") +
    ylim(NA, upper_ylim) +
    xlab("p") +
    ggtitle(label = title, 
            subtitle = subtitle)
  
}

.plot_hdi_multiple_groups = function(happy_hdi, title = NULL) {
  # subset aggregate replicate
  sdf = happy_hdi %>% filter(replicate_id == ".aggregate")
    
  # validate input
  if (dim(sdf)[1] != length(unique(happy_hdi$Group.Id))) {
    stop("Too many data points provided; revise input data.frame")
  }
  
  # plot
  x = seq(0, 1, length = 100)
  line_data = lapply(1:dim(sdf)[1], function(i) {
    d = data.frame(
      x = x, 
      density = dbeta(x, sdf$alpha1[i], sdf$beta1[i]), 
      group = sdf$Group.Id[i]
    )
    d$group = as.character(d$group)
    d
  }) %>% bind_rows()
  
  upper_ylim = max(line_data$density[!is.infinite(line_data$density)])
  lower_ylim = -max(line_data$density[!is.infinite(line_data$density)]) / 3
  padding = abs(lower_ylim / (dim(sdf)[1] + 1))
  
  segment_data = lapply(1:dim(sdf)[1], function(i) {
    d = data.frame(
      x = sdf$lower[i],
      xend = sdf$upper[i], 
      y = 0 - i*padding,
      yend = 0 - i*padding,
      group = sdf$Group.Id[i]
    )
    d$group = as.character(d$group)
    d
  }) %>% bind_rows()
  
  point_data_observed = lapply(1:dim(sdf)[1], function(i) {
    d = data.frame(
      x = sdf$observed_p[i],
      y = 0 - i*padding,
      group = sdf$Group.Id[i]
    )
    d$group = as.character(d$group)
    d
  }) %>% bind_rows()    
  
  point_data_estimated = lapply(1:dim(sdf)[1], function(i) {
    d = data.frame(
      x = sdf$estimated_p[i],
      y = 0 - i*padding,
      group = sdf$Group.Id[i]
    )
    d$group = as.character(d$group)
    d
  }) %>% bind_rows()   
  
  title = title
  subtitle = happy_hdi %>% 
    filter(replicate_id != ".aggregate") %>% 
    group_by(Group.Id) %>% 
    summarise(
      n = n(),
      s = paste(successes, collapse = ","),
      t = paste(totals, collapse = ",")) %>% 
    mutate(subtitle = sprintf("%s - n: %d; s: %s; t: %s", Group.Id, n, s, t)) %>% 
    select(subtitle) %>% 
    unlist() %>% 
    paste(., collapse = "\n")
  
  ggplot() +
    geom_line(data = line_data, 
              aes(x = x, y = density, group = group, color = group), lwd = 1, lty = 1) +
    geom_segment(data = segment_data, 
                 aes(x = x, y = y, xend = xend, yend = yend, color = group), lwd = 1) +
    geom_point(data = point_data_estimated,
               aes(x = x, y = y, color = group)) +
    geom_point(data = point_data_observed, pch = 4,
               aes(x = x, y = y, color = group)) +
    ylim(NA, upper_ylim) +
    xlab("p") +
    ggtitle(label = title, 
            subtitle = subtitle) +
    theme(legend.position = "bottom")
  
}
