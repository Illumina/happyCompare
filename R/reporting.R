## happy_results methods


#' Tabulate hap.py summary results
#' 
#' Display hap.py summary results into tabular format using `knitr`.
#' 
#' @param happy_summary A `happy_summary` object.
#' @param type Variant type. One of: `SNP`, `INDEL`.
#' @param filter Variant filter. One of: `PASS`, `ALL`.
#' @param cols Columns to keep.
#' @param colnames Column names to use in the output table. Defaults to
#'   `cols`.
#' @param digits Number of digits to display for performance
#'   metrics. Default: `4`.
#' @param aggregate Summarise performance across groups. Default: `FALSE`.
#'   
#' @return A `data.frame` object with the selected hap.py performance
#'   metrics.
#'   
#' @examples
#' 
#' \dontrun{
#' cols = c('Group.Id', 'Replicate.Id', 'METRIC.Recall', 'METRIC.Precision')
#' colnames = c('Group.Id', 'Sample', 'Recall', 'Precision')
#' t = hc_tabulate_summary(happy_summary, cols = cols, colnames = colnames, 
#'              filter = 'PASS', vartype = 'SNP', aggregate = FALSE)
#' }
#' @export
hc_tabulate_summary <- function(happy_summary, type = c("SNP", "INDEL"), filter = c("PASS", 
  "ALL"), cols, colnames = cols, digits = 4, aggregate = FALSE) {
  
  # validate input
  if ("happy_summary" %in% class(happy_summary)) {
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
    cols <- c("Group.Id", cols)
  }
  if (aggregate && !"Group.Id" %in% colnames) {
    colnames <- c("Group.Id", colnames)
  }
  type <- match.arg(type)
  filter <- match.arg(filter)
  
  # subset data
  data <- happy_summary %>% filter(Type == type, Filter == filter) %>% select_(.dots = cols) %>% 
    setNames(colnames)
  
  # aggregate
  if (aggregate) {
    # TODO: test me
    numeric_colnames <- names(which(vapply(data, is.numeric, logical(1))))  
    summary <- bind_cols(data %>% group_by(Group.Id) %>% summarise(N = n()), 
      lapply(numeric_colnames, function(m) {
        data %>% group_by(Group.Id) %>% select_(.dots = c("Group.Id", m)) %>% 
          summarise_each(funs(mean, sd)) %>% mutate(formatted_str = sprintf("%s &plusmn; %s", 
          round(mean, digits = digits), round(sd, digits = digits))) %>% 
          select_(.dots = c("formatted_str")) %>% setNames(m)
      }))
    data <- summary
  }
  
  return(data)
  
}


#' Plot a ROC curve for precision vs recall
#' 
#' Plot precision vs recall ROC curves from the selected filters.
#' 
#' @param happy_roc A `happy_roc` object.
#' @param filter Variant filter. Default: `PASS`.
#' @param type Variant type. One of: `SNP`, `INDEL`.
#' @param subtype Variant subtype. Default: `*`.
#' @param subset Variant subset. Default: `*`.
#' @param legend_position A character string to pass to `ggplot` to specify
#'   the legend position. Default: `bottom`.
#'   
#' @examples
#' 
#' \dontrun{
#' roc = extract(happy_compare, table = 'pr_curve.all')
#' hc_plot_roc(roc, type = 'SNP', filter = 'ALL')  
#' }
#' @export
hc_plot_roc <- function(happy_roc, filter = "PASS", type, subtype = "*", subset = "*", 
  legend_position = "bottom") {
  
  # validate
  if ("happy_roc" %in% class(happy_roc)) {
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
  sel_filter <- ifelse(filter %in% c("PASS", "SEL"), filter, "PASS")
  data <- rbind(happy_roc %>% filter(Type == type, Subtype == subtype, Filter == 
    filter, Subset == subset) %>% mutate(Filter = "ROC"), happy_roc %>% filter(Type == 
    type, Subtype == subtype, Filter %in% c(filter, sel_filter), Subset == subset) %>% 
    group_by(Filter) %>% filter(QQ == min(QQ)) %>% ungroup() %>% mutate(Filter = "SEL"))
  
  # axis limits
  sel <- subset(data, Filter == "SEL")
  margin <- .01
  xlim <- c(max(0, min(sel$METRIC.Recall, na.rm = TRUE) - margin),
            min(1, max(sel$METRIC.Recall, na.rm = TRUE) + margin))
  ylim <- c(max(0, min(sel$METRIC.Precision, na.rm = TRUE) - margin),
            min(1, max(sel$METRIC.Precision, na.rm = TRUE) + margin))
  
  # title
  title <- paste(filter, type, "- Subset:", subset)
  
  # plot
  ggplot(data, aes(x = METRIC.Recall, y = METRIC.Precision, color = Group.Id, group = Group.Id)) + 
    coord_cartesian(xlim = xlim, ylim = ylim) + # roc lines
  geom_line(data = data %>% filter(Filter == "ROC"), size = 1) + # connector line
  geom_line(data = data %>% filter(Filter == "SEL"), size = 1, lty = 2) + # pass and all points
  geom_point(data = data %>% filter(Filter == "SEL"), size = 4) + ggtitle(title) + 
    theme(legend.position = legend_position)
  
}


#' Plot HDI
#' 
#' Plot distribution of success rates and HDI estimates for 
#' a given subset from a `data.frame`.
#' 
#' @param happy_hdi A `data.frame` obtained with `estimate_hdi()`.
#' @param title Title. Default: `NULL`.
#'   
#' @examples
#' 
#' \dontrun{
#' hdi = estimate_hdi(happy_extended, successes_col = 'TRUTH.TP', totals_col = 'TRUTH.TOTAL', 
#'                    group_cols = c('Group.Id', 'Subset', 'Type', 'Subtype'))
#' hc_plot_hdi(happy_hdi = hdi %>% filter(Subset == '*'))  
#' }
#' @export
hc_plot_hdi <- function(happy_hdi, title = NULL) {
  
  # validate input
  if (length(unique(happy_hdi$Subset)) > 1) {
    stop("Must plot one subset at a time")
  }
  
  # plot
  n_groups <- length(unique(happy_hdi$Group.Id))
  if (n_groups == 1) {
    .plot_hdi_one_group(happy_hdi, title)
  } else {
    .plot_hdi_multiple_groups(happy_hdi, title)
  }
  
}

.plot_hdi_one_group <- function(happy_hdi, title = NULL) {
  # validate input
  if (dim(happy_hdi)[1] != length(unique(happy_hdi$Replicate.Id))) {
    stop("Too many data points provided; revise input data.frame")
  }
  if (length(unique(happy_hdi$Group.Id)) > 1) {
    stop("More than one group provided")
  }
  
  # plot
  x <- seq(0, 1, length = 100)
  line_data <- lapply(1:dim(happy_hdi)[1], function(i) {
    Replicate.Id <- happy_hdi$Replicate.Id[i]
    d <- data.frame(x = x, density = dbeta(x, happy_hdi$alpha1[i], happy_hdi$beta1[i]), 
      Group.Id = Replicate.Id, color = ifelse(Replicate.Id == ".aggregate", "red", 
        "black"), lwd = ifelse(Replicate.Id == ".aggregate", 1, 0.5))
    d$Group.Id <- as.character(d$Group.Id)
    d$color <- as.character(d$color)
    d
  }) %>% bind_rows()
  
  upper_ylim <- max(line_data$density[!is.infinite(line_data$density)])
  lower_ylim <- -max(line_data$density[!is.infinite(line_data$density)])/3
  padding <- abs(lower_ylim/(dim(happy_hdi)[1] + 1))
  
  segment_data <- lapply(1:dim(happy_hdi)[1], function(i) {
    Replicate.Id <- happy_hdi$Replicate.Id[i]
    d <- data.frame(x = happy_hdi$lower[i], 
                    xend = happy_hdi$upper[i], 
                    y = 0 - i * padding, 
                    yend = 0 - i * padding, 
                    Group.Id = Replicate.Id, 
                    color = ifelse(Replicate.Id == ".aggregate", "red", "black"), lwd = 1)
    d$Group.Id <- as.character(d$Group.Id)
    d$color <- as.character(d$color)
    d
  }) %>% bind_rows()
  segment_data$color <- factor(segment_data$color)
  
  point_data_observed <- lapply(1:dim(happy_hdi)[1], function(i) {
    Replicate.Id <- happy_hdi$Replicate.Id[i]
    d <- data.frame(x = happy_hdi$observed_p[i], 
                    y = 0 - i * padding, 
                    Group.Id = Replicate.Id,
                    color = ifelse(Replicate.Id == ".aggregate", "red", "black"))
    d$Group.Id <- as.character(d$Group.Id)
    d$color <- as.character(d$color)
    d
  }) %>% bind_rows()
  
  point_data_estimated <- lapply(1:dim(happy_hdi)[1], function(i) {
    Replicate.Id <- happy_hdi$Replicate.Id[i]
    d <- data.frame(x = happy_hdi$estimated_p[i], 
                    y = 0 - i * padding, 
                    Group.Id = Replicate.Id, 
                    color = ifelse(Replicate.Id == ".aggregate", "red", "black"))
    d$Group.Id <- as.character(d$Group.Id)
    d$color <- as.character(d$color)
    d
  }) %>% bind_rows()
  
  colors <- rep("black", dim(happy_hdi)[1])
  names(colors) <- happy_hdi$Replicate.Id
  colors[names(colors) == ".aggregate"] <- "red"
  
  s <- happy_hdi[happy_hdi$Replicate.Id != ".aggregate", ]$successes
  t <- happy_hdi[happy_hdi$Replicate.Id != ".aggregate", ]$totals
  subtitle <- paste0("successes: ", paste(s, collapse = ", "), "\n", 
                     "totals: ", paste(t, collapse = ", "), "\n", 
                     "sigma: ", round(unique(happy_hdi$sigma), 4))
  
  ggplot() + geom_line(data = line_data[line_data$Group.Id != ".aggregate", ], 
                       aes(x = x, y = density, group = Group.Id, color = Group.Id), lwd = 0.5, lty = 2) + 
    geom_line(data = line_data[line_data$Group.Id == ".aggregate", ], 
              aes(x = x, y = density, group = Group.Id, color = Group.Id), lwd = 1, lty = 1) + 
    geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend, color = Group.Id), lwd = 1) + 
    geom_point(data = point_data_estimated, aes(x = x, y = y, color = Group.Id)) + 
    geom_point(data = point_data_observed, pch = 4, aes(x = x, y = y, color = Group.Id)) + 
    scale_colour_manual(values = colors) + 
    annotate("rect", xmin = segment_data[segment_data$group == ".aggregate", ]$x, 
             xmax = segment_data[segment_data$group == ".aggregate", ]$xend, 
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") + 
    theme(legend.position = "none") + 
    ylim(NA, upper_ylim) + xlab("p") + 
    ggtitle(label = title, subtitle = subtitle)
  
}

.plot_hdi_multiple_groups <- function(happy_hdi, title = NULL) {
  # subset aggregate replicate
  sdf <- happy_hdi %>% filter(Replicate.Id == ".aggregate")
  
  # validate input
  if (dim(sdf)[1] != length(unique(happy_hdi$Group.Id))) {
    stop("Too many data points provided; revise input data.frame")
  }
  
  # plot
  x <- seq(0, 1, length = 100)
  line_data <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = x, density = dbeta(x, sdf$alpha1[i], sdf$beta1[i]), group = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  upper_ylim <- max(line_data$density[!is.infinite(line_data$density)])
  lower_ylim <- -max(line_data$density[!is.infinite(line_data$density)])/3
  padding <- abs(lower_ylim/(dim(sdf)[1] + 1))
  
  segment_data <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = sdf$lower[i], xend = sdf$upper[i], y = 0 - i * padding, 
      yend = 0 - i * padding, group = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  point_data_observed <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = sdf$observed_p[i], y = 0 - i * padding, group = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  point_data_estimated <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = sdf$estimated_p[i], y = 0 - i * padding, group = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  subtitle <- happy_hdi %>% 
    filter(Replicate.Id != ".aggregate") %>% 
    group_by(Group.Id) %>% 
    summarise(n = n(), 
              s = paste(successes, collapse = ","), 
              t = paste(totals, collapse = ",")) %>% 
    mutate(subtitle = sprintf("%s - n: %d; s: %s; t: %s", Group.Id, n, s, t)) %>% 
    select(subtitle) %>% unlist() %>% paste(., collapse = "\n")
  
  ggplot() + 
    geom_line(data = line_data, 
              aes(x = x, y = density, group = Group.Id, color = Group.Id), lwd = 1, lty = 1) + 
    geom_segment(data = segment_data, 
                 aes(x = x, y = y, xend = xend, yend = yend, color = Group.Id), lwd = 1) + 
    geom_point(data = point_data_estimated, aes(x = x, y = y, color = Group.Id)) + 
    geom_point(data = point_data_observed, pch = 4, aes(x = x, y = y, color = Group.Id)) + 
    ylim(NA, upper_ylim) + 
    xlab("p") + 
    ggtitle(label = title, subtitle = subtitle) + 
    theme(legend.position = "bottom")
  
}
