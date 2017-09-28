## custom reporting methods


#' Summarise metrics
#' 
#' Calculate mean and sd for specified metrics and return formatted results.
#' 
#' @param df A `data.frame` object.
#' @param group_cols Vector of columns to group counts by. Observations 
#' within the same group will be treated as replicates.
#' @param metric_cols Vector of metric columns for which to calculate summary stats.
#' @param digits Number of decimal places. Default: `4`.
#'   
#' @return A formatted `data.frame` object.
#'   
#' @examples
#' 
#' \dontrun{
#' df <- extract_metrics(happy_compare, table = "summary")
#' hc_summarise_metrics(df)
#' }
#' 
#' @export
hc_summarise_metrics <- function(df, group_cols = c("Group.Id", "Type", "Filter"), 
                                 metric_cols = c("METRIC.F1_Score", "METRIC.Recall", "METRIC.Precision", "METRIC.Frac_NA"),
                                 digits = 4) {
  
  # validate input
  if (!"data.frame" %in% class(df)) {
    stop("Must provide a data.frame object")
  }
  if (dim(df)[1] == 0) {
    stop("Input data.frame is empty")
  }
  if (!all(group_cols %in% colnames(df))) {
    stop("Could not find all specified group columns in input data.frame")
  }
  if (!all(metric_cols %in% colnames(df))) {
    stop("Could not find all specified metric columns in input data.frame")
  }
  
  # group
  df <- df %>% ungroup() %>% group_by_(.dots = lapply(group_cols, as.symbol))
  
  # calculate summary metrics for selected vars
  summary <- df %>% summarise_at(.vars = metric_cols, .funs = c("mean", "sd"))
  
  # fix col names
  colnames(summary) <- stringr::str_replace(colnames(summary), "_mean", "#mean")
  colnames(summary) <- stringr::str_replace(colnames(summary), "_sd", "#sd")
  
  # reformat as mean +- sd
  gather_cols <- c(paste(metric_cols, "mean", sep = "#"), paste(metric_cols, "sd", sep = "#"))
  mutate_dots <- lazyeval::interp(~ sprintf("%s &plusmn; %s", round(mean, d = digits), round(sd, d = digits)))
  summary <- summary %>% 
    gather_(key_col = "variable", value_col = "value", gather_cols = gather_cols) %>% 
    separate(variable, into = c("metric", "fun"), sep = "#") %>% 
    spread_(key_col = "fun", value_col = "value") %>% 
    mutate_(.dots = setNames(list(mutate_dots), "value")) %>% 
    select_(.dots = c(group_cols, "metric", "value")) %>% 
    spread_(key_col = "metric", value_col = "value")
    
  return(summary)

}


#' Plot a ROC curve for precision vs recall
#' 
#' Plot precision vs recall ROC curves from the selected filters. If `pr.all` is
#' provided, overlay pass points to `ALL` curve.
#' 
#' @param happy_roc A `happy_roc` object.
#' @param type Variant type.
#' @param filter Variant filter.
#' @param subtype Variant subtype. Default: `*`.
#' @param subset Variant subset. Default: `*`.
#' @param xlim Vector with x axis limits, e.g. c(0, 1). Default: auto-adjust.
#' @param ylim Vector with y axis limits, e.g. c(0, 1). Default: auto-adjust.
#'   
#' @examples
#' 
#' \dontrun{
#' roc <- extract_metrics(happy_compare, table = "pr.all")
#' hc_plot_roc(roc, type = "INDEL", filter = "PASS")
#' }
#' @export
hc_plot_roc <- function(happy_roc, type, filter, subtype = "*", subset = "*", xlim = NA, ylim = NA) {
  
  # validate
  if (!"happy_roc" %in% class(happy_roc)) {
    stop("Must provide a happy_roc object")
  }
  if (!type %in% unique(happy_roc$Type)) {
    stop("Invalid type")
  }
  if (!filter %in% unique(happy_roc$Filter)) {
    stop("Invalid filter")
  }  
  if (!subtype %in% unique(happy_roc$Subtype)) {
    stop("Invalid subtype")
  }
  if (!subset %in% unique(happy_roc$Subset)) {
    stop("Invalid subset")
  }
  
  # format data
  sel_filter <- ifelse(filter %in% c("PASS", "SEL"), filter, "PASS")
  data <- rbind(happy_roc %>% 
                  filter(Type == type, Subtype == subtype, Filter == filter, Subset == subset) %>% 
                  mutate(Filter = "ROC"), 
                happy_roc %>% 
                  filter(Type == type, Subtype == subtype, Filter %in% c(filter, sel_filter), Subset == subset) %>% 
                  group_by(Filter) %>% 
                  filter(QQ == min(QQ)) %>% 
                  ungroup() %>% mutate(Filter = "SEL")
                )
  
  # axis limits
  sel <- subset(data, Filter == "SEL")
  margin <- 0.05
  if (all(is.na(xlim))) {
    xlim <- c(max(0, min(sel$METRIC.Recall, na.rm = TRUE) - margin),
              min(1, max(sel$METRIC.Recall, na.rm = TRUE) + margin))
    
  }
  if (all(is.na(ylim))) {
    ylim <- c(max(0, min(sel$METRIC.Precision, na.rm = TRUE) - margin),
              min(1, max(sel$METRIC.Precision, na.rm = TRUE) + margin))
    
  }
  
  # plot
  ggplot(data, aes(x = METRIC.Recall, y = METRIC.Precision, color = Group.Id)) + 
    coord_cartesian(xlim = xlim, ylim = ylim) + # limits
    geom_line(data = data %>% filter(Filter == "ROC"), aes(group = Replicate.Id)) + # roc curve
    geom_line(data = data %>% filter(Filter == "SEL"), aes(group = Replicate.Id), lty = 2) + # connector line
    geom_point(data = data %>% filter(Filter == "SEL"), size = 2) + # pass and all points
    ggtitle(paste(filter, type, "- Subset:", subset)) +
    theme(legend.position = "bottom")
  
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
#'   e <- estimate_hdi(df = d, successes_col = successes_col, totals_col = totals_col, 
#'                     group_cols = group_cols, aggregate_only = FALSE, sample_size = 1000)
#'   h <- e %>% dplyr::filter(Subset == "high.at")
#'   hc_plot_hdi(h, title = "PCR-Free vs Nano high.at")
#' }
#' 
#' @export
hc_plot_hdi <- function(happy_hdi, title = NULL) {
  
  # validate input
  if (length(unique(happy_hdi$Subset)) > 1) {
    stop("Must plot one subset at a time")
  }
  
  # plot
  n_groups <- length(unique(happy_hdi$Group.Id))
  message(sprintf("Plotting HDIs across %d group(s)", n_groups))
  if (n_groups == 1) {
    .plot_hdi_one_group(happy_hdi, title)
  } else {
    .plot_hdi_multiple_groups(happy_hdi, title)
  }
  
}

.plot_hdi_one_group <- function(happy_hdi, title = "One group") {
  # validate input
  if (dim(happy_hdi)[1] != length(unique(happy_hdi$replicate_id))) {
    stop("Too many data points provided; revise input data.frame")
  }
  if (length(unique(happy_hdi$Group.Id)) > 1) {
    stop("More than one group provided")
  }
  
  # prepare data
  happy_hdi <- happy_hdi %>% 
    mutate(Category = ifelse(replicate_id == ".aggregate", "aggregate", "replicate"))
  
  x <- seq(0, 1, length = 100)
  line_data <- lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id <- happy_hdi$replicate_id[i]
    d <- data.frame(x = x, 
                    density = dbeta(x, happy_hdi$alpha1[i], happy_hdi$beta1[i]), 
                    Replicate.Id = replicate_id,
                    Category = happy_hdi$Category[i], 
                    color = ifelse(replicate_id == ".aggregate", "red", "black"), 
                    lwd = ifelse(replicate_id == ".aggregate", 1, 0.5))
    d$Category <- as.character(d$Category)
    d$color <- as.character(d$color)
    d$Replicate.Id <- as.character(d$Replicate.Id)
    d
  }) %>% bind_rows()
  
  upper_ylim <- max(line_data$density[!is.infinite(line_data$density)])
  lower_ylim <- -max(line_data$density[!is.infinite(line_data$density)])/3
  padding <- abs(lower_ylim/(dim(happy_hdi)[1] + 1))
  
  segment_data <- lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id <- happy_hdi$replicate_id[i]
    d <- data.frame(x = happy_hdi$lower[i], 
                    xend = happy_hdi$upper[i], 
                    y = 0 - i * padding, 
                    yend = 0 - i * padding, 
                    Replicate.Id = replicate_id,
                    Category = happy_hdi$Category[i], 
                    color = ifelse(replicate_id == ".aggregate", "red", "black"), 
                    lwd = 1)
    d$Category <- as.character(d$Category)
    d$color <- as.character(d$color)
    d$Replicate.Id <- as.character(d$Replicate.Id)
    d
  }) %>% bind_rows()
  segment_data$color <- factor(segment_data$color)
  
  point_data_observed <- lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id <- happy_hdi$replicate_id[i]
    d <- data.frame(x = happy_hdi$observed_p[i], 
                    y = 0 - i * padding, 
                    Replicate.Id = replicate_id,
                    Category = happy_hdi$Category[i], 
                    color = ifelse(replicate_id == ".aggregate", "red", "black"))
    d$Category <- as.character(d$Category)
    d$color <- as.character(d$color)
    d$Replicate.Id <- as.character(d$Replicate.Id)
    d
  }) %>% bind_rows()
  
  point_data_estimated <- lapply(1:dim(happy_hdi)[1], function(i) {
    replicate_id <- happy_hdi$replicate_id[i]
    d <- data.frame(x = happy_hdi$estimated_p[i], 
                    y = 0 - i * padding, 
                    Replicate.Id = replicate_id,
                    Category = happy_hdi$Category[i], 
                    color = ifelse(replicate_id == ".aggregate", "red", "black"))
    d$Category <- as.character(d$Category)
    d$color <- as.character(d$color)
    d$Replicate.Id <- as.character(d$Replicate.Id)
    d
  }) %>% bind_rows()
  
  s <- happy_hdi[happy_hdi$replicate_id != ".aggregate", ]$successes
  t <- happy_hdi[happy_hdi$replicate_id != ".aggregate", ]$totals
  subtitle <- paste0("successes: ", paste(s, collapse = ", "), "\n", 
                     "totals: ", paste(t, collapse = ", "), "\n", 
                     "sigma: ", round(unique(happy_hdi$sigma), 4))
  
  # plot
  ggplot() + 
    geom_line(data = line_data[line_data$Replicate.Id != ".aggregate", ], 
              aes(x = x, y = density, group = Replicate.Id, color = Category), 
              lwd = 0.5, lty = 2) + 
    geom_line(data = line_data[line_data$Replicate.Id == ".aggregate", ], 
              aes(x = x, y = density, group = Replicate.Id, color = Category), 
              lwd = 1, lty = 1) + 
    geom_segment(data = segment_data, 
                 aes(x = x, y = y, xend = xend, yend = yend, color = Category), 
                 lwd = 1) + 
    geom_point(data = point_data_estimated, 
               aes(x = x, y = y, color = Category)) + 
    geom_point(data = point_data_observed, 
               aes(x = x, y = y, color = Category),
               pch = 4) + 
    scale_colour_manual(values = c("red", "black")) + 
    annotate("rect", 
             xmin = segment_data[segment_data$Category == "aggregate", ]$x, 
             xmax = segment_data[segment_data$Category == "aggregate", ]$xend, 
             ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "red") + 
    theme(legend.position = "bottom") + 
    ylim(NA, upper_ylim) + 
    xlab("p") + 
    ggtitle(label = title, subtitle = subtitle)
  
}

.plot_hdi_multiple_groups <- function(happy_hdi, title = "Two groups") {
  
  # subset aggregate replicate
  sdf <- happy_hdi %>% filter(replicate_id == ".aggregate")
  
  # validate input
  if (dim(sdf)[1] != length(unique(happy_hdi$Group.Id))) {
    stop("Too many data points provided; revise input data.frame")
  }
  
  # plot
  x <- seq(0, 1, length = 100)
  line_data <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = x, 
                    density = dbeta(x, sdf$alpha1[i], sdf$beta1[i]),
                    Group.Id = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  upper_ylim <- max(line_data$density[!is.infinite(line_data$density)])
  lower_ylim <- -max(line_data$density[!is.infinite(line_data$density)])/3
  padding <- abs(lower_ylim/(dim(sdf)[1] + 1))
  
  segment_data <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = sdf$lower[i], 
                    xend = sdf$upper[i], 
                    y = 0 - i * padding, 
                    yend = 0 - i * padding, 
                    Group.Id = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  point_data_observed <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = sdf$observed_p[i], 
                    y = 0 - i * padding, 
                    Group.Id = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  point_data_estimated <- lapply(1:dim(sdf)[1], function(i) {
    d <- data.frame(x = sdf$estimated_p[i], 
                    y = 0 - i * padding, 
                    Group.Id = sdf$Group.Id[i])
    d$Group.Id <- as.character(d$Group.Id)
    d
  }) %>% bind_rows()
  
  subtitle <- happy_hdi %>% 
    filter(replicate_id != ".aggregate") %>% 
    group_by(Group.Id) %>% 
    summarise(n = n(), 
              s = paste(successes, collapse = ","), 
              t = paste(totals, collapse = ",")) %>% 
    mutate(subtitle = sprintf("%s - n: %d; s: %s; t: %s", Group.Id, n, s, t)) %>% 
    select(subtitle) %>% unlist() %>% paste(., collapse = "\n")
  
  ggplot() + 
    geom_line(data = line_data, 
              aes(x = x, y = density, group = Group.Id, color = Group.Id), 
              lwd = 1, lty = 1) + 
    geom_segment(data = segment_data, 
                 aes(x = x, y = y, xend = xend, yend = yend, color = Group.Id), 
                 lwd = 1) + 
    geom_point(data = point_data_estimated, 
               aes(x = x, y = y, color = Group.Id)) + 
    geom_point(data = point_data_observed, 
               aes(x = x, y = y, color = Group.Id), 
               pch = 4) + 
    ylim(NA, upper_ylim) + 
    xlab("p") + 
    ggtitle(label = title, subtitle = subtitle) + 
    theme(legend.position = "bottom")
  
}
