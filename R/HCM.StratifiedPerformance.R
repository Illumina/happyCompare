#' HCM.StratifiedPerformance
#'
#' Plot flowcell layout for builds in each group.
#'
#' @param happy_metrics A `data.frame` with happy metrics obtained with `load_happy_metrics()`.
#' @param title Plot title. Default: 'Stratified performance'.
#' @return A `list` of results.
#' @examples
#' \dontrun{PCM.TwoGroupFlowcellLayout(groupA, groupB)}
#' @export

HCM.StratifiedPerformance = function(happy_metrics, title = 'Stratified performance') {

  ## functions
  .prepare_data = function(happy_metrics) {
      happy_avg = calculate_average_performance(happy_metrics[happy_metrics$Subset.Level < 2,])

      id.vars = c("Group", "Type", "Subset", "Subset.Level", "Subset.Size", "TRUTH.TOTAL")
      measure.vars = c("N", "METRIC.Recall.mean", "METRIC.Precision.mean", 
                       "METRIC.F1_Score.mean", "METRIC.Frac_Assessed.mean")
      mhappy_avg = reshape2::melt(happy_avg[ ,c(id.vars, measure.vars)], 
                                  id.vars = id.vars, measure.vars = measure.vars)
      
      return(mhappy_avg)
  }
  
  .prepare_plot_title = function(data, title) {
      plot_title = unique(data[data$variable == 'N', c('Group', 'value')])
      plot_title = paste(paste(plot_title$Group, plot_title$value, sep=' N='), collapse = '; ')
      plot_title = paste0(title, ' (', plot_title, ')')
      return(plot_title)
  }
  
  .plot_size = function(mhappy_avg) {
    data = unique(mhappy_avg[,c("Type", "Subset", "Subset.Size")])
    data[data$Subset.Size==0,]$Subset.Size = NA
    data$total_kb = round(data$Subset.Size/10^3)
    data$log10_total_kb = log10(data$total_kb)
    data$Group = rep("Size (kb)", dim(data)[1])
    ggplot(data, aes("", Subset)) +
      geom_tile(aes(fill = log10_total_kb)) +
      geom_text(aes(label = round(total_kb, 2)), color="white") +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks=element_blank()) +
      theme(legend.position="none") +
      theme(panel.background = element_rect(fill = "white")) +
      xlab("") +
      ylab("") +
      ggtitle("Subsets") +
      facet_grid(Type ~ Group, switch = "y")
  }

  .plot_truth_total = function(mhappy_avg) {
    data = unique(mhappy_avg[,c("Type", "Subset", "TRUTH.TOTAL")])
    data$log10_truth_total = log10(data$TRUTH.TOTAL)
    data$Group = rep("N variants", dim(data)[1])
    ggplot(data, aes("", Subset)) +
      geom_tile(aes(fill = log10_truth_total)) +
      geom_text(aes(label = TRUTH.TOTAL), color="white") +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.position="none",
            strip.text.y = element_blank()) +
      theme(panel.background = element_rect(fill = "white")) +
      xlab("") +
      ylab("") +
      ggtitle("T.TOTAL") +
      facet_grid(Type ~ Group, switch = "y")
  }

  .plot_average = function(mhappy_avg, y) {
    m = mhappy_avg[mhappy_avg$variable == y,]
    p = ggplot(m, aes("", Subset)) +
      geom_tile(aes(fill = value)) +
      geom_text(aes(label = round(value*100, 2)), color="white") +
      scale_fill_gradient2(limits=c(0,1), low = ("deepskyblue3"), high = ("orange"), midpoint = 0.5, mid = "aquamarine4") +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text = element_text(size = 10)) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "none",
            strip.text.y = element_blank()) +
      theme(panel.background = element_rect(fill = "white")) +
      ylab("") +
      facet_grid(Type ~ Group) +
      ggtitle(gsub('METRIC.', '', y))
    return(p)
  }

  .plot_ratio = function(mhappy_avg, y) {
    m = mhappy_avg[mhappy_avg$variable == y,]
    d = reshape2::dcast(m, Type + Subset ~ Group + variable)
    d$ratio = round(d[,dim(d)[2]]/d[,dim(d)[2]-1], 4)
    group_1 = unlist(strsplit(colnames(d)[dim(d)[2] - 1], "_"))[1]
    group_2 = unlist(strsplit(colnames(d)[dim(d)[2] - 2], "_"))[1]
    title = paste0(group_1, "/", group_2)
    d$Group = rep(title, dim(d)[1])

    p = ggplot(d, aes("", Subset)) +
      geom_tile(aes(fill = ratio)) +
      geom_text(aes(label = ratio), color="black") +
      scale_fill_gradient2(low = "#D55E00", high = "#009E73", midpoint = 1, mid = "white") +
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(axis.text = element_text(size=10)) +
      theme(axis.title=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            legend.position="none",
            strip.text.y = element_blank()) +
      theme(panel.background = element_rect(fill = "white")) +
      ylab("") +
      facet_grid(Type ~ Group) +
      ggtitle("")
    return(p)
  }

  ## main
  if (dim(happy_metrics)[1] == 0) {
      stop("Input dataframe is empty.")
  }
  
  data = .prepare_data(happy_metrics)
  
  plot_title = .prepare_plot_title(data, title)
  plot = gridExtra::grid.arrange(
    .plot_size(data),
    .plot_truth_total(data),
    .plot_average(data, y = "METRIC.F1_Score.mean"),
    .plot_ratio(data, y = "METRIC.F1_Score.mean"),
    .plot_average(data, y = "METRIC.Precision.mean"),
    .plot_ratio(data, y = "METRIC.Precision.mean"),
    .plot_average(data, y = "METRIC.Recall.mean"),
    .plot_ratio(data, y = "METRIC.Recall.mean"),
    .plot_average(data, y = "METRIC.Frac_Assessed.mean"),
    .plot_ratio(data, y = "METRIC.Frac_Assessed.mean"),
    nrow = 1,
    widths = c(2.5, 1, rep(c(2,1), 4)),
    left = plot_title)
  
  result = list(
      data = list(raw = data), 
      plots = list(combined = plot)
      )
  return(result)
}
