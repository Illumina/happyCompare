# set up
library(happyCompare)

# load extended metrics
config_path = "K:/stratify/R/config.gene_sets.subset.txt"
happy_metrics = load_happy_metrics(config_path, L1_subsets = c('hgnc_exons', 'hgnc_genes'))

# plot average performance per group for high level subsets
result = HCM.StratifiedPerformance(happy_metrics = happy_metrics, title = 'NA12877')
names(result$plots)
pdf(file = 'examples/example1.pdf', width = 19, height = 3)
grid::grid.draw(result$plots$combined)
dev.off()