library("happyCompare")
library("ggplot2")
theme_set(theme_bw())
library("dplyr")

# inspect the pre-loaded demo_happy_extended_ci dataset
class(demo_happy_extended_ci)
dim(demo_happy_extended_ci)
head(demo_happy_extended_ci)

# apply happy_extended_ci methods
pdf(file = 'examples/demo_happy_extended_ci.pdf', width = 6, height = 4)
plot_subset(demo_happy_extended_ci, subset = 'hgnc_genes_ANK3', type = 'SNP', metric = 'METRIC.Recall')
dev.off()
