# set up
library(happyCompare)

# load extended metrics
config_path = "K:/stratify/R/config.gene_sets.subset.txt"
happy_metrics = load_happy_metrics(config_path, L1_subsets = c('hgnc_exons', 'hgnc_genes'))

# retrieve top differing subsets across groups for SNPs
tds = HCM.TopDifferingSubsets(happy_metrics = happy_metrics, variant_type = 'SNP')
length(tds$data$raw)

# plot an example
subset_id = 'hgnc_exons_TAS2R43_1'
ci = HCM.ConfidenceIntervals(subsetted_happy_metrics = happy_metrics[happy_metrics$Subset == subset_id,], 
                             variant_type = 'SNP')
names(ci$plots)
pdf(file = 'examples/example2.pdf', width = 15, height = 3.5)
grid::grid.draw(ci$plots$combined)
dev.off()
