devtools::load_all()

# NovaSeq PCR-Free vs. Nano - subset
samplesheet_path <- "vignettes/pcrfree_vs_nano.subset.csv"
happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE)
saveRDS(happy_compare, file = "vignettes/pcrfree_vs_nano.subset.Rds")

# NovaSeq PCR-Free vs. Nano - full dataset
samplesheet_path <- "vignettes/pcrfree_vs_nano.csv"
happy_compare <- read_samplesheet(samplesheet_path, lazy = TRUE)
saveRDS(happy_compare, file = "vignettes/pcrfree_vs_nano.Rds")
