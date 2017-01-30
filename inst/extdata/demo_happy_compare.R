library("happyCompare")
library("ggplot2")
theme_set(theme_bw())
library("dplyr")

# inspect the pre-loaded demo_haplocompare dataset
class(demo_haplocompare)

# we can see that the config contains custom metadata fields
config = demo_haplocompare$config
config

# let's use the information from these fields to answer a few questions about our experimental design
# Q: how many replicates do we have per sample?
config %>%
    select(sample_id) %>%
    table() %>%
    data.table::data.table() %>%
    knitr::kable()

# Q: how are replicates distributed within the 96-well plate?
pdf(file = 'inst/extdata/demo_haplocompare.plate_layout.pdf', width = 10, height = 4)
config %>%
    mutate(platebarcode = gsub('_.*', '', replicate_id),
           column = factor(substrRight(replicate_id, 2), levels = rev(sort(c("01", "02", "03", "04", "05", "06", "07", "08", "09",
                                                                             "10", "11", "12")))),
           row = factor(substr(substrRight(replicate_id, 3), 1, 1), levels = LETTERS[1:8])) %>%
    ggplot(aes(x = row, y = column)) +
    geom_tile(aes(fill = sample_id), color = 'white') +
    theme(legend.title = element_blank(), legend.position = "bottom") + 
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    xlab('') +
    ylab('') +
    facet_grid(platebarcode ~ group_id)
dev.off()

# Q: how many lanes have been used in each build?
config %>%
    mutate(n_lanes = sapply(lanes, function(x) length(unlist(strsplit(x, split = '\\+'))))) %>%
    select(replicate_id, n_lanes) %>%
    table() %>%
    data.table::data.table() %>%
    knitr::kable()

# Q: what is the flowcell layout for each replicate?
expanded_lanes = lapply(seq_along(config$lanes), function(i) {
    replicate_id = config$replicate_id[i]
    lanes_str = config$lanes[i]
    expand_lanes(replicate_id = replicate_id, lanes_str = lanes_str)
}) %>%
    dplyr::bind_rows() %>%
    unique()

pdf(file = 'inst/extdata/demo_haplocompare.flowcell_layout.pdf', width = 10, height = 4)
expanded_lanes %>%
    ggplot(aes(lane, flowcell)) +
    geom_tile(color = 'white') +
    scale_x_discrete(drop = FALSE) +
    facet_grid(replicate_id ~ machine, scales = "free_y", space = "free")
dev.off()
