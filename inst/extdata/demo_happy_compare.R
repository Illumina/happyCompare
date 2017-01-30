library("happyCompare")
library("ggplot2")
theme_set(theme_bw())
library("dplyr")

# inspect the pre-loaded demo_happy_compare dataset
class(demo_happy_compare)

# metadata is stored in the config
config = demo_happy_compare$config
config

# how many replicates do we have per sample?
config %>%
    select(sample_id) %>%
    table()

# since replicate_id matches the format of a 96-well plate, let's plot its layout
pdf(file = 'examples/demo_happy_compare.pdf', width = 10, height = 4)
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

# from the lanes column we can see that we are dealing with multiplexed data; let's inspect
# how many lanes have been used in each build
config %>%
    mutate(n_lanes = sapply(lanes, function(x) length(unlist(strsplit(x, split = '\\+'))))) %>%
    select(replicate_id, n_lanes) %>%
    table()

# how many replicates have we sequenced per machine?
