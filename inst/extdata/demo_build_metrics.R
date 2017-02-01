library("happyCompare")
library("ggplot2")
library("dplyr")
library("data.table")

# inspect the pre-loaded demo_build_metrics dataset
class(demo_build_metrics)
print(demo_build_metrics)

# inspect the build metric results
bm = demo_build_metrics$build_metrics
class(bm)
head(bm)

# we can extract selected metrics using tidy(), then use as needed
tidy(bm, metrics = c('autosome_mean_coverage', 'percent_q30_bases'))
# ...

# if there are conflicts in the metric names, we can fix them with rename_metrics()
tidy(bm, metrics = c('metrics_version', 'percent_q30_bases'))

bm[['B-NA12877-LP1100076-DNA_A01']]

metrics_map = list(
    data.table(
        old_name = 'MetricsVersion',
        new_name = 'metrics_version'
    ),
    data.table(
        old_name = 'percent_q30_bases',
        new_name = '% Q30 bases'
    )
) %>% 
    bind_rows()

renamed_bm = rename(bm, metrics_map = metrics_map)
tidy(renamed_bm, metrics = c('metrics_version', '% Q30 bases'))