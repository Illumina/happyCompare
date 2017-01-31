library("happyCompare")
library("ggplot2")
theme_set(theme_bw())
library("dplyr")

# inspect the pre-loaded dataset
class(demo_sompy_stats)
print(demo_sompy_stats)

# inspect sompy stats results
dst = demo_sompy_stats$sompy_stats
class(dst)
dst

# apply sompy_stats methods
head(tidy(dst))

pdf(file = 'inst/extdata/demo_sompy_stats.pdf', width = 12, height = 5)
plot(dst, type = 'SNVs')
dev.off()

pdf(file = 'inst/extdata/demo_sompy_stats.af.pdf', width = 12, height = 5)
plot_af(dst, type = 'SNVs')
dev.off()
# 
# summary(dst, type = 'SNP')