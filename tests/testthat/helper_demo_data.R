load(system.file("data", "freebayes.RData", package = "happyCompare"))
load(system.file("data", "gatk.RData", package = "happyCompare"))
load(system.file("data", "platypus.RData", package = "happyCompare"))

# add metadata - temporary
happyr_freebayes$summary = happyr_freebayes$summary %>% 
  mutate(Group.Id = "freebayes", Replicate.Id = "freebayes 1")
happyr_gatk$summary = happyr_gatk$summary %>% 
  mutate(Group.Id = "gatk", Replicate.Id = "gatk 1")
happyr_platypus$summary = happyr_platypus$summary %>% 
  mutate(Group.Id = "platypus", Replicate.Id = "platypus 1")

happy_result_list = c(happyr_freebayes, happyr_gatk, happyr_platypus)
