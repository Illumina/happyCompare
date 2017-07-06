#!/usr/bin/env Rscript

######################
## FUNCTIONS
#####################
parse_args = function() {
  required = c("q1", "q2", "q3", "x_from", "x_to", "x_by", "r", "n_from", "n_to", "n_by",
               "sample_size", "output_csv")
  
  option_list = list(
    # simulation parameters
    optparse::make_option("--q1", type = "numeric", default = NULL,
                          help = "Probability that the variant is never seen."),
    optparse::make_option("--q2", type = "numeric", default = NULL,
                          help = "Probability that the variant is seen with probability x."),
    optparse::make_option("--q3", type = "numeric", default = NULL,
                          help = "Probability that the variant is always seen."),
    optparse::make_option("--x_from", type = "numeric", default = NULL,
                          help = "Probability of seeing variants in q2: starting value."),
    optparse::make_option("--x_to", type = "numeric", default = NULL,
                          help = "Probability of seeing variants in q2: end value."),
    optparse::make_option("--x_by", type = "numeric", default = NULL,
                          help = "Probability of seeing variants in q2: increment."),
    optparse::make_option("--r", type = "numeric", default = NULL,
                          help = "Number of replicates."),
    optparse::make_option("--n_from", type = "numeric", default = NULL,
                          help = "Number of variants per subset: starting value."),
    optparse::make_option("--n_to", type = "numeric", default = NULL,
                          help = "Number of variants per subset: end value."),
    optparse::make_option("--n_by", type = "numeric", default = NULL,
                          help = "Number of variants per subset: increment."),
    optparse::make_option("--sample_size", type = "numeric", default = NULL,
                          help = "Number of samples to draw from the posterior for HDI calculation."),
    # output
    optparse::make_option(c("-o", "--output_csv"), type = "character", default = NULL,
                          help = "Path to output csv."),
    optparse::make_option(c("-f", "--force_overwrite"), action = "store_true", default = FALSE,
                          help = "Over-write existing results [default %default].")
    
  )
  names(option_list) = c(required, "force_overwrite")
  
  opt_parser = optparse::OptionParser(option_list = option_list)
  opt = optparse::parse_args(opt_parser)
  
  if (!all(required %in% names(opt))) {
    stop("Missing required arguments")
  }
  
  if (file.exists(opt$output_csv)) {
    if (opt$force_overwrite) {
      message(sprintf("[WARNING] Will over-write existing results: %s", opt$output_csv))
    } else {
      stop("Output csv already exists")
    }
  }
  
  return(opt)
}

simulate_dataset = function(opt) {
  message("[INFO] Starting simulation...")
  
  # define simulation params from args
  q = c(opt$q1, opt$q2, opt$q3)
  r = opt$r
  N = seq(opt$n_from, opt$n_to, opt$n_by)
  X = seq(opt$x_from, opt$x_to, opt$x_by)
  I = length(N) * length(X)
  set.seed(2017)
  
  # actual simulation
  sim_data = vector("list", I)
  i = 0
  for (n in N) {
    for (x in X) {
      # progress
      i = i + 1
      if (i %% 100 == 0) {
        message(sprintf("%d/%d iterations", i, I))
      }
      
      # simulate a sample
      expected_p = (n*q[3] + n*q[2]*x) / n
      sim_sample = sample(factor(c("never", "sometimes", "always")),
                          n, prob = q, replace = T) %>%
        table() %>%
        data.table::data.table() %>%
        tidyr::spread(data = ., key = ., value = N) %>%
        mutate(total = n,
               q_1 = q[1],
               q_2 = q[2],
               q_3 = q[3],
               x = x,
               expected_p = expected_p,
               subset_id = sprintf("subset_r:%s_n:%s_q1:%s_q2:%s_q3:%s_x:%s", r, n, q[1], q[2], q[3], x),
               sample_id = "SIM")
      
      # simulate replicates
      # replicates have the same parameters as the parent sample, but the observed TP will vary
      sim_rep = sim_sample %>%
        slice(rep(1, r)) %>%
        mutate(
          tp = always + rbinom(n(), sometimes, x),
          replicate_id = paste0("replicate_", 1:n())
        )
      
      # prepare data for happyCompare
      class(sim_rep) = c("happy_extended", class(sim_rep))
      
      # add eb estimates
      sim_eb = estimate_hdi(sim_rep, successes_col = "tp", totals_col = "total",
                            group_cols =  c("sample_id", "subset_id", "expected_p"),
                            aggregate_only = FALSE, sample_size = opt$sample_size)
      
      # save
      sim_data[[i]] = sim_eb
    }
  }
  
  sim_data = sim_data %>%
    bind_rows() %>%
    ungroup()
  
  return(sim_data)
}

save = function(df, output_csv) {
  write.csv(df, file = output_csv, row.names = FALSE, quote = FALSE)
}

######################
## MAIN
#####################
suppressMessages(library(dplyr))
suppressMessages(library(happyR))
suppressMessages(library(happyCompare))

opt = parse_args()
ds = simulate_dataset(opt = opt)
save(df = ds, output_csv = opt$output_csv)
message(sprintf("[DONE] Output: %s", opt$output_csv))