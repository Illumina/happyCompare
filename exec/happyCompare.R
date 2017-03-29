#!/usr/bin/env Rscript

## parse args
option_list = list(
    optparse::make_option(c("-i", "--input_template"), type = "character", default = NULL, 
                help = "Path to input template Rmd."),
    optparse::make_option(c("-s", "--samplesheet"), type = "character", default = NULL,
                help = "Path to happyCompare samplesheet"),
    optparse::make_option(c("-o", "--output_dir"), type = "character", default = NULL, 
                help = "Path to output directory.")
)
opt_parser = optparse::OptionParser(option_list = option_list)
opt = optparse::parse_args(opt_parser)
if (any(sapply(opt, is.null))) {
    optparse::print_help(opt_parser)
    stop("Missing required arguments", call. = FALSE)
}

## generate report
rmarkdown::render(input = opt$input_template, 
                  output_dir = opt$output_dir, 
                  params = list(samplesheet = opt$samplesheet))
message(sprintf("\n[DONE] Output: %s", opt$output_dir))