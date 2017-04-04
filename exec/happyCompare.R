#!/usr/bin/env Rscript

## parse args
option_list = list(
    optparse::make_option(c("-i", "--input_template"), type = "character", default = NULL, 
                help = "Path to input template Rmd."),
    optparse::make_option(c("-s", "--samplesheet"), type = "character", default = NULL,
                help = "Path to happyCompare samplesheet"),
    optparse::make_option(c("-o", "--output_dir"), type = "character", default = NULL, 
                help = "Path to output directory."),
    optparse::make_option(c("-r", "--root_dir"), type = "character", default = ".", 
                          help = "Path to results root. Default: '.'")
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
                  params = list(samplesheet = opt$samplesheet, root_dir = opt$root_dir))
message(sprintf("\n[DONE] Output: %s", opt$output_dir))