# Examples

## Interactive examples
Examples on how to use the different metric readers interactively are available under `demo_haplocompare.Rmd` (see [demo_haplocompare.md](demo_haplocompare.md) for the markdown output). 

All of the demo code relies on pre-saved data, which have been created from two different input configs (`config.haplocompare_[germline|somatic].csv`) using  `load_data.R`.


## Static reports
An example static report can be generated using the code below. Its output will be saved as [example_germline_report.md](example_germline_report.md).
```
# run from repo root
Rscript --vanilla exec/happyCompare.R --input_template inst/rmd/example_germline_report.Rmd --output_dir inst/examples --config inst/examples/config.haplocompare_germline.csv
```
