# Examples

## Interactive examples


## Static reports
The code below can be used to generate an example static report from R:
```
rmarkdown::render(input = 'inst/rmd/example_germline_report.Rmd', 
                  output_dir = 'inst/rmd/', 
                  params = list(config = 'data-raw/config.happy_summary.csv'))
```