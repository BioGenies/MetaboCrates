# Show missing values ratios without those in the removed metabolites

`show_ratios()` returns LOD ratios without metabolites in removed
attribute.

## Usage

``` r
show_ratios(dat)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- remove_metabolites(dat, "C0", type = "LOD")
show_ratios(dat)
#> # A tibble: 8 × 2
#>   metabolite    NA_frac
#>   <chr>           <dbl>
#> 1 C2             0.0333
#> 2 C3             0.1   
#> 3 C3-DC (C4-OH)  1     
#> 4 C3-OH          1     
#> 5 C3:1           0.8   
#> 6 C4             0.0667
#> 7 C4:1           1     
#> 8 C5             0.133 
```
