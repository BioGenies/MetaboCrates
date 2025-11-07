# Get metabolites to remove based on missing values proportion

`get_LOD_to_remove()` returns the names of metabolites having the higher
proportion of missing values than the given threshold.

## Usage

``` r
get_LOD_to_remove(dat, threshold = 0.8, use_group = TRUE)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- threshold:

  a decimal specifying the minimum proportion of missing values a
  metabolite must have to be removed.

- use_group:

  logical. If `TRUE`, a metabolite will be returned only if the
  proportion of missing values exceeds the threshold in every group
  level.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
get_LOD_to_remove(dat, 0.1)
#> No group to use! It will be ignored.
#>             If you want to use group provide it with add_group function first.
#> [1] "C3"            "C3-DC (C4-OH)" "C3-OH"         "C3:1"         
#> [5] "C4:1"          "C5"           
```
