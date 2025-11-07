# Get CV to remove

`get_CV_to_remove()` returns metabolite names having more CV value than
the given threshold.

## Usage

``` r
get_CV_to_remove(dat, threshold)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- threshold:

  a decimal specifying the minimum CV a metabolite must have to be
  removed.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 109 < LOD values...
#> Completing 6 < LLOQ values...
#> Completing 9 < ULOQ values...
dat <- calculate_CV(dat)
get_CV_to_remove(dat, 0.3)
#> [1] "C3"   "C4"   "C4:1" "C5"  
```
