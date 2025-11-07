# Q-Q plot

`create_empirical_qq_plot()` creates empirical Q-Q plot based on the
single metabolite distribution before and after imputation.

## Usage

``` r
create_empirical_qqplot(dat, metabolite)
```

## Arguments

- dat:

  a `raw_data` object, the output of the
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function. The data have to be completed, for example using the
  [`complete_data()`](http://biogenies.info/MetaboCrates/reference/complete_data.md)
  function.

- metabolite:

  a name of the metabolite of interest.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 109 < LOD values...
#> Completing 6 < LLOQ values...
#> Completing 9 < ULOQ values...
create_empirical_qqplot(dat, "C5")

```
