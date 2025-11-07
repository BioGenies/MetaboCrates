# Beeswarm Plot of Metabolite Values

This function creates a beeswarm plot for a specified metabolite,
allowing visualization of the distribution of metabolite values across
different plate bar code.

## Usage

``` r
create_beeswarm_plot(dat, metabolite)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- metabolite:

  a character string specifying the name of the metabolite for which the
  beeswarm plot should be created.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
path <- get_example_data("two_sets_example.xlsx")
dat <- read_data(path)
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 16240 < LOD values...
#> No < LLOQ values found.
#> No > ULOQ values found.
create_beeswarm_plot(dat, "C0")

```
