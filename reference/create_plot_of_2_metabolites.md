# Plot of two metabolites

Plot of two metabolites

## Usage

``` r
create_plot_of_2_metabolites(dat, metabolite1, metabolite2, interactive = TRUE)
```

## Arguments

- dat:

  a `raw_data` object, the output of the
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function. The data have to be completed, for example using the
  [`complete_data()`](http://biogenies.info/MetaboCrates/reference/complete_data.md)
  function.

- metabolite1:

  first metabolite name.

- metabolite2:

  second metabolite name.

- interactive:

  logical. If `TRUE` (default), a ggiraph interactive plot is returned;
  otherwise, a standard ggplot object is produced.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 109 < LOD values...
#> Completing 6 < LLOQ values...
#> Completing 9 < ULOQ values...
print(create_plot_of_2_metabolites(dat, "C0", "C2"))
```
