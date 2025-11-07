# Heatmap of correlations between metabolites

Heatmap of correlations between metabolites

## Usage

``` r
create_correlations_heatmap(
  dat,
  type = "completed",
  threshold = 0.3,
  metabolites_to_display = "all",
  interactive = TRUE
)
```

## Arguments

- dat:

  a `raw_data` object, the output of the
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function. The data have to be completed, for example using the
  [`complete_data()`](http://biogenies.info/MetaboCrates/reference/complete_data.md)
  function.

- type:

  default to `completed`, which creates a heatmap of correlations
  between the metabolites after imputation. If `both`, then correlations
  between the metabolites before and after imputation are shown.

- threshold:

  a numeric value specifying the minimum absolute correlation to display
  (only metabolites specified in metabolites_to_display are taken into
  account).

- metabolites_to_display:

  a vector of names or number of metabolites to display. If a number is
  provided, the first metabolites are selected. Defaults to `all`.

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
print(create_correlations_heatmap(dat))
print(create_correlations_heatmap(dat, type = "both"))
#> Warning: There were 8 warnings in `mutate()`.
#> The first warning was:
#> ℹ In argument: `across(everything(), suppressWarnings(as.numeric))`.
#> Caused by warning:
#> ! NAs introduced by coercion
#> ℹ Run `dplyr::last_dplyr_warnings()` to see the 7 remaining warnings.
```
