# Barplot of missing values proportion in each metabolite

`plot_NA_percent()` creates a barplot showing the proportion of missing
values in each metabolite. Optionally, for every metabolite, it can
include the division by the type of missing values or display the
proportion of missing values in each group level.

## Usage

``` r
plot_NA_percent(dat, type = "joint", interactive = TRUE)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- type:

  a character string indicating which type of plot to return. This can
  be either "joint", "NA_type" or "group". Default type is "joint",
  which creates a plot of missing values percents in each metabolite.
  "NA_type" additionally splits missing values by type, and "group"
  includes the ratios in every group level.

- interactive:

  logical. If `TRUE` (default), a ggiraph interactive plot is returned;
  otherwise, a standard ggplot object is produced.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
print(plot_NA_percent(dat))
print(plot_NA_percent(dat, "NA_type"))
dat <- add_group(dat, "group")
print(plot_NA_percent(dat, "group"))
```
