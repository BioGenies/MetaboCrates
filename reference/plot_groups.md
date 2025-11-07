# Barplot of group level sizes

`plot_groups()` creates a barplot showing the count of observations in
each group level.

## Usage

``` r
plot_groups(dat)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object, the output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md),
  with group specified using the
  [`add_group()`](http://biogenies.info/MetaboCrates/reference/add_group.md)
  function.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")
plot_groups(dat)

```
