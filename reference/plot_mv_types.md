# Barplot of missing values types

`plot_mv_types()` creates a barplot showing the count of missing values
for each type.

## Usage

``` r
plot_mv_types(dat)
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
test_dat <- read_data(path)
plot_mv_types(test_dat)

```
