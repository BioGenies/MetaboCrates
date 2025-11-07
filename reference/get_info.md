# Get information about data

`get_info()` returns information about the number of sample types,
missing values types and grouping (if applied).

## Usage

``` r
get_info(dat)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

## See also

[`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
cat(get_info(dat))
#> Data contains 11 sample types and 3 NA types.

dat <- add_group(dat, "group")
cat(get_info(dat))
#> Data contains 11 sample types and 3 NA types.
#> Groupping by: "group" (4 levels).
```
