# Group data

`add_group()` groups data by one or more columns.

## Usage

``` r
add_group(dat, group_names)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- group_names:

  a single character string or character vector specifying the names of
  the columns to group the data by.

## Value

[`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
object.

## See also

[`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")

dat <- add_group(dat, c("group", "species"))
#> Warning: You already have grouping defined in your data. It will be replaced!
```
