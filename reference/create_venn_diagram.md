# Venn diagram for group levels

This function creates Venn diagram, showing counts of metabolites having
ratios of missing values larger than the given threshold for each group
level. Function works only when group has up to 4 levels.

## Usage

``` r
create_venn_diagram(dat, threshold)
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

- threshold:

  a minimum ratio of metabolite missing values in one group level for
  metabolite to be included in the diagram, given as decimal.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")
create_venn_diagram(dat, 0.1)
#> Warning: Although not display in plot, outside elements are still count in percentages.

```
