# Add metabolites to the attribute `removed`

`remove_metabolites()` remove the specified metabolites from data by
adding them to the attribute `removed`.

## Usage

``` r
remove_metabolites(dat, metabolites_to_remove, type)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- metabolites_to_remove:

  a character string or vector specifying the names of metabolites to
  remove.

- type:

  a character string specifying the criterion used to evaluate whether a
  metabolite should be removed. Can be `LOD` or `QC`.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)
attr(remove_metabolites(test_dat, "C0", "LOD"), "removed")
#> $LOD
#> [1] "C0"
#> 
#> $QC
#> NULL
#> 
```
