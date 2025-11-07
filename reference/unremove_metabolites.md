# Removing the specified metabolites from the attribute `removed`

`unremove_metabolites()` flags the specified metabolites as not removed.

## Usage

``` r
unremove_metabolites(dat, metabolites)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- metabolites:

  a character string or vector specifying the names of metabolites to
  restore.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)
test_dat <- remove_metabolites(test_dat, c("C0", "C2"), "LOD")
test_dat <- remove_metabolites(test_dat, "C0", "QC")
attr(test_dat, "removed")
#> $LOD
#> [1] "C0" "C2"
#> 
#> $QC
#> [1] "C0"
#> 
attr(unremove_metabolites(test_dat, "C0"), "removed")
#> $LOD
#> [1] "C2"
#> 
#> $QC
#> NULL
#> 
```
