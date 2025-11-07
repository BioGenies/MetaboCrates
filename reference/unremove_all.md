# Setting the specified type in attribute `removed` to `NULL`

`unremove_all()` set all the metabolites from the specified type as not
removed.

## Usage

``` r
unremove_all(dat, type)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- type:

  a character string indicating the type from which to restore all
  metabolites.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
test_dat <- read_data(path)
test_dat <- remove_metabolites(test_dat, "C0", "LOD")
attr(test_dat, "removed")
#> $LOD
#> [1] "C0"
#> 
#> $QC
#> NULL
#> 
attr(unremove_all(test_dat, "LOD"), "removed")
#> $LOD
#> NULL
#> 
#> $QC
#> NULL
#> 
```
