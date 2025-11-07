# Creation of validated raw_data class

The wrapper function for the constructor of the
[`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
and its validator. Used in
[`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
function.

## Usage

``` r
new_raw_data(
  metabolomics_matrix,
  LOD_table,
  NA_info,
  metabolites,
  samples,
  group,
  removed,
  completed,
  cv
)
```

## Value

[`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
object.
