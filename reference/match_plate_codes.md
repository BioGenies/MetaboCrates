# Find matching plate bar codes in the LOD table

Find matching plate bar codes in the LOD table

## Usage

``` r
match_plate_codes(LOD_table, sets)
```

## Arguments

- LOD_table:

  an `LOD_table` attribute from
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object.

- sets:

  an unique character vector of plate bar codes (for example
  '1036372116-1 \| 1036372121-1').

## Value

A long-format table containing limits of detection/quantification and
corresponding plate bar codes.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
sets <- unique(dat[["plate bar code"]])
MetaboCrates:::match_plate_codes(attr(dat, "LOD_table"), sets)
#> # A tibble: 54 × 4
#> # Groups:   compound, plate bar code [18]
#>    compound `plate bar code`            type        thresh_est
#>    <chr>    <chr>                       <chr>            <dbl>
#>  1 C0       1036372116-1 | 1036372121-1 LLOQ              0   
#>  2 C0       1036372116-1 | 1036372121-1 LOD (calc.)       3.1 
#>  3 C0       1036372116-1 | 1036372121-1 ULOQ              0   
#>  4 C0       1036372121-1                LLOQ              0   
#>  5 C0       1036372121-1                LOD (calc.)       0   
#>  6 C0       1036372121-1                ULOQ              0   
#>  7 C2       1036372116-1 | 1036372121-1 LLOQ              0   
#>  8 C2       1036372116-1 | 1036372121-1 LOD (calc.)       0.17
#>  9 C2       1036372116-1 | 1036372121-1 ULOQ            100   
#> 10 C2       1036372121-1                LLOQ              0   
#> # ℹ 44 more rows
```
