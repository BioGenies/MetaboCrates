# Calculate third quartile ignoring character values

`general_third_quartile()` calculates third quartile without values such
as NA's, values below or above LOD limit and so on.

## Usage

``` r
general_third_quartile(x)
```

## Arguments

- x:

  a vector of observations.

## Examples

``` r
x <- c("<LOD", 5, 6, NA, 9, 16)
MetaboCrates:::general_third_quartile(x)
#> 75% 
#>   9 
```
