# Calculate minimum ignoring character values

`general_min()` calculates minimum without values such as NA's, values
below or above LOD limit and so on.

## Usage

``` r
general_min(x)
```

## Arguments

- x:

  a vector of observations.

## Examples

``` r
x <- c("<LOD", 5, 6, NA, 9, 16)
MetaboCrates:::general_min(x)
#> [1] 5
```
