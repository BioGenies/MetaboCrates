# Convert weird values into `NA`'s

Convert weird values into `NA`'s

## Usage

``` r
check_values(metabolite_vals, special_signs = "auto")
```

## Arguments

- metabolite_vals:

  a [`character`](https://rdrr.io/r/base/character.html) vector
  containing measured metabolite values.

- special_signs:

  a character vector of permitted special signs that should not be
  converted into `NA`'s. Default to `"auto"`, which converts
  `< LOD`,`< LLOQ`, `> ULOQ`, `NA` and \\\infty\\, where ULOQ means
  upper limit of quantification, LLOQ means lower limit of
  quantification and LOD means limit of detection.
