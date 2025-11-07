# Complete not quantified values

`complete_data()` completes missing values related to the limits of
quantification or detection.

## Usage

``` r
complete_data(
  dat,
  LOD_method = NULL,
  LLOQ_method = NULL,
  ULOQ_method = NULL,
  LOD_type = "calc"
)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- LOD_method:

  a character string specifying the imputation method to be applied to
  `< LOD` values, or `NULL` to change these values to `NA`. Available
  methods are: `halfmin`, `random`, `halflimit`, `limit`, `limit-0.2min`
  and `logspline`.

- LLOQ_method:

  a character string specifying the imputation method to be applied to
  `< LLOQ` values, or `NULL` if these values should not be imputed.
  Currently, the only available method is: `limit`.

- ULOQ_method:

  a character string specifying the imputation method to be applied to
  `> ULOQ` values, or `NULL` if these values should not be imputed.
  Available methods are: `limit`, `third quartile` and `scaled random`.

- LOD_type:

  a character string specifying which LOD type to use for imputing
  values. Possible values are `"OP"` and `"calc"`.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- complete_data(dat)
#> Skipping < LOD imputation.
#> Skipping < LLOQ imputation.
#> Skipping > ULOQ imputation.
```
