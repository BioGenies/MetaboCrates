# Complete values below limit of detection

Complete values below limit of detection

## Usage

``` r
complete_LOD(gathered_data, LOD_type, LOD_method, LOD_vals)
```

## Arguments

- gathered_data:

  a long-format data table.

- LOD_type:

  a character string specifying which LOD type to use for imputing
  values. Possible values are `"OP"` and `"calc"`.

- LOD_method:

  a character string specifying the imputation method to be applied to
  `< LOD` values, or `NULL` to change these values to `NA`. Available
  methods are: `halfmin`, `random`, `halflimit`, `limit`, `limit-0.2min`
  and `logspline`.

- LOD_vals:

  a long-format table containing limits of detection/quantification and
  corresponding plate bar codes.
