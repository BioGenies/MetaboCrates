# Complete values above limit of quantification

`complete_ULOQ()` imputes values below limit of quantification ("\>
ULOQ").

## Usage

``` r
complete_ULOQ(gathered_data, ULOQ_method, LOD_vals)
```

## Arguments

- gathered_data:

  a long-format data table.

- ULOQ_method:

  a character string specifying the imputation method to be applied to
  `> ULOQ` values, or `NULL` if these values should not be imputed.
  Available methods are: `limit`, `third quartile` and `scaled random`.

- LOD_vals:

  a long-format table containing limits of detection/quantification and
  corresponding plate bar codes.
