# Complete values below limit of quantification

`complete_LLOQ()` imputes values below limit of quantification ("\<
LLOQ").

## Usage

``` r
complete_LLOQ(gathered_data, LLOQ_method, LOD_vals)
```

## Arguments

- gathered_data:

  a long-format data table.

- LLOQ_method:

  a character string specifying the imputation method to be applied to
  `< LLOQ` values, or `NULL` if these values should not be imputed.
  Currently, the only available method is: `limit`.

- LOD_vals:

  a long-format table containing limits of detection/quantification and
  corresponding plate bar codes.
