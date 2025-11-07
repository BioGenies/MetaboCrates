# raw_data class

raw_data class

## Usage

``` r
raw_data(metabolomics_matrix, LOD_table, metabolites, group = NULL)
```

## Arguments

- metabolomics_matrix:

  a [`data.frame`](https://rdrr.io/r/base/data.frame.html), matrix
  containing biocrates data

- LOD_table:

  a [`list`](https://rdrr.io/r/base/list.html) containing two elements:

  - `table`: LOD table of limits of detection / quantification,

  - `types`: character vector of types of values, for example calc.
    meaning calculated from samples and op, received from operator.

- metabolites:

  a [`character`](https://rdrr.io/r/base/character.html), vector of
  metabolites names contained in the data.

- group:

  a `NULL` indicating no grouping or a vector of
  [`character`](https://rdrr.io/r/base/character.html) names of group
  columns. Default to NULL. The code will throw an error in the case
  when:

  - column of such a name won't be contained in the dataset,

  - there will be NA's in the grouping column,

  - any group will have less than 2 observations.

## Value

`raw_data` object metabolomics matrix with the following attributes:

- `LOD_table`

- `NA_info`: a [`list`](https://rdrr.io/r/base/list.html) related to
  missing values in the data. It contains `NA_ratios`: fractions of
  missing values per every metabolite. When `group` parameter (see
  below) is provided and `counts`: table of types of missing values with
  their counts (related to "sample" type samples).

- `metabolites`

- `samples`: a [`data.frame`](https://rdrr.io/r/base/data.frame.html)
  containing names of samples types and their counts

- `group` : a character name of group from the table

- `removed`: a list of removed metabolites

- `plate bar code`: each measurement has two plate bar codes depending
  on the method used
