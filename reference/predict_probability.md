# Predict probabilities

`predict_probability()` returns the provided dataset with predicted
probabilities for the response variable.

## Usage

``` r
predict_probability(model, new_dat = NULL)
```

## Arguments

- model:

  a model object, the output of
  [`build_model()`](http://biogenies.info/MetaboCrates/reference/build_model.md).

- new_dat:

  a cleaned metabolomics or compounds matrix. If no dataset is provided,
  predictions for the test dataset are returned.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 109 < LOD values...
#> Completing 6 < LLOQ values...
#> Completing 9 < ULOQ values...
model <- build_model(dat, "group", "2", "Lasso")
#> Warning: one multinomial or binomial class has fewer than 8  observations; dangerous ground
predict_probability(model)
#>    probability_2 sample identification 2   C0    C2    C3 C3-DC (C4-OH) C3-OH
#> 1      0.1045564       K_Biocrates_4_1 1 45.1  5.28 0.352          0.17 0.019
#> 2      0.1167782       K_Biocrates_4_2 1 46.2  7.88 0.495          0.17 0.019
#> 3      0.8492329      K_Biocrates_4_18 1 30.1 10.50 0.191          0.17 0.019
#> 4      0.8253519      K_Biocrates_4_20 1 38.6  6.87 0.285          0.17 0.019
#> 5      0.1255293      K_Biocrates_4_24 1 52.0  9.55 0.544          0.17 0.019
#> 6      0.1046923      K_Biocrates_4_25 1 23.4  5.31 0.181          0.17 0.019
#> 7      0.1295654      K_Biocrates_4_51 0 44.6 10.30 0.200          0.17 0.019
#> 8      0.1209570      K_Biocrates_4_56 0 42.2  8.69 0.558          0.17 0.019
#> 9      0.1046470      K_Biocrates_4_69 0 34.4  5.30 0.328          0.17 0.019
#> 10     0.1136235      K_Biocrates_4_76 0 36.3  7.21 0.236          0.17 0.019
#>      C3:1    C4    C5
#> 1   0.019 0.217 0.157
#> 2   0.015 0.248 0.231
#> 3  10.000 0.107 0.087
#> 4  10.000 0.107 0.105
#> 5   0.019 0.322 0.182
#> 6   0.019 0.246 0.109
#> 7   0.019 0.200 0.245
#> 8   0.017 0.287 0.238
#> 9   0.019 0.189 0.222
#> 10  0.019 0.153 0.135
```
