# Get models summary

`get_models_info()` returns the summaries for models returned by
[`build_models()`](http://biogenies.info/MetaboCrates/reference/build_models.md).

## Usage

``` r
get_models_info(models)
```

## Arguments

- models:

  the object returned by
  [`build_models()`](http://biogenies.info/MetaboCrates/reference/build_models.md).

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 109 < LOD values...
#> Completing 6 < LLOQ values...
#> Completing 9 < ULOQ values...
models <- build_models(dat, "group", "2")
#> Warning: SLOPE regularization cannot be performed -
#>                   using ridge regression instead
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
get_models_info(models)
#> $data
#> $data$full
#> # A tibble: 25 × 14
#>    group_2    C0    C2    C3 `C3-DC (C4-OH)` `C3-OH` `C3:1`    C4    C5 .fitted
#>    <fct>   <dbl> <dbl> <dbl>           <dbl>   <dbl>  <dbl> <dbl> <dbl>   <dbl>
#>  1 1        45.1  5.28 0.352            0.17   0.019  0.019 0.217 0.157   0.499
#>  2 1        46.2  7.88 0.495            0.17   0.019  0.015 0.248 0.231   0.241
#>  3 1        43.3 13    0.457            0.17   0.019 10     0.378 0.174   1.000
#>  4 1        30.1 10.5  0.191            0.17   0.019 10     0.107 0.087   1.000
#>  5 1        31.5 11.3  0.243            0.17   0.019 10     0.161 0.087   1.000
#>  6 1        38.6  6.87 0.285            0.17   0.019 10     0.107 0.105   1.000
#>  7 1        46.3 14    0.491            0.17   0.019 10     0.283 0.191   1.000
#>  8 1        41.8  9.3  0.289            0.17   0.019  0.019 0.187 0.203   0.297
#>  9 1        52    9.55 0.544            0.17   0.019  0.019 0.322 0.182   0.478
#> 10 1        23.4  5.31 0.181            0.17   0.019  0.019 0.246 0.109   0.283
#> # ℹ 15 more rows
#> # ℹ 4 more variables: .resid <dbl>, .hat <dbl>, .cooksd <dbl>, .std.resid <dbl>
#> 
#> $data$reduced
#>    group_2     C3   C3:1    fitted      resid
#> 1        1  0.352  0.019 0.2814609  1.5923326
#> 2        1  0.495  0.015 0.2810782  1.5931870
#> 3        1  0.457 10.000 0.8765453  0.5133554
#> 4        1  0.191 10.000 0.8766926  0.5130280
#> 5        1  0.243 10.000 0.8766638  0.5130920
#> 6        1  0.285 10.000 0.8766405  0.5131437
#> 7        1  0.491 10.000 0.8765265  0.5133973
#> 8        1  0.289  0.019 0.2815262  1.5921871
#> 9        1  0.544  0.019 0.2812622  1.5927762
#> 10       1  0.181  0.019 0.2816380  1.5919376
#> 11       0  0.425  0.019 0.2813854 -0.8129330
#> 12       0  0.473  0.019 0.2813357 -0.8128479
#> 13       0  0.200  0.019 0.2816183 -0.8133318
#> 14       0  0.200  0.019 0.2816183 -0.8133318
#> 15       0  0.422  0.019 0.2813885 -0.8129383
#> 16       0  0.738  0.013 0.2807096 -0.8117759
#> 17       0  0.378  0.019 0.2814340 -0.8130163
#> 18       0  0.558  0.017 0.2811303 -0.8124964
#> 19       0  0.606  0.019 0.2811980 -0.8126123
#> 20       0  0.386  0.019 0.2814257 -0.8130021
#> 21       0  0.328  0.019 0.2814858 -0.8131049
#> 22       0  0.892  0.019 0.2809022 -0.8121057
#> 23       0  0.353  0.019 0.2814599 -0.8130606
#> 24       0 10.000  0.014 0.2712930 -0.7955923
#> 25       0  0.236  0.019 0.2815811 -0.8132680
#> 
#> 
#> $summary
#> $summary$general
#>   null.deviance df.null nobs
#> 1      33.65058      24   25
#> 
#> $summary$full
#> # A tibble: 1 × 5
#>   logLik   AIC   BIC deviance df.residual
#>    <dbl> <dbl> <dbl>    <dbl>       <int>
#> 1  -9.29  36.6  47.5     18.6          16
#> 
#> $summary$reduced
#>      logLik      AIC      BIC deviance df.residual
#> 1 -11.93996 27.87992 73.87992 23.87992          23
#> 
#> 
#> $coefficients
#> $coefficients$full
#> # A tibble: 9 × 5
#>   term          estimate std.error statistic p.value
#>   <chr>            <dbl>     <dbl>     <dbl>   <dbl>
#> 1 (Intercept)    -1.25     198.     -0.00632   0.995
#> 2 C0              0.0959     0.119   0.808     0.419
#> 3 C2             -0.0478     0.337  -0.142     0.887
#> 4 C3             -3.40       4.87   -0.697     0.486
#> 5 C3-DC (C4-OH)  -0.196     49.6    -0.00395   0.997
#> 6 C3-OH         -42.2    10384.     -0.00406   0.997
#> 7 C3:1            1.15      10.1     0.114     0.909
#> 8 C4              3.43      11.9     0.289     0.773
#> 9 C5             -9.97      16.6    -0.599     0.549
#> 
#> $coefficients$reduced
#> # A tibble: 3 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 (Intercept) -0.941  
#> 2 C3          -0.00512
#> 3 C3:1         0.290  
#> 
#> 
```
