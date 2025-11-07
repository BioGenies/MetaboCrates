# Build linear models

`build_models()` fits full and penalized logistic models using the
specified grouping column as the outcome.

## Usage

``` r
build_models(dat, response, level = NULL)
```

## Arguments

- dat:

  a
  [`raw_data`](http://biogenies.info/MetaboCrates/reference/raw_data.md)
  object. Output of
  [`read_data()`](http://biogenies.info/MetaboCrates/reference/read_data.md)
  function.

- response:

  a string specifying the name of the response variable.

- level:

  a string specifying the name of the level to model when the response
  has more than two levels.

## Examples

``` r
path <- get_example_data("small_biocrates_example.xls")
dat <- read_data(path)
dat <- add_group(dat, "group")
dat <- complete_data(dat, "limit", "limit", "limit")
#> Completing 109 < LOD values...
#> Completing 6 < LLOQ values...
#> Completing 9 < ULOQ values...
build_models(dat, "group", "2")
#> Warning: SLOPE regularization cannot be performed -
#>                   using ridge regression instead
#> Warning: Option grouped=FALSE enforced in cv.glmnet, since < 3 observations per fold
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> $full
#> 
#> Call:  glm(formula = paste0(response, "~ ."), family = "binomial", data = clean_model_dat)
#> 
#> Coefficients:
#>     (Intercept)               C0               C2               C3  
#>        -1.24733          0.09586         -0.04783         -3.39808  
#> `C3-DC (C4-OH)`          `C3-OH`           `C3:1`               C4  
#>        -0.19557        -42.15797          1.15361          3.43253  
#>              C5  
#>        -9.97122  
#> 
#> Degrees of Freedom: 24 Total (i.e. Null);  16 Residual
#> Null Deviance:       33.65 
#> Residual Deviance: 18.57     AIC: 36.57
#> 
#> $reduced
#> 
#> Call:  glmnet(x = as.matrix(select(clean_model_dat, !all_of(response))),      y = clean_model_dat[[response]], family = "binomial", lambda = lambda_best,      foldid = foldid) 
#> 
#>   Df  %Dev Lambda
#> 1  2 29.04 0.0617
#> 
```
