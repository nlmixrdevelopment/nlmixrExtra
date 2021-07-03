# nlmixrExtra

<!-- badges: start -->
[![R-CMD-check](https://github.com/nlmixrdevelopment/nlmixrExtra/workflows/R-CMD-check/badge.svg)](https://github.com/nlmixrdevelopment/nlmixrExtra/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/nlmixrExtra)](https://CRAN.R-project.org/package=nlmixrExtra)
[![Code_Coverage_Badge](http://codecov.io/github/nlmixrdevelopment/nlmixrExtra/coverage.svg?branch=master)](http://codecov.io/github/nlmixrdevelopment/nlmixrExtra?branch=master)
<!-- badges: end -->

The goal of nlmixrExtra is to extend the functionality of nlmixr.

## Installation

You can install the released version of nlmixrExtra from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("nlmixrExtra")
```

You can install the development version of nlmixrExtra from GitHub with:

``` r
install.packages("remotes")
remotes::install_github("nlmixrdevelopment/nlmixrExtra")
```

## Example

To replace part of a model with a standard model template, you can use code like
that below.

``` r
library(nlmixrExtra)
model <- function() {
  ini({
    lcl <- (0.01)
    lv <- log(0.5)
    eta_cl ~ 0.1
    add_err <- 0.1
  })
  model({
    linear_model_ode_onecmt_iv(cl=exp(lcl+eta_cl), v=exp(lv))
    cp <- central/exp(lv)
    cp ~ add(add_err)
  })
}
nlmixr_trans(model, linear_model_ode_onecmt_iv)
```

## Contributions Welcome!

The current, main goal of `nlmixrExtra` is to simplify model building by
providing model templates.  If you can think of a model method that could use a
template, please suggest it on the [issues
page](https://github.com/nlmixrdevelopment/nlmixrExtra/issues).  Or, if you're
feeling adventuresome, also prepare a pull request.
