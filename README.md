
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chunky

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/jasenfinch/chunky/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jasenfinch/chunky/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/jasenfinch/chunky/branch/main/graph/badge.svg)](https://codecov.io/gh/jasenfinch/chunky?branch=main)
<!-- badges: end -->

> Programmatic R Markdown Code Chunk Generation

The goal of chunky is to enable quick and simple programmatic generation
of R Markdown code chunks.

## Installation

Install the `chunky` package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jasenfinch/chunky")
```

## Example

``` r
library(chunky)

chunk(a <- 1,
      b <- 2,
      a + b,
      label = 'example',
      chunk_options = list(eval = FALSE),
      text_above = 'Some example text above.',
      text_below = 'Some example text below.') 
#> Some example text above.
#> 
#> ```{r example, eval=FALSE}
#> a <- 1
#> b <- 2
#> a + b
#> ```
#> 
#> Some example text below.
```
