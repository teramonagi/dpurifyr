# dpurifyr

[![Build Status](https://travis-ci.org/teramonagi/dpurifyr.svg?branch=master)](https://travis-ci.org/teramonagi/dpurifyr)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dpurifyr)](http://cran.r-project.org/package=dpurifyr) 

## Overview
dpurifyr package gives you a practical way for `data preprocessing` providing a consistent set of verbs that help you solve the most common data preprocessing challenges.

## Installation

``` r
# Install from CRAN (not yet)
install.packages("dpurifyr")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("teramonagi/dpurifyr")
```

## Usage
```
a <- scale(head(iris), Sepal.Width, Petal.Length)
str(a)
a
pp <- dpurifyr::scale(head(iris), Sepal.Width, Petal.Length) %>%
  dpurifyr::scale(Sepal.Length)
print(pp)

```
## Contribution
If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/teramonagi/dupurifyr/issues)
