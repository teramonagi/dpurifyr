
<!-- README.md is generated from README.Rmd. Please edit that file -->
dpurifyr
========

[![Build Status](https://travis-ci.org/teramonagi/dpurifyr.svg?branch=master)](https://travis-ci.org/teramonagi/dpurifyr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dpurifyr)](http://cran.r-project.org/package=dpurifyr)

Overview
--------

dpurifyr package gives you a practical way for `data preprocessing` providing a consistent set of verbs that help you solve the most common data preprocessing challenges.

Installation
------------

You can install from CRAN with:

``` r
# Not yet
# install.packages("dpurifyr")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("teramonagi/dpurifyr")
```

Simple Example
--------------

The following example uses dpurifyr to solve a fairly realistic problem: apply different types of data preprocessing (`standard_scale` and `scale_minmax`) to columns (`dplyr::starts_with("Sepal")` and `Petal.Width`) selected by the way which is consistent with other tidyverse packages.

``` r
library("dpurifyr")
df <- head(iris)
# Create Data Pre-Processing chain while data preproessing for df is done.
pp <- dpurifyr::scale_standard(df, dplyr::starts_with("Sepal")) %>% 
  dpurifyr::scale_minmax(Petal.Width) 

# PreProcessing object(pp) behave like data.frame object
head(pp)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1    0.5206576   0.3401105          1.4           0  setosa
#> 2   -0.1735525  -1.1175060          1.4           0  setosa
#> 3   -0.8677627  -0.5344594          1.3           0  setosa
#> 4   -1.2148677  -0.8259827          1.5           0  setosa
#> 5    0.1735525   0.6316338          1.4           0  setosa
#> 6    1.5619728   1.5062037          1.7           1  setosa
```

Once you get `preprocessing` object, `preprocessing` object can be applied to the other data. This means that you can apply the same `preprocessing` with fixed parameter to other data.

``` r
# You can apply the same preprocessing to different data.frame
pp <- dpurifyr::scale_standard(head(iris), Sepal.Width, Petal.Length) %>% 
  dpurifyr::scale_standard(Sepal.Length) 
# Using the same parameters( `use_param=TRUE`) estimated in pp object.
pp2 <- dpurifyr::apply(head(iris, 10), pp, TRUE) 
pp2
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1     0.5206576  0.34011052   -0.3627381         0.2  setosa
#> 2    -0.1735525 -1.11750599   -0.3627381         0.2  setosa
#> 3    -0.8677627 -0.53445939   -1.0882144         0.2  setosa
#> 4    -1.2148677 -0.82598269    0.3627381         0.2  setosa
#> 5     0.1735525  0.63163382   -0.3627381         0.2  setosa
#> 6     1.5619728  1.50620373    1.8136906         0.4  setosa
#> 7    -1.2148677  0.04858722   -0.3627381         0.3  setosa
#> 8     0.1735525  0.04858722    0.3627381         0.2  setosa
#> 9    -1.9090779 -1.40902929   -0.3627381         0.2  setosa
#> 10   -0.1735525 -0.82598269    0.3627381         0.1  setosa
```

You can find more information in [vignettes](https://github.com/teramonagi/dpurifyr/blob/master/vignettes/introduction-to-dpurifyr.Rmd).

Contribution
------------

-   If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/teramonagi/dupurifyr/issues)

Citation
--------

To cite package `dpurifyr` in publications use:

    Nagi Teramo, Shinichi Takayanagi (2017). dpurifyr: A grammar of data preprocessing. R package version 0.1.0. https://github.com/teramonagi/dpurifyr

A BibTeX entry for LaTeX users is

    @Manual{,
      title = {dpurifyr: A grammar of data preprocessing},
      author = {Nagi Teramo, Shinichi Takayanagi},
      year = {2017}, 
      note = {R package version 0.1.0},
      url = {https://github.com/teramonagi/dpurifyr},
    }
