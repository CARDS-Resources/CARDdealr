
# CARDdealr

<!-- badges: start -->
<!-- badges: end -->

For researchers and administrators trying to understand and 
describe the populations they represent or work with, a necessary and
often challenging first step is to identify and then access data. 
This package provides a ready-to-
use catalog of functions that access high-value epidiomiology and public
health datasets. While the package caters to cancer population
health and NCI Cancer Center data science needs, the data resources are 
likely of general use to anyone doing this type of work in any field.

## Installation

You can install the development version of CARDdealr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CARDS-Resources/CARDdealr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(CARDdealr)
lung_cancer_screening_locations = src_acr_lung_cancer_screening_data()
```

