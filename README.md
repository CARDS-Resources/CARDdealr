The CARDdealr package
================

<!-- badges: start -->
<!-- badges: end -->

For researchers and administrators trying to understand and describe the
populations they represent or work with, a necessary and often
challenging first step is to identify and then access data. This package
provides a ready-to- use catalog of functions that access high-value
epidiomiology and public health datasets. While the package caters to
cancer population health and NCI Cancer Center data science needs, the
data resources are likely of general use to anyone doing this type of
work in any field.

## Installation

You can install the development version of CARDdealr from
[GitHub](https://github.com/) with:

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

    ## INFO  [15:45:26.456] Starting acr_lung_cancer_screening_data

    ## Rows: 3590 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): 1 Facility Name, 2 Street Address, 3 city, 4 state, 5 ZIP Code, 6 P...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## INFO  [15:45:28.634] Completing acr_lung_cancer_screening_data

## Available datasets

``` r
available_datasets() |> knitr::kable()
```

| name                                                                        | accessor                           |
|:----------------------------------------------------------------------------|:-----------------------------------|
| American College of Radiology Lung Cancer Screening Registry (LCSR) dataset | src_acr_lung_cancer_screening_data |
| Bureau of Labor Statistics Unemployment data                                | src_bls_unemployment_data          |
| Behavioral Risk Factor Surveillance System (BRFSS)                          | src_brfss                          |
| CDC Places                                                                  | src_cdc_places_data                |
| EPA Toxic Release Inventory (TRI)                                           | src_epa_tri_data                   |
| Environmental Protection Agency Superfund                                   | src_epa_superfund_data             |
| FDA Certified Mammography Facilities Dataset                                | src_fda_mammography_data           |
| Health Resources and Services Administration (HRSA) facilities              | src_hrsa_facility_data             |
| National Plan & Provider Enumeration System (NPPES) GI providers            | src_nppes_gi_data                  |
| National Plan & Provider Enumeration System (NPPES) Oncology Providers      | src_nppes_onco_data                |
| CDC Social Vulnerability Index                                              | src_cdc_social_vulnerability_index |
| United States county health rankings                                        | src_us_county_health_rankings      |
