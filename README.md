# portland-crime-project

## Motivation

To use data analysis and visualization techniques to identify pattterns in crime across the City of Portland using publicly available data.

## Installation

Necessary data sets and shapefiles are included in the Github repository.

Install R 4.1 or later (optionally, install RStudio as an IDE) and load the following packages.
```
install.packages(c("tidyverse", "sf", "gt", "gtExtras", "RColorBrewer"))
```
## Data
- ```CrimeData-2021.csv ``` Publicly available crime data from the City of Portland, encompasses all reported incidents during 2021.
- ```PDX_2020_ACS_chng.csv``` American Community Survey (ACS) 2020 data for the City of Portland, with neighborhood names fixed.

## Scripts
- ```arson.R``` Uses ACS and crime data to generate some neat maps and tables for reported arson incidents in Portland.

## Sources

U.S. Census Bureau, City of Portland Maps, GIS, and Open Data
