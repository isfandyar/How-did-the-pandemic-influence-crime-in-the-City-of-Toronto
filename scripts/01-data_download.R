#### Preamble ####
# Purpose: Download dataset from opendatatoronto [https://open.toronto.ca]
# Author: Isfandyar Virani
# Data: 04 Feb 2022
# Contact: isfandyar.virani@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(dplyr)

#### Download data ####
# From: https://open.toronto.ca/dataset/neighbourhood-crime-rates/
## Get Packages ## 
package <- show_package("fc4d95a6-591f-411f-af17-327e6c5d03c7")

## Get Resources ##
# Get all resources for the package
resources <- list_package_resources("fc4d95a6-591f-411f-af17-327e6c5d03c7")


# Identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))


# Load the first datastore resource as a sample
raw_data <- filter(datastore_resources, row_number()==1) %>% get_resource()

#### Saving the dataset as .csv file####
write_csv(raw_data, 'starter_folder/inputs/data/raw_data.csv')
