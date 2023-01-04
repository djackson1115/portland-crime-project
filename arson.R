## SETUP ##

#install.packages(c("tidyverse", "sf", "RColorBrewer" ,"ggmap"))
library(tidyverse)
library(sf)
library(RColorBrewer)
library(gt)
library(gtExtras)

# rm(list = ls())
setwd("C:/Users/drewj/Downloads/Portland City Project")

# read in CSV file and import shapefiles for neighborhoods and streets
pdx.crime <- read_csv("Data/CrimeData-2021.csv")

pdx.shape <- read_sf("Shapefiles/pdx/hoods.shp")
st_crs(pdx.shape) # check crs 

pdx.sextants <- read_sf("Shapefiles/pdx/sextants.shp")
st_crs(pdx.sextants) # check crs 

# read in ACS data
pdx.acs <- read_csv("Data/PDX_2020_ACS_chng.csv")

# select relevant columns
pdx.acs <- pdx.acs %>%
  select(...1, POP20, edu_hsd, edu_gebach, edu_bach, Pov20, lt75000, ge75000, SVI, mobility) %>%
  rename(Neighborhood = ...1) %>% 
  filter(!(Neighborhood %in% c("Neighborhood", "MC Unclaimed #11", "MC Unclaimed #13", "MC Unclaimed #14", "MC Unclaimed #5"))) %>%
  drop_na()
  
## COORDINATE SYSTEMS & MAPPING ##

# Oregon SCPS North
or.plane.north.crs <- "+proj=lcc +lat_0=43.6666666666667 +lon_0=-120.5 +lat_1=44.3333333333333 +lat_2=46 +x_0=2500000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +type=crs"

# WGS 84 / Pseudo-Mercator CRS
wgs84.crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

# transform neighborhood shapefile
pdx.shape.t <- st_transform(pdx.shape, crs = st_crs(or.plane.north.crs))

# define function to get and fix coordinates from crime data
get_coords <- function(offense.cat, dataset){
  coords <- dataset %>%
    filter(OffenseCategory == offense.cat) %>%
    filter(!is.na(OpenDataX) | !is.na(OpenDataY))  %>% 
    select(OpenDataX, OpenDataY) %>%
    st_as_sf(coords = c("OpenDataX", "OpenDataY")) %>%
    st_set_crs(or.plane.north.crs) # define CRS
  return(coords)
}

# get coords for arson
ars.coords <- get_coords("Arson", pdx.crime)

# create map
ars.map <- ggplot() +
  geom_sf(data = pdx.shape, 
          linewidth = 0.55, 
          fill = "ghostwhite") +
  geom_sf(data = ars.coords, 
          colour = "red",
          size = 1.35, 
          alpha = 0.40, 
          show.legend = FALSE) + 
  theme_void()

ars.map

## DATA CLEANING ## 

# create table of neighborhoods
pdx.hoods <- distinct(pdx.crime, Neighborhood) %>%
  filter(!is.na(Neighborhood))

# define function to get get relevant info from crime data
get_data <- function(offense.cat, dataset){
  dat <- dataset %>%
    filter(OffenseCategory== offense.cat) %>%
    select(Neighborhood, OccurDate, OccurTime, OffenseCount)
}

# get data for arson
ars.data <- get_data("Arson", pdx.crime)

# group and summarize our data by neighborhood, drop NA values
ars.data <- ars.data %>%
  group_by(Neighborhood) %>%
  mutate(Neighborhood = ifelse(Neighborhood == "Buckman East" | Neighborhood == "Buckman West", 
                "Buckman",
                Neighborhood))  %>%
  summarise(total = sum(OffenseCount)) %>%
  drop_na() 

# join our data with the neighborhood list
ars.data <- full_join(ars.data, pdx.hoods, by = "Neighborhood") %>%
  mutate(total = replace_na(total, 0)) 

# compute total number of arson incidents
total.ars.inc <- sum(ars.data$total)

# join arson and ACS data
ars.plus.acs <- right_join(ars.data, pdx.acs, by = "Neighborhood") %>%
  mutate(total = ifelse(Neighborhood == "City of Portland",
                        total.ars.inc,
                        total),
         ars100k = round(total/POP20 * 100000, 1)) # create per capita column

# select relevant columns
table.data <- ars.plus.acs %>%
  select(Neighborhood, total, ars100k) %>%
  as_tibble() %>%
  arrange(desc(ars100k))

# create table
ars.table <- table.data %>%
  head(30) %>% # top 30 neighborhoods
  gt() %>%
  cols_label(total = "Incidents",
             ars100k = "Per 100k") %>%
  tab_header(title = "Worst Neighborhoods for Arson",
             subtitle = "Reported incidents from Jan. 1 - Dec. 31, 2021") %>%
  tab_footnote(footnote = "Sources: 2020 ACS - U.S. Census Bureau, Open Data - City of Portland") %>%
  gt_color_rows(ars100k,
                domain = range(min(table.data$ars100k), max(table.data$ars100k)),
                palette = "OrRd") %>%
  tab_row_group(label = "",
                rows = 30) %>%
  gt_theme_538()

ars.table  
