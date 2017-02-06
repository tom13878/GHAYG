#'========================================================================================================================================
#' Project:  DFID
#' Subject:  Script to create tables for policy paper #1
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "stargazer", "reshape2")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### TAB-1: TABLE WITH SUMMARY DATA FOR THE MAIZE SECTOR

# table function
tablr <- function(df, value=value){
  group_by(df, country) %>%
    summarise(`Last Recorded Year`=max(year),
              `Value in Last Year`=value[year==max(year)],
              `5 Year Average`=mean(value[order(year, decreasing=TRUE)][1:5]))
}

# Set target country
countries <- c("Ghana")

# -------------------------------------
# maize variables
# Yield: hg (hectogram)/ha
# area harvested: ha
# land variables are in units of 1000 ha
# -------------------------------------

# read in the maize and total land
# production data as separate files
maize <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_maize.csv"))
land <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_land.csv"))
maize$Area <- as.character(maize$Area)
land$Area <- as.character(land$Area)

# shorten the name of tanzania
maize$Area <- ifelse(maize$Area %in% "United Republic of Tanzania", "Tanzania", maize$Area)
land$Area <- ifelse(land$Area %in% "United Republic of Tanzania", "Tanzania", land$Area)

# Ethiopia was Ethiopia PDR
maize$Area <- ifelse(maize$Area %in% "Ethiopia PDR", "Ethiopia", maize$Area)
land$Area <- ifelse(land$Area %in% "Ethiopia PDR", "Ethiopia", land$Area)

# select only maize yield and create table
# hectograms are 0.1 kilograms -> convert
maize_yield <- select(maize, country=Area, variable = Element,
                      year=Year, value = Value) %>%
  filter(variable=="Yield")
maize_yield$value <- maize_yield$value * 0.1

# table of maize yield in kg/ha averaged
# across each country
maize_yield_table <- tablr(maize_yield)
maize_yield_table[maize_yield_table$country %in% countries, ]

# Make a table of the percentage of arable
# land used for maize production per country
# land used for maize is recorded in ha
# whereas total arable land is recorded in
# 1000 ha -> convert maize are
maize_area_harv <- select(maize, country=Area, variable = Element,
                          year=Year, value = Value) %>%
  filter(variable!="Yield")
maize_area_harv$value <- maize_area_harv$value/1000

# select land variables and join with
# maize variables to get a measure for
# maize share of total crop area
# note unit conversions

land <- select(land, country=Area, year=Year,
               variable = Item, value = Value) %>%
  spread(variable, value)

maize_area_harv <- left_join(maize_area_harv, land)
maize_area_harv <- transmute(maize_area_harv,
                             country, year,
                             value = value/`Arable land` * 100)
maize_area_share_table <- tablr(maize_area_harv)
maize_area_share_table[3:4] <- round(maize_area_share_table[3:4], 1)
maize_area_share_table[maize_area_share_table$country %in% countries, ]



# -------------------------------------
# fertilizer table of nitrogen
# use tonnes per 1000 ha per country and year
# and only on arable and permanent crop
# area
# -------------------------------------

fertilizer <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_fertilizer.csv"))
fertilizer$Country <- as.character(fertilizer$Country)
fertilizer$Country <- ifelse(fertilizer$Country %in% "United Republic of Tanzania",
                             "Tanzania", fertilizer$Country)

fertilizer <- select(fertilizer, country=Country, year=Year, value = Value) 
fert_table <- tablr(fertilizer)
fert_table[3:4] <- round(fert_table[3:4], 2)
fert_table

# -------------------------------------
# food balance table of self sufficiency
# ratios *100. Need to check whether
# total production is the correct variable 
# to use here.
# -------------------------------------

food_bal <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_food_balance.csv"))
food_bal$Country <- as.character(food_bal$Country)
food_bal$Country <- ifelse(food_bal$Country %in% "United Republic of Tanzania",
                           "Tanzania", food_bal$Country)
food_bal <- select(food_bal, country=Country, variable = Element,
                   year=Year, value = Value) %>%
            spread(variable, value)
names(food_bal) <- c("country", "year", "domestic_supply", "exports",
                     "food", "imports", "production")
food_bal$value <- food_bal$production/(food_bal$production + food_bal$imports - food_bal$exports) * 100
food_bal_table <- tablr(food_bal)
food_bal_table[3:4] <- round(food_bal_table[3:4], 2)
food_bal_table

# -------------------------------------
# Calories kcal/capita
# The table at the end shows the
# percentage of calories consumed on
# average per capita which can be attributed
# to maize. But the total calories are
# calculated as being the sum of everything
# else that uis eaten on average and
# recorded by the fao. It may not include
# certain food groups, for example snacks
# -------------------------------------

cal <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_calories_per_capita.csv"))
cal$Country <- as.character(cal$Country)
cal$Country <- ifelse(cal$Country %in% "United Republic of Tanzania",
                      "Tanzania", cal$Country)
cal <- select(cal, country=Country, item=Item,
              year=Year, value = Value) %>%
      spread(item, value)
cal <- transmute(cal, country, year,
                 maize=`Maize and products`,
                 total=rowSums(cal[-c(1,2)], na.rm=TRUE))
cal$value <- cal$maize/cal$total*100
cal_table <- tablr(cal)
cal_table[3:4] <- round(cal_table[3:4], 2)
cal_table

# -------------------------------------
# Table for just Ethiopia and Ghana
# and all statistics
# -------------------------------------

# Ghana table
gha_table <- rbind(maize_yield_table,
                     maize_area_share_table,
                     fert_table,
                     food_bal_table,
                     cal_table) %>%
  filter(country=="Ghana")
gha_table$country <- NULL
gha_table$Variable <- c("maize Yield", "Maize Area Share",
                          "Fertilizer Use per 1000 ha",
                          "SSR", "Maize Calories")
gha_table <- gha_table[c(4, 1:3)]

