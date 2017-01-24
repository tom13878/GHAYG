# -------------------------------------
# Supporting Evidence from faostat
# on maize use in Ghana and Ethiopia
# with four other countries for
# comparison
# -------------------------------------

library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)

# table function
tablr <- function(df, value=value){
  group_by(df, country) %>%
    summarise(`Last Recorded Year`=max(year),
              `Value in Last Year`=value[year==max(year)],
              `5 Year Average`=mean(value[order(year, decreasing=TRUE)][1:5]))
}

# -------------------------------------
# maize variables
# Yield: hg (hectogram)/ha
# area harvested: ha
# land variables are in units of 1000 ha
# -------------------------------------

# read in the maize and total land
# production data as separate files
maize <- read.csv(file.path(root, "Data/FAOSTAT_data_1-23-2017_maize.csv"))
land <- read.csv(file.path(root, "Data/FAOSTAT_data_1-23-2017_land.csv"))
maize$Area <- as.character(maize$Area)
land$Area <- as.character(land$Area)
maize$Area <- ifelse(maize$Area %in% "United Republic of Tanzania", "Tanzania", maize$Area)
land$Area <- ifelse(land$Area %in% "United Republic of Tanzania", "Tanzania", land$Area)

# select only maize yield and create table
# hectograms are 0.1 kilograms -> convert
maize_yield <- dplyr::select(maize, country=Area, variable = Element,
                year=Year, value = Value) %>%
  dplyr::filter(variable=="Yield")
maize_yield$value <- maize_yield$value * 0.1

# table of maize yield in kg/ha averaged
# across each country
maize_yield_table <- tablr(maize_yield)
maize_yield_table

# Make a table of the percentage of arable
# land used for maize production per country
# land used for maize is recorded in ha
# whereas total arable land is recorded in
# 1000 ha -> convert maize are
maize_area_harv <- dplyr::select(maize, country=Area, variable = Element,
                      year=Year, value = Value) %>%
  dplyr::filter(variable!="Yield")
maize_area_harv$value <- maize_area_harv$value/1000

# select land variables and join with
# maize variables to get a measure for
# maize share of total crop area
# note unit conversions
land <- dplyr::select(land, country=Area, year=Year,
              arable = Value)
maize_area_harv <- left_join(maize_area_harv, land)
maize_area_harv <- transmute(maize_area_harv,
                             country, year,
                             value = value/arable * 100)
maize_area_share_table <- tablr(maize_area_harv)
maize_area_share_table[3:4] <- round(maize_area_share_table[3:4], 1)
maize_area_share_table

# make plots of the evolution of
# maize over time, separately for
# each country and then on one graph
# divide all values by 1000
# for interpretation
maize_yield$value <- maize_yield$value

# make a plotting function for each variable/country
plotpattern <- function(df){
  country=unique(df$country)
  df$year <- as.character(df$year)
  df$year <- as.Date(df$year, "%Y", origin=1961)
  ggplot(df, aes(x=year, y = value)) +
    geom_line() +
    scale_colour_identity() +
    scale_x_date(breaks=date_breaks("2 year"), labels=date_format("%Y")) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    ylab("Maize Yield (kg/ha)") +
    theme(
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_line( size=.1, color="grey" ), 
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")) +
    ggtitle(country)
}

# plots for yield area harvested etc
# for each country separately
maize_list <- split(maize_yield, list(maize_yield$country))
lapply(maize_list, plotpattern)

# all countries on one plot
plotpattern2 <- function(df){
  df$year <- as.character(df$year)
  df$year <- as.Date(df$year, "%Y", origin=1961)
  ggplot(df, aes(x=year, y = value, linetype=country)) +
    geom_line() +
    scale_x_date(breaks=date_breaks("2 year"), labels=date_format("%Y")) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    ylab("Maize Yield (kg/ha)") +
    theme(
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_line( size=.1, color="grey" ), 
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"))
    
}
plotpattern2(maize_yield)


# -------------------------------------
# fertilizer table of nitrogen
# use tonnes per 1000 ha per country and year
# -------------------------------------

fertilizer <- read.csv(file.path(root, "Data/FAOSTAT_data_1-23-2017_fertilizer.csv"))
fertilizer$Country <- as.character(fertilizer$Country)
fertilizer$Country <- ifelse(fertilizer$Country %in% "United Republic of Tanzania",
                          "Tanzania", fertilizer$Country)

fertilizer <- dplyr::select(fertilizer, country=Country, item=Item,
                year=Year, value = Value)
fertilizer <- dcast(fertilizer, country + year ~ item)
names(fertilizer) <- c("country", "year", "total", "value", "phosphate")
fert_table <- tablr(fertilizer)
fert_table[3:4] <- round(fert_table[3:4], 2)
fert_table

# -------------------------------------
# food balance table of self sufficiency
# ratios *100. Need to check whether
# total production is the correct variable 
# to use here.
# -------------------------------------

food_bal <- read.csv(file.path(root, "Data/FAOSTAT_data_1-23-2017_food_balance.csv"))
food_bal$Country <- as.character(food_bal$Country)
food_bal$Country <- ifelse(food_bal$Country %in% "United Republic of Tanzania",
                             "Tanzania", food_bal$Country)
food_bal <- dplyr::select(food_bal, country=Country, variable = Element,
                 year=Year, unit=Unit, value = Value)
food_bal <- dcast(food_bal, country + year ~ variable)
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
# to maize.
# -------------------------------------

cal <- read.csv(file.path(root, "Data/FAOSTAT_data_1-23-2017_calories_per_capita.csv"))
cal$Country <- as.character(cal$Country)
cal$Country <- ifelse(cal$Country %in% "United Republic of Tanzania",
                      "Tanzania", cal$Country)
cal <- dplyr::select(cal, country=Country, item=Item,
                   year=Year, unit=Unit, value = Value)
cal <- dcast(cal, country + year ~ item)
cal <- transmute(cal, country, year,
                 maize=`Maize and products`,
                 total=rowSums(cal[-c(1,2)], na.rm=TRUE))
cal$value <- cal$maize/cal$total*100
cal_table <- tablr(cal)
cal_table[3:4] <- round(cal_table[3:4], 2)
cal_table

