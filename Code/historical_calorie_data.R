#'==============================================================================
#' Project:  Food Futures
#' Subject:  Historical calorie availability data
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   File for retrieving historical calorie availability data
#'==============================================================================

# ------------------------------------------------------------------------------
#' Notes: 
#'       Calorie availability was downloaded from faostat food balance sheets.
#'       This file was very large and took a long time/a lot of memory so
#'       selection was made once and then saved as a new file. Commented
#'       code below shows the exact selection made.
# ------------------------------------------------------------------------------

library(pacman)
p_load(char=c("rprojroot", "dplyr", "readxl", "tidyr", "WDI"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)

# get datapath
dataPath <- "C:/Users/morle001/WEcR/WageningenUR/Dijk, Michiel van - 2281500228 FoodFutures"

# ------------------------------------------------------------------------------
# Calorie information is gathered using bulk download data from
# FAOSTAT and then filtered on world, grand total and kcal/cap/day
# as below
# 
# setwd("C:/Users/morle001/FoodBalanceSheets_E_All_Data_(Normalized)/")
# tmp <- read.csv("FoodBalanceSheets_E_All_Data_(Normalized).csv")
# tmp <- filter(tmp,
#                Item %in% "Maize and products",
#                Unit %in% "kcal/capita/day")
# 
# # for some reason this file has duplicate rows
# tmp <- unique(tmp)
# 
# # write to csv file
# write.csv(tmp, file.path(dataPath, "/Data/Historical/maize_kcal_13042018.csv"),
#           row.names=FALSE)

# read in prepared file
hist_maize <- read.csv(file.path(dataPath, "/Data/Historical/maize_kcal_13042018.csv"))

# select important variables
hist_cal <- select(hist_cal, Item, Element, Year, Value)

# change names of variables to match with studies
hist_cal <- select(hist_cal, variable = Element, year = Year,
                   hist_value = Value)
hist_cal$variable <- "CALO"
hist_cal$sector <- "TOT"
hist_cal$region <- "World"

# take out trash
rm(dataPath)
