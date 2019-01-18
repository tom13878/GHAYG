#'==============================================================================
#' Project:  DFID Yield Gap 
#' Subject:  data preparation file
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   plot level chemical variables wave 1 (2009-2010)
#'==============================================================================

# ------------------------------------------------------------------------------
# Notes:
#      From Africa fertilizer http://africafertilizer.org/national we can search
#      for national prices of urea in 2009/10 - these were around 350 dollars
#      per ton. In 2009 a dollar was worth 1.5 cidis
#      http://xe.com/currencycharts/?from=USD&to=GHS&view=10Y
#      Hence the price should be about 0.525 GHS/kg
#
#      We cannot distinguish between urea and npk here, even though
#      these are the two most used fertilizers
#
#      In 2012, from IFPRI, we know that the price of nitrogen was
#      1.46GHS/kg unsubsidized and 0.74GHS/kg subsidized
# 
#      On africa fertilizer we can also get the prices. These prices
#      seem to have dropped dramatically in 2009-10 period
#
#      in a World bank report we have a 50kg bag of urea or npk-10 
#      costing 50GHS in 2009, before dropping to half that amount
#      in 2010 So we might see prices from between 0.5 and 1
#
#      Need to know the data at which survey was undertaken in order
#      to know which prices to use, the big one or small one.
#
#      Africa Fertilizer also tells us when fertilizer peak demand is
#      for each crop under the Ghana page
#
#      Need to have a fix on when the major season in Ghana is.
# ------------------------------------------------------------------------------

# get packages
library(pacman)
p_load(char=c("dplyr", "haven", "tidyr", "rprojroot", "sjlabelled"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)
source("Code/get_dataPath.r")

# ------------------------------------------------------------------------------
# read in data on chemical use at the crop level for the major season
chem_maj <- read_dta(file.path(dataPath, "Data/S4AVI1.dta"))

# ------------------------------------------------------------------------------
# Data is not recorded at the crop level but at the field level
# therefore it is necessary to split the data into crops and
# then recombine
chem_maj_1 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a162:s4avi_a169iv)
chem_maj_2 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a170:s4avi_a177iv)
chem_maj_3 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a178:s4avi_a185iv)
chem_maj_4 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a186:s4avi_a193iv)
chem_maj_5 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a194:s4avi_a201iv)

# there is the option to record chemical use on five crops per plot
# (in order of importance 1 - 5) but often fewer than five crops are recorded,
# we remove the NA crop values
chem_maj_1 <- chem_maj_1[!is.na(chem_maj_1$s4avi_a162),]
chem_maj_2 <- chem_maj_2[!is.na(chem_maj_2$s4avi_a170),]
chem_maj_3 <- chem_maj_3[!is.na(chem_maj_3$s4avi_a178),]
chem_maj_4 <- chem_maj_4[!is.na(chem_maj_4$s4avi_a186),]
chem_maj_5 <- chem_maj_5[!is.na(chem_maj_5$s4avi_a194),]

# rename variables in each dataframe
names(chem_maj_1) <-
  names(chem_maj_2) <-
  names(chem_maj_3) <-
  names(chem_maj_4) <-
  names(chem_maj_5) <- c("hhno", "plotno", "chem_use", "type",
                         "chem_qty_tot", "unit", "cost_c", "cost_p_orig",
                         "sub", "chem_qty_sub", "unit_sub", "cost_sub_c",
                         "cost_sub_p_orig", "crop1", "crop2", "crop3", "crop4")

# bind all chemicals together and make factors from labelled vectors
chem_maj_tot <- rbind(chem_maj_1, chem_maj_2, chem_maj_3, chem_maj_4, chem_maj_5)
rm(list=c("chem_maj_1", "chem_maj_2", "chem_maj_3", "chem_maj_4", "chem_maj_5"))

# Convert pessawas to cedis, add and remove redundant variables
chem_maj_tot <- chem_maj_tot %>% 
  mutate(cost_p = cost_p_orig/100,
         cost_tot = rowSums(cbind(cost_c, cost_p), na.rm=TRUE),
         cost_tot = replace(cost_tot, cost_tot == 0, NA),
         cost_sub_p = cost_sub_p_orig/100,
         cost_sub = rowSums(cbind(cost_sub_c, cost_sub_p), na.rm=TRUE),
         cost_sub = replace(cost_sub, cost_sub == 0, NA)) 

# make factors of important variables
chem_maj_tot <- chem_maj_tot[!is.na(chem_maj_tot$type), ]
chem_maj_tot$type <- factor(haven::as_factor(chem_maj_tot$type))
newnames <- c("manure", "inorg", "herbicide", "insecticide", "fungicide", NA)
# CHECK there is level '6' so NA added.
levels(chem_maj_tot$type) <- newnames
chem_maj_tot$unit <- as.integer(chem_maj_tot$unit)
chem_maj_tot$unit_sub <- as.integer(chem_maj_tot$unit_sub)
chem_maj_tot$crop1 <- haven::as_factor(chem_maj_tot$crop1)
chem_maj_tot$crop2 <- haven::as_factor(chem_maj_tot$crop2)
chem_maj_tot$crop3 <- haven::as_factor(chem_maj_tot$crop3)
chem_maj_tot$crop4 <- haven::as_factor(chem_maj_tot$crop4)

# Reshape data so that the crop level is 
# the unit of observation
chem_maj_tot <- gather(chem_maj_tot, variable, crop, crop1: crop4) %>% 
  select(-variable) %>%
  mutate(hhno = as.character(hhno))

# plots where either the plotno, crop or type
# of fertilizer are NA can be removed as these
# cannot be linked for the analysis.
chem_maj_tot <- filter(chem_maj_tot, !is.na(plotno),
                       !is.na(crop), !is.na(type))

# ------------------------------------------------------------------------------
# convert local units into standard units
# read in external file with the correct units
# corresponding to each unit code. 
contain_units <- read.csv(file.path(dataPath, "../../Other/Conversion/GHA/container_units_GHA.csv"))

# convert fertilizer to kilograms using conversions from extension officers
conv_fertunit <- read.csv(file.path(dataPath, "../../Other/Conversion/GHA/Fert_GHA.csv")) %>%
  select(-note)

# fertilizer
# Compute subsidised and market prices for inputs averaged over hhno, plotno, crop and chem
# Convert quantities in correct units, select maize crop
fert <- filter(chem_maj_tot, type == "inorg") %>%
  left_join(., contain_units) %>%
  left_join(., conv_fertunit) %>%
  mutate(fert_qty_kg = chem_qty_tot*fert_conv)

# rename conversions to join with the subsidized fertilizer unit
contain_units <- rename(contain_units,
                        unit_sub = unit,
                        unit_sub_name = unit_name)
conv_fertunit <- rename(conv_fertunit,
                        unit_sub_name = unit_name)

# combine fertilizer variables with conversions and convert
fert <- left_join(fert, contain_units) %>%
  left_join(., conv_fertunit) %>%
  mutate(fert_qty_sub_kg = chem_qty_sub * fert_conv)

# calcualte total cost, sub cost, 
fert <- mutate(fert,
               fert_cost_mar = cost_tot - cost_sub,
               fert_qty_mar_kg = fert_qty_kg - fert_qty_sub_kg)

# make sub a factor
fert$sub <- haven::as_factor(fert$sub)

# ==============================================================================
# Because we don't know what fertilizer was used, we are forced to assume
# that it was NPK 15:15:15, the most common fertilizer used in Ghana (Banful 2009)
fert <- fert %>%
  mutate(N = fert_qty_kg * 0.15, 
         N_sub = fert_qty_sub_kg * 0.15,
         N_mar = fert_qty_mar_kg * 0.15)

# take out trash
rm(chem_maj, chem_maj_tot, contain_units, conv_fertunit, dataPath, newnames)
