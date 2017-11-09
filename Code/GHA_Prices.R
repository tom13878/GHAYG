#######################################
###### ANALYSIS of GHA price data #####
#######################################

# CHECK Compare prices with other prices and check if they are realistic!


### PACKAGES
library(pacman)
p_load(char=c("plyr", "dplyr", "haven", "rprojroot", "sjmisc", "tidyr"), install=TRUE)


#######################################
############## READ DATA ##############
#######################################

source("Code/GHA_2010.r")


### SETWD
root <- find_root(is_rstudio_project)
setwd(root)

### SOURCE
source(file.path(root, "Code/get_dataPath.r"))
source(file.path(root, "Code/winsor.R"))


#######################################
############## PACKAGES ETC ###########
#######################################

library(dplyr)
library(ggplot2)
library(stargazer)
library(haven)
library(tidyr)
library(xtable)
library(DescTools)
library(sandwich)
library(lmtest)
library(assertive)

options(scipen=999)



#######################################
############ PROCESSING ###############
#######################################


# -------------------------------------
# Similar to output chemical inputs have
# been recorded in a bizarre way and need
# to be rearranged for both seasons
# -------------------------------------

# -------------------------------------
# Major season
# -------------------------------------

chem_maj <- read_dta(file.path(dataPath, "Data/S4AVI1.dta"))

chem_maj_1 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a162:s4avi_a169iv)
chem_maj_2 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a170:s4avi_a177iv)
chem_maj_3 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a178:s4avi_a185iv)
chem_maj_4 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a186:s4avi_a193iv)
chem_maj_5 <- select(chem_maj, hhno, plotno=s4avi1_plotno, s4avi_a194:s4avi_a201iv)

chem_maj_1 <- chem_maj_1[!is.na(chem_maj_1$s4avi_a162),]
chem_maj_2 <- chem_maj_2[!is.na(chem_maj_2$s4avi_a170),]
chem_maj_3 <- chem_maj_3[!is.na(chem_maj_3$s4avi_a178),]
chem_maj_4 <- chem_maj_4[!is.na(chem_maj_4$s4avi_a186),]
chem_maj_5 <- chem_maj_5[!is.na(chem_maj_5$s4avi_a194),]

names(chem_maj_1) <-
  names(chem_maj_2) <-
  names(chem_maj_3) <-
  names(chem_maj_4) <-
  names(chem_maj_5) <- c("hhno", "plotno", "chem_use", "type",
                         "qty_tot", "unit", "value_c", "value_p", "sub", "qty_sub",
                         "unit_sub", "value_sub_c", "value_sub_p", "crop1",
                         "crop2", "crop3", "crop4")


# bind all chemicals together and make factors from labelled vectors
chem_maj_tot <- rbind(chem_maj_1, chem_maj_2, chem_maj_3, chem_maj_4, chem_maj_5)
rm(list=c("chem_maj_1", "chem_maj_2", "chem_maj_3", "chem_maj_4", "chem_maj_5"))

# Convert pessawas to cedis, add and remove redundant variables
chem_maj_tot <- chem_maj_tot %>% 
  mutate(value_p = value_p/100,
         value_tot = rowSums(cbind(value_c, value_p), na.rm=TRUE),
         value_tot = replace(value_tot, value_tot == 0, NA),
         value_sub_p = value_sub_p/100,
         value_sub = rowSums(cbind(value_sub_c, value_sub_p), na.rm=TRUE),
         value_sub = replace(value_sub, value_sub == 0, NA)) %>%
  select(-value_p, -value_c, -value_sub_p, -value_sub_c) 

# make factors of important variables
chem_maj_tot <- chem_maj_tot[!is.na(chem_maj_tot$type), ]
chem_maj_tot$type <- factor(as_factor(chem_maj_tot$type))
newnames <- c("manure", "inorg", "herbicide", "insecticide", "fungicide", NA)
# CHECK there is level '6' so NA added.
levels(chem_maj_tot$type) <- newnames
chem_maj_tot$unit <- as.integer(chem_maj_tot$unit)
chem_maj_tot$unit_sub <- as.integer(chem_maj_tot$unit_sub)
chem_maj_tot$crop1 <- as_factor(chem_maj_tot$crop1)
chem_maj_tot$crop2 <- as_factor(chem_maj_tot$crop2)
chem_maj_tot$crop3 <- as_factor(chem_maj_tot$crop3)
chem_maj_tot$crop4 <- as_factor(chem_maj_tot$crop4)

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

# read in external file with the correct units
# corresponding to each unit code. 
contain_units <- read.csv(file.path(dataPath, "../Other/container_unitsGHA.csv"))

# convert fertilizer to kilograms using conversions from extension officers
conv_fertunit <- read.csv(file.path(dataPath, "../../Other/Conversion/GHA/Fert_GHA.csv")) %>%
  select(-note)


# fertilizer
# Compute subsidised and market prices for inputs averaged over hhno, plotno, crop and chem
# Convert quantities in correct units, select maize crop
fert <- filter(chem_maj_tot, type == "inorg") %>%
  filter(crop == "Maize") %>%
  left_join(., contain_units) %>%
  left_join(., conv_fertunit) %>%
  mutate(qty_tot = qty_tot*fert_conv) %>%
  select(-fert_conv, -unit_name, -unit) %>%
  dplyr::rename(unit = unit_sub) %>%
  left_join(., contain_units) %>%
  left_join(., conv_fertunit) %>%
  mutate(qty_sub = qty_sub*fert_conv) %>%
  mutate(value_tot = replace(value_tot, value_tot<0, 0),
         value_sub = replace(value_sub, is.na(value_sub) | value_sub<0, 0),
         qty_sub = replace(qty_sub, is.na(qty_sub), 0),
         value_mar = value_tot-value_sub,
         qty_mar = qty_tot-qty_sub,
         value_mar = replace(value_mar, value_mar<0, 0),
         qty_mar = replace(qty_mar, qty_mar<0, 0)) %>%
  select(hhno, plotno, value_tot, qty_tot, value_mar, qty_mar, value_sub, qty_sub)

#rm(chem_maj, chem_maj_tot)

# To calculate prices it is important that data is available for both value and qty or both are zero
# We assume that handful of NA values are zero and ensure that value and qty are pairwise 0
fert[is.na(fert)] <- 0

fert <- fert %>%
  mutate(value_tot =ifelse(qty_tot==0, 0, value_tot),
         value_mar =ifelse(qty_mar==0, 0, value_mar),
         value_sub =ifelse(qty_sub==0, 0, value_sub),
         qty_tot =ifelse(value_tot==0, 0, qty_tot),
         qty_mar =ifelse(value_mar==0, 0, qty_mar),
         qty_sub =ifelse(value_sub==0, 0, qty_sub))

# Convert quantity to N.
# virtually all fertilizer in Ghana is NPK 15:15:15
# making it very easy to work out the nitrogen. 
# phosphorous and potassium contents are the same but not added
fert <- fert %>%
  mutate(N = qty_tot*0.15, 
         N_sub = qty_sub*0.15,
         N_mar = qty_mar*0.15) %>%
  select(-qty_tot, qty_sub, qty_mar)

# Fertilizer prices 
# Note that for some plots there are duplicated entries (i.e. two types of fertilizer)
# We use all information to calculate regional price averages

key <- select(GHA2010, hhid, plotno, ZONE, REGNAME, DISCODE)

fert <- fert %>%
  rename(hhid = hhno) %>%
  mutate(Pn = value_tot/N,
            Pnm = value_mar/N_mar,
            Pns = value_sub/N_sub) %>%
  select(hhid, plotno, Pn, Pnm, Pns) 


# Remove all inf, nan
inf.nan.na.clean.f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  return(x)
}

fert <- inf.nan.na.clean.f(fert) %>%
    left_join(key, .) %>%
  dplyr::select(-hhid, -plotno)

# Construct price data.frame
# construct base dataframe with all zones, regions and districts
base <- GHA2010 %>% 
  dplyr::select(ZONE, REGNAME, DISCODE) %>%
  unique() %>%
  na.omit

# Values are winsored by surveyyear, aggregates are presented for at least 5 values
medianPrice_f <- function(df, level, group, type){
  prices <- df %>% 
    group_by_(.dots = c(group)) %>%
    dplyr::summarize(
      number = sum(!is.na(price)),
      price = median(price, na.rm=T)) %>%
    filter(number>=5) %>%
    mutate(level = level) %>%
    select(-number) 
  #prices <- setNames(prices, c(group, "price", "level")) 
  out <- left_join(base, prices) %>%
    mutate(type = type)
  return(out)
}


# market  prices
fertmar <- fert %>%
  select(-Pn, -Pns) %>%
  mutate(price = winsor2(Pnm))

fpCountry <- fertmar %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pnm")

fpZone <- medianPrice_f(fertmar, "zone", c("ZONE"), "Pnm")
fpRegion <- medianPrice_f(fertmar, "region", c("ZONE", "REGNAME"), "Pnm")
fpDistrict <- medianPrice_f(fertmar, "district", c("ZONE", "REGNAME", "DISCODE"), "Pnm")

fertMarPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertmar)

# Market and subsidised prices
fertmix <- fert %>%
  select(-Pnm, -Pns) %>%
  mutate(price = winsor2(Pn))

fpCountry <- fertmix %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pn")

fpZone <- medianPrice_f(fertmix, "zone", c("ZONE"), "Pn")
fpRegion <- medianPrice_f(fertmix, "region", c("ZONE", "REGNAME"), "Pn")
fpDistrict <- medianPrice_f(fertmix, "district", c("ZONE", "REGNAME", "DISCODE"), "Pn")

fertMixPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertmix)

# Subsidised prices
fertsub <- fert %>%
  select(-Pnm, -Pn) %>%
  mutate(price = winsor2(Pns))

fpCountry <- fertsub %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country") 
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pns")

fpZone <- medianPrice_f(fertsub, "zone", c("ZONE"), "Pns")
fpRegion <- medianPrice_f(fertsub, "region", c("ZONE", "REGNAME"), "Pns")
fpDistrict <- medianPrice_f(fertsub, "district", c("ZONE", "REGNAME", "DISCODE"), "Pns")

fertSubPrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "fertilizer") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, fertsub)

# Maize prices
maize <- GHA2010 %>% 
  dplyr::select(ZONE, REGNAME, DISCODE, price = crop_price) %>%
  mutate(price = winsor2(price))

fpCountry <- maize %>%
  dplyr::summarize(price = median(price, na.rm=T)) %>%
  mutate(level = "country")
fpCountry <- mutate(base, price = fpCountry$price,
                    level = "country",
                    type = "Pc") 

fpZone <- medianPrice_f(maize, "zone", c("ZONE"), "Pc")
fpRegion <- medianPrice_f(maize, "region", c("ZONE", "REGNAME"), "Pc")
fpDistrict <- medianPrice_f(maize, "district", c("ZONE", "REGNAME", "DISCODE"), "Pc")

maizePrice <- bind_rows(fpDistrict, fpRegion, fpZone, fpCountry) %>%
  na.omit %>%
  spread(level, price) %>%
  mutate(regPrice = ifelse(!is.na(district), district, 
                        ifelse(!is.na(region), region,
                               ifelse(!is.na(zone), zone, country))),
         source = ifelse(!is.na(district), "district", 
                         ifelse(!is.na(region), "region",
                                ifelse(!is.na(zone), "zone", "country"))),
         product = "maize") %>%
  select(-country, -zone, -region, -district)

rm(fpCountry, fpZone, fpRegion, fpDistrict, maize)

# Combine fert price data files 
regPrice <- bind_rows(fertMixPrice, fertMarPrice, fertSubPrice, maizePrice) %>% ungroup
  

# Create price file at plot level.
# Again, we winsor the prices for each type of price and per surveyyear
plotPrice <- select(GHA2010, hhid, plotno, ZONE, REGNAME, DISCODE, Pn = WPn, Pns = WPnsub, Pnm = WPnosub, Pc = crop_price) %>%
  gather(type, plotPrice, Pn, Pns, Pnm, Pc) %>%
  group_by(type) %>%
  mutate(plotPrice =winsor2(plotPrice)) %>%
  ungroup() %>% unique


# Substitute regional prices when plot level price is not available
price <- left_join(plotPrice, regPrice) %>%
          mutate(price = ifelse(is.na(plotPrice), regPrice, plotPrice)) %>%
          unique() %>% # should not be necessary but never know
          select(-source, -regPrice, -plotPrice, -product) %>%
          spread(type, price)


# Plot
ggplot(data = as.data.frame(price), aes(Pc)) + geom_density() + facet_wrap(~ZONE)
summary(price)

# save data
saveRDS(price, "Cache/Prices_GHA.rds")


