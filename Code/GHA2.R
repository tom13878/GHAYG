#######################################
####### GHANA data preparation ########
#######################################  

# 17/08/2015

# for munging
library(haven)
library(reshape2)
library(plyr)
library(Hmisc)
detach(package:dplyr)
library(dplyr)

fp1 <- "C:/Users/Tomas/Documents/Work/LEI/"
setwd(fp1)

#######################################
############### output ################
####################################### 

# -------------------------------------
# Minor season
# -------------------------------------

oput_min <- read_dta("S4AV2.dta")

oput_min$id1 <- as.character(as_factor(oput_min$id1))
oput_min$s4v_a120i <- as_factor(oput_min$s4v_a120i)
oput_min$s4v_a120ii <- as_factor(oput_min$s4v_a120ii)
oput_min$s4v_a120iii <- as_factor(oput_min$s4v_a120iii)
oput_min$s4v_a120iv <- as_factor(oput_min$s4v_a120iv)
oput_min$s4v_a120v <- as_factor(oput_min$s4v_a120v)

# need to select out the data seperately and rbind everything together
# first crop
crop_min_1 <- select(oput_min, reg=id1, id3, hhno, plot_no=s4av2_plotno, s4v_a120i, s4v_a121i:s4v_a128 )
crop_min_2 <- select(oput_min, reg=id1, id3, hhno, plot_no=s4av2_plotno, s4v_a120ii, s4v_a129i:s4v_a136)
crop_min_3 <- select(oput_min, reg=id1, id3, hhno, plot_no=s4av2_plotno, s4v_a120iii, s4v_a137i:s4v_a144)
crop_min_4 <- select(oput_min, reg=id1, id3, hhno, plot_no=s4av2_plotno, s4v_a120iv, s4v_a145i:s4v_a152)
crop_min_5 <- select(oput_min, reg=id1, id3, hhno, plot_no=s4av2_plotno, s4v_a120v, s4v_a153i:s4v_a160)

# drop any NA values which are only there because of weird format
crop_min_1 <- crop_min_1[!is.na(crop_min_1$s4v_a120i),]
crop_min_2 <- crop_min_2[!is.na(crop_min_2$s4v_a120ii),]
crop_min_3 <- crop_min_3[!is.na(crop_min_3$s4v_a120iii),]
crop_min_4 <- crop_min_3[!is.na(crop_min_4$s4v_a120iv),]
crop_min_5 <- crop_min_3[!is.na(crop_min_5$s4v_a120v),]


# now need to change the names so they are all the same
names(crop_min_1) <- names(crop_min_2) <- names(crop_min_3) <- names(crop_min_4) <- names(crop_min_5) <-
  c("reg", "id3", "hhno", "plot_no", "crop", "crop_id", "type", "quantity",
    "unit", "value_c", "value_p", "revenue_c", "revenue_p", "left_over",
    "left_over_value_c", "left_over_value_p", "disease", "percent_lost")

# Bind together all of the oput values
oput_min_tot <- rbind(crop_min_1, crop_min_2, crop_min_3, crop_min_4, crop_min_5)
rm(list=c("crop_min_1", "crop_min_2", "crop_min_3", "crop_min_4", "crop_min_5"))

oput_min_tot$crop <- as_factor(oput_min_tot$crop)
oput_min_tot$type <- as_factor(oput_min_tot$type)
oput_min_tot$left_over <- as_factor(oput_min_tot$left_over)
oput_min_tot$disease <- as_factor(oput_min_tot$disease)

# add a variable for the number of crops on a plot
# and whether or not a legume was grown on that plot

oput_min_tot <- ddply(oput_min_tot, .(hhno, plot_no), transform,
                      crop_count=length(crop[!is.na(crop)]),
                      legume=ifelse(any(crop %in% "Beans/Peas"), 1, 0))

oput_min_mze <- filter(oput_min_tot, crop %in% "Maize") %>% 
  select(hhno, plot_no, qty=quantity, unit, value_c, crop_count, legume)

# -------------------------------------
# There are many units of output and 
# it is necessary to make conversions
# unfortunately conversions are not 
# part of the data. Instead, section 5A
# of the community questionnaire deals
# with conversions from local units
# to kilograms at the market level. 
# these are used as the conversions for
# output but note that there is a lot of
# variation in units across communities.
# -------------------------------------

# convert to kilograms using conversions in sec 5A of
# rural community questionnaire

SEC5A <- read_dta("data/GHA/SEC 5A.dta")

# construct an auxillary conversion table
aux <- melt(SEC5A, id.vars=c("reg", "EA_No", "commcode", "s5", "s5a_1"))
aux <- aux[!is.na(aux$value),]
aux <- ddply(aux, .(s5a_1, variable), summarize, kilo_bar = mean(value, na.rm=TRUE))

# Add unit codes by hand from BID
variable <- as.character(unique(aux$variable))[order(as.character(unique(aux$variable)))]
unit <- c(2, 27, 4, 6, 7, 29, 8, 9, 11, 12, 14, 17, 18, 19, 34, 37, 23, 24, 26)
cnvrt <- data.frame(variable, unit)

# join conversion table with unit table
aux <- left_join(aux, cnvrt)
aux_mze <- filter(aux, s5a_1 %in% "MAIZE")

# join unit table with maize output

oput_min_mze <- left_join(oput_min_mze, select(aux_mze, unit, kilo_bar))

# calculate quantity in kilograms

oput_min_mze <- mutate(oput_min_mze, qty = qty *  kilo_bar)

# get rid of the unit variable and NA values for maize quantity

oput_min_mze <- select(oput_min_mze, -unit, -kilo_bar)
oput_min_mze <- oput_min_mze[!is.na(oput_min_mze$qty),]

rm(list=c("aux", "aux_mze", "cnvrt", "SEC5A", "unit", "variable"))

#######################################
################ area #################
#######################################

area <- read_dta("data/GHA/S4AII.dta") 

area <- select(area, reg=id1, id2, hhno, plotno=plot_no,
               size=s4aii_a10, unit=s4aii_a11, test_area=s4aii_a12,
               test_compare=s4aii_a13, test_in=s4aii_a14, in_test=s4aii_a15a,
               area_ha)

# use as_factor to get labels. 
area$reg <- as.character(as_factor(area$reg))
area$unit <- as_factor(area$unit)
area$test_area <- as_factor(area$test_area)
area$test_compare <- as_factor(area$test_compare)

# for now we just need the area in hectacres
area <- select(area, reg, hhno, plotno,  area_ha)


#######################################
############## CHEMICALS ##############
#######################################

# -------------------------------------
# Minor season
# -------------------------------------

chem_min <- read_dta("S4AVI2.dta") 

chem_min_1 <- select(chem_min, hhno, plotno=s4avi2_plotno, s4avi_a203:s4avi_210iv)
chem_min_2 <- select(chem_min, hhno, plotno=s4avi2_plotno, s4avi_211:s4avi_218iv)
chem_min_3 <- select(chem_min, hhno, plotno=s4avi2_plotno, s4avi_219:s4avi_226iv)
chem_min_4 <- select(chem_min, hhno, plotno=s4avi2_plotno, s4avi_227:s4avi_234iv)
chem_min_5 <- select(chem_min, hhno, plotno=s4avi2_plotno, s4avi_235:s4avi_242iv)

chem_min_1 <- chem_min_1[!is.na(chem_min_1$s4avi_a203),]
chem_min_2 <- chem_min_2[!is.na(chem_min_2$s4avi_211),]
chem_min_3 <- chem_min_3[!is.na(chem_min_3$s4avi_219),]
chem_min_4 <- chem_min_4[!is.na(chem_min_4$s4avi_227),]
chem_min_5 <- chem_min_5[!is.na(chem_min_5$s4avi_235),]

names(chem_min_1) <-
  names(chem_min_2) <-
  names(chem_min_3) <-
  names(chem_min_4) <-
  names(chem_min_5) <- c("hhno", "plotno", "chem_use", "type",
                         "qty", "unit", "value_c", "value_p", "gov", "gov_qty",
                         "gov_unit", "gov_value_c", "gov_value_p", "crop1",
                         "crop2", "crop3", "crop4")


# bind all chemicals together and make factors from labelled vectors
chem_min_tot <- rbind(chem_min_1, chem_min_2, chem_min_3, chem_min_4, chem_min_5)
rm(list=c("chem_min_1", "chem_min_2", "chem_min_3", "chem_min_4", "chem_min_5"))

# make factors of important variables
chem_min_tot$chem_use <- as_factor(chem_min_tot$chem_use)
chem_min_tot$type <- as_factor(chem_min_tot$type)
chem_min_tot$unit <- as_factor(chem_min_tot$unit)
chem_min_tot$gov <- as_factor(chem_min_tot$gov)
chem_min_tot$gov_unit <- as_factor(chem_min_tot$gov_unit)
chem_min_tot$crop1 <- as_factor(chem_min_tot$crop1)
chem_min_tot$crop2 <- as_factor(chem_min_tot$crop2)
chem_min_tot$crop3 <- as_factor(chem_min_tot$crop3)
chem_min_tot$crop4 <- as_factor(chem_min_tot$crop4)

# grab only plots where some maize was grown
chem_min_tot <- melt(chem_min_tot, measure.vars=c("crop1", "crop2", "crop3", "crop4")) %>%
  rename(crop=value) %>% select(-variable)


# remove any NA values for chemical type
chem_min_tot <- chem_min_tot[!is.na(chem_min_tot$type),]

# add in dummy variables for whether a
# a particular chemical was used on a plot - try to get to 711
chem_min_tot <- ddply(chem_min_tot, .(hhno, plotno, crop), summarise,
                      pest=ifelse(any(type %in% c("Herbicide", "Insecticide", "Fungicide")), 1, 0),
                      org=ifelse(any(type %in% "Fertilizer (organic)"), 1, 0),
                      inorg=ifelse(any(type %in% "Fertilizer (inorganic)"), 1, 0),
                      qty=ifelse(any(type %in% "Fertilizer (inorganic)"), qty, NA),
                      prc=ifelse(any(type %in% "Fertilizer (inorganic)"), value_c, NA))

# -------------------------------------
# Don't know what kind of fertilizer 
# people use in Ghana. Go ahead and 
# assume n=0.25 and p=010 and k=0
# ------------------------------------


chem_min_tot$n <- 0.25
chem_min_tot$p <- 0.10

chem_min_tot <- mutate(chem_min_tot, N=qty*n, P=qty*p)

# -------------------------------------
# calculate the unit producer price
# paid for the fertilizer
# -------------------------------------

