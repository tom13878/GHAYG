#######################################
########## GHANA 2010-11 ##############
####################################### 

# Tom dataPath
dataPath <- "C:/Users/Tomas/Documents/LEI/data/GHA/Data"

# LEI Path
# dataPath <- "W:/LEI/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/GHA/Data"

library(haven)
library(reshape2)
library(plyr)
library(dplyr)
library(assertive)


options(scipen=999)


#######################################
############## LOCATION ###############
#######################################


#######################################
########### SOCIO/ECONOMIC ############
#######################################


#######################################
############### OUTPUT ################
#######################################

# -------------------------------------
# output section has to be manipulated 
# because it has been recorded in a bad 
# way. Not one observation per row!
# -------------------------------------

oput_maj <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AV1.dta"))
oput_maj$id1 <- as.character(as_factor(oput_maj$id1))
oput_maj$s4v_a78i <- as_factor(oput_maj$s4v_a78i)
oput_maj$s4v_a78ii <- as_factor(oput_maj$s4v_a78ii)
oput_maj$s4v_a78iii <- as_factor(oput_maj$s4v_a78iii)
oput_maj$s4v_a78iv <- as_factor(oput_maj$s4v_a78iv)
oput_maj$s4v_a78v <- as_factor(oput_maj$s4v_a78v)


# need to select out the data seperately and rbind everything together
# first crop
crop_maj_1 <- select(oput_maj, reg=id1, id3, hhno, plotno=s4av1_plotno, s4v_a78i, s4v_a80i:s4v_a87 )
crop_maj_2 <- select(oput_maj, reg=id1, id3, hhno, plotno=s4av1_plotno, s4v_a78ii, s4v_a88i:s4v_a95)
crop_maj_3 <- select(oput_maj, reg=id1, id3, hhno, plotno=s4av1_plotno, s4v_a78iii, s4v_a96i:s4v_a103)
crop_maj_4 <- select(oput_maj, reg=id1, id3, hhno, plotno=s4av1_plotno, s4v_a78iv, s4v_a104i:s4v_a111)
crop_maj_5 <- select(oput_maj, reg=id1, id3, hhno, plotno=s4av1_plotno, s4v_a78v, s4v_a112i:s4v_a119)

# drop any NA values which are only there because of weird format
crop_maj_1 <- crop_maj_1[!is.na(crop_maj_1$s4v_a78i),]
crop_maj_2 <- crop_maj_2[!is.na(crop_maj_2$s4v_a78ii),]
crop_maj_3 <- crop_maj_3[!is.na(crop_maj_3$s4v_a78iii),]

# the 4th and fifth rows have a problem in them because of classes of columns
crop_maj_4$s4v_a111 <- as.numeric(crop_maj_4$s4v_a111)
crop_maj_5$s4v_a119 <- as.numeric(crop_maj_5$s4v_a119)

# then drop NA values
crop_maj_4 <- crop_maj_4[!is.na(crop_maj_4$s4v_a78iv),]
crop_maj_5 <- crop_maj_5[!is.na(crop_maj_5$s4v_a78v),]

# now need to change the names so they are all the same
names(crop_maj_1) <- names(crop_maj_2) <- names(crop_maj_3) <- names(crop_maj_4) <- names(crop_maj_5) <-
        c("reg", "id3", "hhno", "plotno", "crop", "crop_id", "type", "quantity", "unit", "value_c",
          "value_p", "revenue_c", "revenue_p", "left_over", "left_over_value_c",
          "left_over_value_p", "disease", "percent_lost")

# Bind together all of the output values
oput_maj_tot <- rbind(crop_maj_1, crop_maj_2, crop_maj_3, crop_maj_4, crop_maj_5)
rm(list=c("crop_maj_1", "crop_maj_2", "crop_maj_3", "crop_maj_4", "crop_maj_5"))

# make some factor variables
oput_maj_tot$crop <- as_factor(oput_maj_tot$crop)
oput_maj_tot$type <- as_factor(oput_maj_tot$type)
oput_maj_tot$left_over <- as_factor(oput_maj_tot$left_over)
oput_maj_tot$disease <- as_factor(oput_maj_tot$disease)

# add a variable for the number of crops on a plot
# and whether or not a legume was grown on that plot

oput_maj_tot <- ddply(oput_maj_tot, .(hhno, plotno), transform,
                      crop_count=length(crop[!is.na(crop)]),
                      legume=ifelse(any(crop %in% "Beans/Peas"), 1, 0))

# select only maize oput and chosen variables
oput_maj_mze <- filter(oput_maj_tot, crop %in% "Maize") %>%
        select(hhno, plotno, qty=quantity, unit, value_c, crop_count, legume)

# make cropcount into a binary variable
oput_maj_mze$crop2 <- ifelse(oput_maj_mze$crop_count %in% 2, 1, 0)
oput_maj_mze$crop3 <- ifelse(oput_maj_mze$crop_count %in% 3, 1, 0)
oput_maj_mze$crop4 <- ifelse(oput_maj_mze$crop_count %in% 4, 1, 0)
oput_maj_mze$crop5 <- ifelse(oput_maj_mze$crop_count %in% 5, 1, 0)

rm(list=c("oput_maj_tot", "oput_maj"))


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

SEC5A <- read_dta(file.path(dataPath, "RURAL/SEC 5A.dta"))

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

# join unit table with maize output. 
# For goods other than maize this
# conversions are difficult because of spelling
# mistakes

oput_maj_mze <- left_join(oput_maj_mze, select(aux_mze, unit, kilo_bar))

# calculate quantity in kilograms

oput_maj_mze <- mutate(oput_maj_mze, qty = qty *  kilo_bar)

# get rid of the unit variable and NA values for maize quantity
oput_maj_mze <- select(oput_maj_mze, -unit, -kilo_bar)
oput_maj_mze <- oput_maj_mze[!is.na(oput_maj_mze$qty) & !oput_maj_mze$qty %in% 0,]
oput_maj_mze$maze_prc <- oput_maj_mze$qty/oput_maj_mze$value_c
oput_maj_mze <- select(oput_maj_mze, -value_c)

oput_maj_mze$hhno <- as.character(oput_maj_mze$hhno)

rm(list=c("aux", "aux_mze", "cnvrt", "SEC5A", "unit", "variable"))

#######################################
############## CHEMICALS ##############
#######################################

# -------------------------------------
# Similar to output chemical inputs have
# been recorded in a bizarre way and need
# to be rearranged for both seasons
# -------------------------------------

# -------------------------------------
# Major season
# -------------------------------------

chem_maj <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AVI1.dta"))
chem_maj <- unattribute(chem_maj)

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
                         "qty", "unit", "value_c", "value_p", "gov", "gov_qty",
                         "gov_unit", "gov_value_c", "gov_value_p", "crop1",
                         "crop2", "crop3", "crop4")


# bind all chemicals together and make factors from labelled vectors
chem_maj_tot <- rbind(chem_maj_1, chem_maj_2, chem_maj_3, chem_maj_4, chem_maj_5)
rm(list=c("chem_maj_1", "chem_maj_2", "chem_maj_3", "chem_maj_4", "chem_maj_5"))

# for now only select variables you need for analysis
chem_maj_tot <- select(chem_maj_tot, -chem_use, -value_p, -gov, - gov_qty, -gov_unit, -gov_value_c, -gov_value_p)

# make factors of important variables
chem_maj_tot <- chem_maj_tot[!is.na(chem_maj_tot$type), ]
chem_maj_tot$type <- factor(as_factor(chem_maj_tot$type))
newnames <- c("manure", "inorg", "herbicide", "insecticide", "fungicide")
levels(chem_maj_tot$type) <- newnames
chem_maj_tot$unit <- as.integer(chem_maj_tot$unit)
chem_maj_tot$crop1 <- as_factor(chem_maj_tot$crop1)
chem_maj_tot$crop2 <- as_factor(chem_maj_tot$crop2)
chem_maj_tot$crop3 <- as_factor(chem_maj_tot$crop3)
chem_maj_tot$crop4 <- as_factor(chem_maj_tot$crop4)

# Reshape data so that the crop level is 
# the unit of observation
chem_maj_tot <- melt(chem_maj_tot, measure.vars=c("crop1", "crop2", "crop3", "crop4")) %>%
  rename(crop=value) %>% select(-variable)

# plots where either the plotno, crop or type
# of fertilizer are NA can be removed as these
# cannot be linked for the analysis.

chem_maj_tot <- filter(chem_maj_tot, !is.na(plotno),
                       !is.na(crop), !is.na(type))

# read in external file with the correct units
# corresponding to each unit code. Not correct
# when using as_factor function

contain_units <- read.csv(file.path(dataPath, "../Other/container_unitsGHA.csv"))
chem_maj_tot <- left_join(chem_maj_tot, contain_units)

# convert fertilizer to kilograms using conversions from extension officers
conv_fertunit <- read.csv(file.path(paste0(dataPath,"/../../"), "Other/Fertilizer/Fert_GHA.csv"))

# join and multiply qty by fert_conv to give
# quantity in kilograms.
chem_maj_tot <- left_join(chem_maj_tot, conv_fertunit)
chem_maj_tot$qty <- chem_maj_tot$qty*chem_maj_tot$fert_conv
chem_maj_tot$fert_conv <- chem_maj_tot$unit_name <- chem_maj_tot$unit <- chem_maj_tot$value_c <- NULL
chem_maj_tot <- select(chem_maj_tot, hhno, plotno, crop, everything())

# change shape of data
chem_maj_tot <- chem_maj_tot %>%
  group_by(hhno, plotno, crop, type) %>%
  summarise(qty=mean(qty)) %>% dcast(hhno + plotno + crop ~ type, value.var="qty")
chem_maj_tot[, newnames][is.na(chem_maj_tot[, newnames])] <- 0

# virtually all fertilizer in Ghana is NPK 15:15:15
# making it very easy to work out the nitrogen,
# phosphorous and potassium contents
chem_maj_tot$N <- chem_maj_tot$inorg*0.15 
chem_maj_tot$P <- chem_maj_tot$inorg*0.15
chem_maj_tot$K <- chem_maj_tot$inorg*0.15

# -------------------------------------
# create binary variables for whether or
# not there is certain chemicals used
# -------------------------------------

# maize level - so pesticide or organic fertilizer used on maize
chem_maj_tot <- mutate(chem_maj_tot,
                       herb = ifelse(herbicide > 0, 1, 0),
                       fung = ifelse(fungicide > 0, 1, 0),
                       pest = ifelse(insecticide > 0, 1, 0))

chem_maj_maize <- filter(chem_maj_tot, crop=="Maize")
chem_maj_maize$hhno <- as.character(chem_maj_maize$hhno)

rm(list=c("chem_maj", "chem_maj_tot", "contain_units", "conv_fertunit"))

#######################################
############### LABOUR ################
#######################################

lab_val1 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX1.dta")) %>%
  select(-id1, -id2, -hhno, s4aix1_plotno)
lab_val1[is.na(lab_val1)] <- 0
lab_val1 <- rowSums(lab_val1)
lab1 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX1.dta")) %>%
  select(hhno, plotno=s4aix1_plotno) %>% cbind(lab_val1)

lab_val2 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX2.dta")) %>%
  select(-id2, -id2, -hhno, s4aix2_plotno)
lab_val2[is.na(lab_val2)] <- 0
lab_val2 <- rowSums(lab_val2)
lab2 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX2.dta")) %>%
  select(hhno, plotno=s4aix2_plotno) %>% cbind(lab_val2)

lab_val3 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX3.dta")) %>%
  select(-id3, -id2, -hhno, s4aix3_plotno)
lab_val3[is.na(lab_val3)] <- 0
lab_val3 <- rowSums(lab_val3)
lab3 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX3.dta")) %>%
  select(hhno, plotno=s4aix3_plotno) %>% cbind(lab_val3)

lab_val4 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX4.dta")) %>%
  select(-id4, -id2, -hhno, s4aix4_plotno)
lab_val4[is.na(lab_val4)] <- 0
lab_val4 <- rowSums(lab_val4)
lab4 <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIX4.dta")) %>%
  select(hhno, plotno=s4aix4_plotno) %>% cbind(lab_val4)

rm(list=c("lab_val1", "lab_val2", "lab_val3", "lab_val4"))

lab1$hhno <- as.character(lab1$hhno)
lab2$hhno <- as.character(lab2$hhno)
lab3$hhno <- as.character(lab3$hhno)
lab4$hhno <- as.character(lab4$hhno)

#######################################
############### GEO ###################
#######################################

# # add Michiel's geo files
# geo10 <- readRDS(file.path(dataPath, "../../../Other/Spatial/TZA/TZA_geo_2010.rds")) %>% 
#   select(y2_hhid, lon, lat, plotnum, SPEI, RootDepth, region_name=NAME_1, district_name=NAME_2,
#          AEZ=land03, ph=ph_sd1_sd3, ph2=ph_sd1_sd5,
#          SOC=SOC_sd1_sd3, SOC2=SOC_sd1_sd5, rain_CRU=gsRainfall,
#          dist_hh=dist01,  dist_road=dist02, dist_popcenter=dist03,
#          dist_market=dist04, dist_borderpost=dist05, dist_regcap=dist06,
#          hh_elevation=soil01, hh_slope=soil02, hh_twi=soil03,
#          rain_year=crops07, rain_wq=crops08,
#          YA, YW, YP) %>%
#   unique()
# 
# geo10 <- read_dta(file.path(dataPath, "TZNPS2GEODTA/HH.Geovariables_Y2.dta")) %>%
#   select(y2_hhid, lon=lon_modified, lat=lat_modified, dist2town=dist02,
#          dist2market=dist03, dist2HQ=dist05, avgTemp=clim01, avgpPrecip=clim03)

#######################################
################ AREAS ################
#######################################

area <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AII.dta"))

area <- select(area, reg=id1, id2, hhno, plotno=plot_no,
                    size=s4aii_a10, unit=s4aii_a11, test_area=s4aii_a12,
                    test_compare=s4aii_a13, test_in=s4aii_a14, in_test=s4aii_a15a,
                    area=area_ha)

# use as_factor to get labels. 
area$reg <- as.character(as_factor(area$reg))
area$unit <- as_factor(area$unit)
area$test_area <- as_factor(area$test_area)
area$test_compare <- as_factor(area$test_compare)

# for now we just need the area in hectacres
area <- select(area, hhno, plotno,  area)
area <- ddply(area, .(hhno), transform,
              area_tot=sum(area, na.rm=TRUE))

area$hhno <- as.character(area$hhno)


#######################################
############### ASSETS ################
#######################################

# -------------------------------------
# follow Sheahan and define assets as
# value of the livestock and farm
# implements
# -------------------------------------

# -------------------------------------
# livestock - note there is a question on
# value of all animals sold
# -------------------------------------

lvstk <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S3AI.dta")) %>% 
        select(hhno, lvstk=animal_id, qty=s3ai_1, valu=s3ai_3i) %>%
        mutate(prc=valu/qty)

lvstk$lvstk <- as_factor(lvstk$lvstk)

# select only the larger animals
big <- c("Drought Animal", "Cattle", "Sheep", "Goats", "Pigs")
lvstk <- lvstk[lvstk$lvstk %in% big,]

lvstk <- ddply(lvstk, .(lvstk), transform,
               valu=ifelse(is.na(valu), mean(prc, na.rm=TRUE)*qty, valu))

# calculate per houshold livestock wealth
lvstk <- group_by(lvstk, hhno) %>%
        summarise(lvstk_valu=sum(valu))

lvstk$hhno <- as.character(lvstk$hhno)

# -------------------------------------
# value of farm equipment
# -------------------------------------

implmt <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S3AII.dta")) %>%
        select(hhno, implmt=s3aii_0, qty=s3aii_a, valu=s3aii_c1) %>%
        filter(!is.na(implmt) & !qty %in% 0)

implmt$implmt <- as_factor(implmt$implmt)

# drop any misisng values for valu variable
# only 8 of them. get implmt valu per hh

implmt <- filter(implmt, !is.na(valu)) %>%
        group_by(hhno) %>% 
        summarise(implmt_valu=sum(valu))

implmt$hhno <- as.character(implmt$hhno)



#######################################
############ FALLOW/IRRIG #############
#######################################

plot <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data/S4AIII.dta")) %>%
  select(hhno, plotno=s4aiii_plotno, fallowB=s4aiii_a18a,
         fallowF=s4aiii_a19a, irrig=s4aiii_a26)

plot$fallowF <- as.integer(plot$fallowF)
plot$fallowB <- as.integer(plot$fallowB)
plot$irrig <- ifelse(plot$irrig %in% 1, 1, 0)

plot$hhno <- as.character(plot$hhno)



#######################################
########### CROSS SECTION #############
#######################################

# at the plot level
GHA2010 <- left_join(oput_maj_mze, chem_maj_maize)
GHA2010 <- left_join(GHA2010, area)
GHA2010 <- left_join(GHA2010, plot)
GHA2010 <- left_join(GHA2010, lab1)
GHA2010 <- left_join(GHA2010, lab2)
GHA2010 <- left_join(GHA2010, lab3)
GHA2010 <- left_join(GHA2010, lab4)

# at the household level
GHA2010 <- left_join(GHA2010, implmt)
GHA2010 <- left_join(GHA2010, lvstk)

# remove any rows thathav an NA for area
GHA2010 <- GHA2010[!is.na(GHA2010$area),]

#######################################
#######################################
#######################################

# set NA values in selected variables to zero
GHA2010$pest <- ifelse(is.na(GHA2010$pest), 0, GHA2010$pest)
GHA2010$manure <- ifelse(is.na(GHA2010$manure), 0, GHA2010$manure)
GHA2010$inorg <- ifelse(is.na(GHA2010$inorg), 0, GHA2010$inorg)
GHA2010$implmt_valu <- ifelse(is.na(GHA2010$implmt_valu), 0, GHA2010$implmt_valu)
GHA2010$lvstk_valu <- ifelse(is.na(GHA2010$lvstk_valu), 0, GHA2010$lvstk_valu)

GHA2010 <- mutate(GHA2010,
  lab = lab_val1 + lab_val2 + lab_val3 + lab_val4,
  asset = implmt_valu +lvstk_valu
)

# calculate total 

GHA2010 <- mutate(GHA2010,
             yld=qty/area,
             asset=asset/area_tot,
             area2=area^2
)

GHA2010 <- mutate(GHA2010,
  area2=area^2,
  asset2 = asset^2,
  lab2 = lab^2
)

# select only the necessary variables
GHA2010 <- select(GHA2010, -plotno, -qty, -lab_val1, -lab_val2, -lab_val3,
             -lab_val4, -implmt_valu, -lvstk_valu)

# remove everything but the cross section
rm(list=ls()[!ls() %in% c("GHA2010", "dataPath")])

# read in region variables
region <- read_dta(file.path(dataPath, "/EGC-ISSER Public Cleaned Data/S4AV1.dta")) %>%
  select(hhno, reg=id1)
region$reg <- as_factor(region$reg)
region$hhno <- as.character(region$hhno)

GHA2010 <- left_join(GHA2010, unique(region))

# saveRDS(GHA2010, file.path(dataPath, "/../GHA2010.rds"))


