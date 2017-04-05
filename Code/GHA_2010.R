#######################################
########## GHANA 2010-11 ##############
####################################### 

# ASK TOM ABOUT MINOR SEASON

library(haven)
library(tidyr)
library(reshape2)
library(plyr)
library(dplyr)
library(assertive)
library(sjmisc)
options(scipen=999)

# setWD
# wd <- "D:\\Data\\Github\\GHAYG"
wd <- "C:/Users/morle001/WEcR/GHAYG"
setwd((wd))
#dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData\\GHA\\2010"
dataPath <- "C:/Users/morle001/WEcR/GHA/2010"

#######################################
############## LOCATION ###############
#######################################

location <- read_dta(file.path(dataPath, "Data/SEC0.dta")) %>%
  transmute(REGNAME = toupper(as_factor(id1)), REGCODE = id1, DISCODE = id2, eaid = id3, hhno = as.character(hhno),
            ZONE = threezones,  rural = as_factor(urbrur)) %>%
  remove_all_labels

# match up with the names from the survey (prepared in a seperate file)
REGZONE <- read.csv(file.path(paste0(dataPath,"/../.."), "Other/Spatial/GHA/REGDISGHA.csv"))

# join with household identifications
# There seems to an error in the numbering of regions. Some combinations are not given in the codebook, in particular for urban areas.
# As the join results in incomplete link we do not perform it. 
#location <- left_join(location, REGZONE)

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

oput_maj <- read_dta(file.path(dataPath, "Data/S4AV1.dta"))
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
        select(hhno, plotno, crop_qty_harv=quantity, unit, value_c, value_p, crop_count, legume) %>%
        mutate(value_p = value_p/100)

# make cropcount into a binary variable
oput_maj_mze$crop2 <- ifelse(oput_maj_mze$crop_count %in% 2, 1, 0)
oput_maj_mze$crop3 <- ifelse(oput_maj_mze$crop_count %in% 3, 1, 0)
oput_maj_mze$crop4 <- ifelse(oput_maj_mze$crop_count %in% 4, 1, 0)
oput_maj_mze$crop5 <- ifelse(oput_maj_mze$crop_count %in% 5, 1, 0)

oput_maj_mze <- remove_all_labels(oput_maj_mze)


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
aux <- gather(SEC5A, variable, value, s5a_a:s5a_w)
aux <- aux[!is.na(aux$value),]
aux <- ddply(aux, .(s5a_1, variable), summarize, kilo_bar = mean(value, na.rm=TRUE))


# CHECK: value_p not used?! And maize price x1- too high

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
oput_maj_mze <- mutate(oput_maj_mze, crop_qty_harv = crop_qty_harv *  kilo_bar)


# get rid of the unit variable and NA values for maize quantity
oput_maj_mze <- select(oput_maj_mze, -unit, -kilo_bar)
oput_maj_mze <- oput_maj_mze[!is.na(oput_maj_mze$crop_qty_harv) & !oput_maj_mze$crop_qty_harv %in% 0,]
oput_maj_mze$crop_price <- oput_maj_mze$crop_qty_harv/oput_maj_mze$value_c
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
contain_units <- read.csv(file.path(dataPath, "../../Other/Conversion/GHA/container_units_GHA.csv"))

# convert fertilizer to kilograms using conversions from extension officers
conv_fertunit <- read.csv(file.path(dataPath, "../../Other/Conversion/GHA/Fert_GHA.csv")) %>%
  select(-note)

# Prepare data for each type of chemical

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
         
# For some plots there are duplicated entries (i.e. two types of fertilizer)
# Calculate sum/averages over plots
fert <- fert %>%
        group_by(hhno, plotno) %>%
        summarise(WPn = sum(value_tot, na.rm=T)/sum(N, na.rm=T),
              WPnosub = sum(value_mar, na.rm=T)/sum(N_mar, na.rm=T),
              WPnsub = sum(value_sub, na.rm=T)/sum(N_sub, na.rm=T),
              N = sum(N, na.rm=T),
              N_sub = sum(N_sub, na.rm=T),
              N_mar = sum(N_mar, na.rm=T))


# Remove all inf, nan
inf.nan.na.clean.f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  return(x)
}
fert <- inf.nan.na.clean.f(fert)

# We are not certian in fertilizer conversion factors also apply to other chemicals so we only create simple dummies for there use.
# Note that insecticide and fungicide is very limited on maize crops (<25)

# Herbicide   
herb <- filter(chem_maj_tot, type == "herbicide") %>%
  filter(crop == "Maize") %>%
  group_by(hhno, plotno) %>%
  summarize(qty_tot = sum(qty_tot, na.rm=T)) %>%
  mutate(herb = ifelse(qty_tot >0, 1, 0)) %>%
  select(hhno, plotno, herb) %>%
  remove_all_labels()

# Manure
manure <- filter(chem_maj_tot, type == "manure") %>%
  filter(crop == "Maize") %>%
  group_by(hhno, plotno) %>%
  summarize(qty_tot = sum(qty_tot, na.rm=T)) %>%
  mutate(manure = ifelse(qty_tot > 0, 1, 0)) %>%
  select(hhno, plotno, manure) %>%
  remove_all_labels()

# insecticide
insec <- filter(chem_maj_tot, type == "insecticide") %>%
  filter(crop == "Maize") %>%
  group_by(hhno, plotno) %>%
  summarize(qty_tot = sum(qty_tot, na.rm=T)) %>%
  mutate(insec = ifelse(qty_tot >0, 1, 0)) %>%
  select(hhno, plotno, insec) %>%
  remove_all_labels()

# fungicide
fung <- filter(chem_maj_tot, type == "fungicide") %>%
  filter(crop == "Maize") %>%
  group_by(hhno, plotno) %>%
  summarize(qty_tot = sum(qty_tot, na.rm=T)) %>%
  mutate(fung = ifelse(qty_tot >0, 1, 0)) %>%
  select(hhno, plotno, fung) %>%
  remove_all_labels()


rm(list=c("chem_maj", "chem_maj_tot", "contain_units", "conv_fertunit"))

#######################################
############### LABOUR ################
#######################################

lab_val1 <- read_dta(file.path(dataPath, "Data/S4AIX1.dta")) %>%
  select(-id1, -id2, -hhno, s4aix1_plotno)
lab_val1[is.na(lab_val1)] <- 0
lab_val1 <- rowSums(lab_val1)
lab1 <- read_dta(file.path(dataPath, "Data/S4AIX1.dta")) %>%
  select(hhno, plotno=s4aix1_plotno) %>% cbind(lab_val1)

lab_val2 <- read_dta(file.path(dataPath, "Data/S4AIX2.dta")) %>%
  select(-id2, -id2, -hhno, s4aix2_plotno)
lab_val2[is.na(lab_val2)] <- 0
lab_val2 <- rowSums(lab_val2)
lab2 <- read_dta(file.path(dataPath, "Data/S4AIX2.dta")) %>%
  select(hhno, plotno=s4aix2_plotno) %>% cbind(lab_val2)

lab_val3 <- read_dta(file.path(dataPath, "Data/S4AIX3.dta")) %>%
  select(-id3, -id2, -hhno, s4aix3_plotno)
lab_val3[is.na(lab_val3)] <- 0
lab_val3 <- rowSums(lab_val3)
lab3 <- read_dta(file.path(dataPath, "Data/S4AIX3.dta")) %>%
  select(hhno, plotno=s4aix3_plotno) %>% cbind(lab_val3)

lab_val4 <- read_dta(file.path(dataPath, "Data/S4AIX4.dta")) %>%
  select(-id4, -id2, -hhno, s4aix4_plotno)
lab_val4[is.na(lab_val4)] <- 0
lab_val4 <- rowSums(lab_val4)
lab4 <- read_dta(file.path(dataPath, "Data/S4AIX4.dta")) %>%
  select(hhno, plotno=s4aix4_plotno) %>% cbind(lab_val4)

rm(list=c("lab_val1", "lab_val2", "lab_val3", "lab_val4"))

lab1$hhno <- as.character(lab1$hhno)
lab2$hhno <- as.character(lab2$hhno)
lab3$hhno <- as.character(lab3$hhno)
lab4$hhno <- as.character(lab4$hhno)

#######################################
############### GEO ###################
#######################################

geo10 <- readRDS("Cache/GHA_geo_2010.rds") %>%
  mutate(hhno = as.character(hhno))

#######################################
################ AREAS ################
#######################################

area <- read_dta(file.path(dataPath, "Data/S4AII.dta"))

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
############### TRACTOR USE ###########
#######################################
# hhno is not in the right format so we use the link file SEC0
hhlink <- read_dta(file.path(dataPath, "Data/SEC0.dta")) %>%
  transmute(id1, id2, id3, id4, hhno = as.character(hhno))

tractor <- read_dta(file.path(dataPath, "Data/S4AVII.dta")) %>%
  select(id1, id2, id3, id4, plotno = s4avii_plotno, mech = s4avii_243) %>%
  left_join(hhlink, .) %>%
  mutate(mech = ifelse(mech == 2, 0, mech),
         hhno = as.character(hhno)) %>%
  select(-id1, -id2, -id3, -id4) %>%
  remove_all_labels()


#######################################
############ SEEDS ####################
#######################################
# hhno is not in the right format so we use the link file SEC0
  
seed1 <- read_dta(file.path(dataPath, "Data/S4AVIII1.dta")) %>%
  transmute(id1, id2, id3, id4, plotno = s4aviii1_plotno, crop = as_factor(s4aviii_248i),  seed_type = as_factor(s4aviii_249)) %>%
  filter(crop == "Maize") %>%
  left_join(hhlink, .) %>%
  select(-crop, -id1, -id2, -id3, -id4) %>%
  remove_all_labels()

# Only seed type one used as seed type 2 and 3 are small datasets. We assume seed 1 is the most important
# seed2 <- read_dta(file.path(dataPath, "Data/S4AVIII1.dta")) %>%
#   transmute(hhno = id4, plotno = s4aviii1_plotno, crop = as_factor(s4aviii_253i),  seed_type = as_factor(s4aviii_254)) %>%
#   filter(crop == "Maize") %>%
#   remove_all_labels()

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

lvstk <- read_dta(file.path(dataPath, "Data/S3AI.dta")) %>% 
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

implmt <- read_dta(file.path(dataPath, "Data/S3AII.dta")) %>%
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
############ FALLOW/IRRIG/SOIL ########
#######################################

plot <- read_dta(file.path(dataPath, "Data/S4AIII.dta")) %>%
  transmute(hhno, plotno=s4aiii_plotno, fallowB=s4aiii_a18a,
         fallowF=s4aiii_a19a, irrig=s4aiii_a26,
         soildepth=as_factor(s4aiii_a21), soildepth_unit=as_factor(s4aiii_a22), 
         soiltype=as_factor(s4aiii_a24)) %>%
         remove_all_labels()

# Correct some errors
plot$fallowF <- as.integer(plot$fallowF)
plot$fallowF[plot$fallowF %in% c(2, 4, 8)] <- NA
plot$fallowB <- as.integer(plot$fallowB)
plot$fallowB[plot$fallowB %in% c(2,8, 4)] <- NA
plot$irrig <- ifelse(plot$irrig %in% 2, 0, plot$irrig)
plot$irrig[plot$irrig %in% c(3)] <- 1
plot$hhno <- as.character(plot$hhno)



#######################################
########### CROSS SECTION #############
#######################################

# at the plot level
GHA2010 <- left_join(oput_maj_mze, fert)
GHA2010 <- left_join(GHA2010, herb)
GHA2010 <- left_join(GHA2010, insec)
GHA2010 <- left_join(GHA2010, fung)
GHA2010 <- left_join(GHA2010, manure)
GHA2010 <- left_join(GHA2010, area)
GHA2010 <- left_join(GHA2010, plot)
GHA2010 <- left_join(GHA2010, lab1)
GHA2010 <- left_join(GHA2010, lab2)
GHA2010 <- left_join(GHA2010, lab3)
GHA2010 <- left_join(GHA2010, lab4)
GHA2010 <- left_join(GHA2010, location)
GHA2010 <- left_join(GHA2010, geo10)
GHA2010 <- left_join(GHA2010, seed1)

# at the household level
GHA2010 <- left_join(GHA2010, implmt)
GHA2010 <- left_join(GHA2010, lvstk)
GHA2010 <- left_join(GHA2010, tractor)

#######################################
#######################################
#######################################

# set NA values in selected variables to zero
# This is allowed because question is asked whether they own or use chemicals, assets, etc and then data is explicitely presented for farmers who do.
GHA2010$herb <- ifelse(is.na(GHA2010$herb), 0, GHA2010$herb)
GHA2010$insec <- ifelse(is.na(GHA2010$insec), 0, GHA2010$insec)
GHA2010$fung <- ifelse(is.na(GHA2010$fung), 0, GHA2010$fung)
GHA2010$manure <- ifelse(is.na(GHA2010$manure), 0, GHA2010$manure)
GHA2010$N <- ifelse(is.na(GHA2010$N), 0, GHA2010$N)
GHA2010$N_mar <- ifelse(is.na(GHA2010$N_mar), 0, GHA2010$N_mar)
GHA2010$N_sub <- ifelse(is.na(GHA2010$N_sub), 0, GHA2010$N_sub)
GHA2010$implmt_valu <- ifelse(is.na(GHA2010$implmt_valu), 0, GHA2010$implmt_valu)
GHA2010$lvstk_valu <- ifelse(is.na(GHA2010$lvstk_valu), 0, GHA2010$lvstk_valu)
GHA2010$mech <- ifelse(is.na(GHA2010$mech), 0, GHA2010$mech)

# Rename hhid variable
GHA2010 <- GHA2010 %>% rename(hhid = hhno)


# remove everything but the cross section
rm(list=ls()[!ls() %in% c("GHA2010", "dataPath")])

# Save file
saveRDS(GHA2010, "Cache/GHA2010.rds")


