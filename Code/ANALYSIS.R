#######################################
###### ANALYSIS of GHA panel data #####
#######################################

# CHECK:
# GEO AND AREA TO RUN FROM ETHYG FOLDER!!

# Imputation of dummy variables.
# Mean scale variables (see Henningsen)
# SFA from other package
# Add texture to soil variables

#######################################
############## PACKAGES ETC ###########
#######################################

library(dplyr)
library(stargazer)
library(broom)
library(DescTools)
library(ggplot2)
library(xtable)
library(frontier)
library(moments)
library(tidyr)
library(openxlsx)
library(frontier)
library(moments)
library(AER)

wdPath <- "D:\\Data\\Projects\\GHAYG"
wdPath <- "C:/Users/morle001/WEcR/GHAYG"
setwd(wdPath)

source("Code/winsor.r")
options(scipen=999)

#######################################
############## LOAD DATA ##############
#######################################

# Load 2010 data
dbP <- readRDS("Cache/GHA2010.rds")

#######################################
############## CLEANING ###############
#######################################


# Create id for plots
dbP <- dbP %>% 
  mutate(id=1:dim(.)[1]) 

# Cleaning and analysis depends strongly on which measure is chosen for area, which is the denominator for many variables.
# there are three possible yield variables. 
# that can be created for the last two waves of data. 
# 1. yld1: above uses the full gps areas as denominator
# 2. yld2: uses harvested area as denominator
# 3. yld3: Uses relative harvest area to correct gps area
# To simplify the code we set these values in this part. Subsequent analysis code can then be used for any definition of yield.

# GHA does not present information  on area harvested, only plot size. Area is only farmer self-assessed. GPS is not presented. 
# We use area here but definition is comparable to yld1

dbP <- dbP %>% 
  mutate(
    area = area, 
    #area = area_gps,
    yld = crop_qty_harv/area,
    N = N/area)


# As we focus on small scale farmers we restrict area size
dbP <- filter(dbP, area <=10)

# cap yield at 18593 kg/ha, the highest potential yield in ETH (not water limited)
dbP <- filter(dbP, yld <= 15229.37956)

# Restrict attention to plots that use N < 700. 700kg/hectare  represents an upper bound limit associated with inorganic fertilizer use in the United States under irrigated corn conditions (Sheahan & Barett 2014) 
dbP <- filter(dbP, N < 700)

# Filter out plots with zero labour
# NOTE we only have harvest labour
# CHECK might replace this with adult equivalent variable!
dbP <- dbP %>%
  mutate(lab = lab_val1 + lab_val2 + lab_val3 + lab_val4) %>%
  filter(lab >0) %>%
  select(-lab)


# CHECK ADD SOIL VARIABLES
# Select relevant variables and complete cases
db0 <- dbP %>% 
  dplyr::select(hhid, id, ZONE, REGNAME, plotno, # ZONE AND REGNAMES reversed to remain consistent with other LSMS
                AEZ, fs,
                SOC, SOC2, ph, ph2, RootDepth, 
                #soildepth, soiltype,
                #rain_year, rain_wq, 
                #SPEI,
                #YA, YW, YP,
                elevation,
                #nutr_av,
                yld, 
                crop_qty_harv, 
                #sold_qty_kg, sold_qty_gr,
                lab_val1, lab_val2, lab_val3, lab_val4,
                implmt_valu, lvstk_valu,
                fung, herb, insec, manure, mech, 
                #seed_type,
                N, 
                irrig, 
                area, area_tot, 
                #sex, age,
                #ed_any, family_size, credit,
                #literate, cage, death, N1555,
                dist_hh, 
                #trans_cost,
                #title,
                #popEA,
                #extension, extension2,
                #fert_source,
                #road, cost2small_town, bank, micro_finance, ext_agent,
                crop_count, 
                rural, 
                lat, lon)


summary(db0)


#######################################
###### COMPLETE CASES DATABASE ########
#######################################

db0 <- db0 %>%
  do(filter(., complete.cases(.)))


######################################
######## Modify and add variables ####
######################################


# Following Burke
db0$phdum[db0$ph < 55] <- 1
db0$phdum[db0$ph >= 55 & db0$ph <=70] <- 2 # Neutral and best suited for crops
db0$phdum[db0$ph > 70] <- 3
db0$phdum <- factor(db0$phdum)


#db0$phdum2[db0$ph2 < 55] <- 1
db0$phdum2 <- ifelse(db0$ph2 >= 55 & db0$ph2 <=70, 1, 0)
#db0$phdum2[db0$ph2 >= 55 & db0$ph2 <=70] <- 1
#db0$phdum2[db0$ph2 > 70] <- 3
#db0$phdum2 <- factor(db0$phdum2)

# Recode AEZ into 4 zones
# db0$AEZ2 <- db0$AEZ
# db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic-warm / semi-arid"), to = c("Tropic-warm"))
# db0$AEZ2 <- mapvalues(db0$AEZ2, from = c("Tropic-warm / sub-humid"), to = c("Tropic-warm"))
# db0$AEZ2 <- factor(db0$AEZ2)

# Crop count > 1
db0$crop_count2[db0$crop_count==1] <- 1
db0$crop_count2[db0$crop_count>1] <- 0

# additional variables
db0 <- db0 %>% mutate (logyld=log(yld),
                       yesN = ifelse(N>0, 1,0), # Dummy when plot does not use fertilizer, following approach of Battese (1997)
                       noN = ifelse(N<=0, 1,0), # Dummy when plot does use fertilizer, following approach of Battese (1997)
                       logN = log(pmax(N, noN)), # maximum of dummy and N following Battese (1997)
                       lab = lab_val1 + lab_val2 + lab_val3 + lab_val4,
                       #hirelab_sh = harv_lab_hire/(harv_lab_hire + harv_lab)*100,
                       #dumoxen = ifelse(oxen>0, 1,oxen),
                       lab=lab/area,
                       #logae = log(ae),
                       #asset = implmt_value + lvstk2_valu,
                       #assetph=asset/area_tot,
                       #logasset = log(assetph+1),
                       loglab = log(lab),
                       logarea = log(area), # area_gps not area because we want to add plot size as proxy for economies of scale
                       asset = (implmt_valu +lvstk_valu)/area_tot,
                       logasset = log(asset+1)
                       #rain_wq2 = rain_wq*rain_wq,
                       #pestherb = ifelse(herb==1 | pest==1, 1, 0),
                       #ext = ifelse(ext_dummy_pp==1 | ext_dummy_ph ==1, 1, 0),
                       #lograin = log(rain_year),
                       #dumfertsource = recode(fert_source, c("'Government' =  1; else = 0")),
                       #surveyyear2 = replace(surveyyear==2011, 1, 0)
)

# Add Translog variables
db0 <- db0 %>% 
  mutate(logNsq = logN^2,
         loglabsq = loglab^2,
         logassetsq = logasset^2,
         logNlab = logN * loglab,
         logNasset = logN * logasset,
         loglabasset = loglab * logasset
  )

db0 <- droplevels(db0)


######################################
##### Get plot specific pricess ######
######################################

db1 <- db0
# Load and merge price data 
Prices <- readRDS("cache/Prices_GHA.rds")

# Merge with panel data
db1 <- left_join(db0, Prices) %>%
       mutate(relprice = Pns/Pc) %>%
      rename(Pm = Pc)

# Drop unused levels (e.g. Zanzibar in zone), which are giving problems with sfa
db1 <- droplevels(db1)

# ANALYSIS
# run the model
CD <- sfa(logyld ~ logN + loglab + logasset +
            logarea + herb +
            mech + elevation + 
            SOC2  + phdum2, data=db1)

TL <- sfa(logyld ~ logN + loglab + logasset +
                logNsq + loglabsq + logassetsq +
                logNlab + logNasset + loglabasset +
                logarea + herb +
                mech + elevation +
                SOC2  + phdum2, data=db1)

lrtest(TL, CD) # TL model fits better so we proceed with this

# we want to evaluate the frontier function
# and avoid the Z variables
xvars <- names(coef(TL))[1:(length(coef(TL))-2)]
X <- TL$dataTable[, xvars]
X <- as.data.frame(X)
xcoef <- coef(TL)[1:(length(coef(TL))-2)]
relprices <- db1$relprice[TL$validObs]

# function to calculate the MPP
calc_mpp <- function(N, row){
  row$logN <- log(N) # add one because we cannot take log(0)
  logY <- as.matrix(row) %*% xcoef
  Y <- exp(logY)
  MPP <- with(row, ((xcoef["logN"] + 
                       2*xcoef["logNsq"]*logN +
                       xcoef["logNlab"]*loglab +
                       xcoef["logNasset"]*logasset )*
                      (Y/N)))
  MPP
}

# function to calculate the mpp less
# the relative price 
mpp_price <- function(N, row, relprice){
  calc_mpp(N, row) - relprice
}

# function to calculate the economically
# optimal level of nitrogen
econ_opt <- function(i){
  
  # get a plot observation and relative
  # price
  row <- X[i, ]
  relprice <- relprices[i]
  
  # first we need a point above zero
  # but before the root to act as the
  # lower limit of the root finding 
  # function - We cap at 700 because
  # even on the most productive farms
  # 700 kg/ha of nitrogen is excessive
  lower <- tryCatch(optimize(function(x) mpp_price(x, row, relprice),
                             interval=c(1, 700),
                             maximum =TRUE)$maximum, error=function(err) NA)
  
  # then we need to find the root
  # i.e. the point at which the
  # nitrogen level crosses zero. 
  # this is the economically
  # constrained level of nitrogen
  tryCatch(uniroot(function(x) {mpp_price(x, row, relprice)},
                   interval=c(lower, 100000))$root,
           error=function(err) NA)
}

# calculate the economically
# optimal level of nitrogen per plot
db2 <- db1[TL$validObs, ]
db2$Npm <- sapply(1:nrow(X), econ_opt)

# results are not good -> try cobb douglass
# instead
# MPP values make much more sense
MPP <- coef(CD)["logN"]*db1$yld/db1$N

# find optimal N -> values are very large
db2$Npm <- coef(CD)["logN"]*db1$yld/(relprices) # sort prices later

model <- CD

# 1. Technical efficiency yield is found using the
# output of the sfa model
# Observations where Npm cannot be calculated are removed
db2 <- db2 %>%
  rename(Y = yld) %>%
  mutate(
    Ycor = exp(as.numeric(fitted(model))+log(as.numeric(efficiencies(model)))), 
    err = Ycor-Y,
    TEY = exp(as.numeric(fitted(model))),
    TE = as.numeric(efficiencies(model)),
    resid = as.numeric(resid(model))
  )

# 2. Economic yield is found by evaluating the frontier 
# function at the economically optimal nitrogen rate (Npm)
# This means we need to swap out the N (logN) variable
# for the Npm variable, BUT we also need a way of doing
# this for the interaction terms that also involve N
model_vars <- names(CD$olsParam)[-c(1, length(names(CD$olsParam)))]
predict_dat <- db2[, model_vars]
predict_dat <- mutate(predict_dat,
                      logN = log(db2$Npm))


# now make the prediction predict.sfa
# function is not made to handle NA values
# so to keep order and compare with other yield
# measures we probably want to set NA values to
# zero temporarily 
predict_dat$logN[is.na(predict_dat$logN)] <- 0
db2$EY <- exp(predict.sfa(model, predict_dat))
db2$EY[predict_dat$logN == 0] <- NA

# 3. PFY: Feasible yield
# evaluate frontier function at N = 150
# Increase labour and seed rate by 10%
# turn on all dummies.
predict_dat2 <- mutate(predict_dat,
                       logN = log(200),
                       loglab = loglab + log(1.5),
                       logasset = logasset + log(1.5),
                       herb = 1,
                       mech = 1)

# make prediction
db2$PFY <- exp(predict.sfa(model, predict_dat2))

# 4. Potential yield
db2$PY <- 15229.37956

GYGA_YW <- max(db2$PY, na.rm=TRUE)
db2 <- mutate(db2, PY = ifelse(is.na(PY), GYGA_YW, PY))

# join all the yield measures into a single
# dataframe
#  We cap all values at PY because we consider this as an absolute potential and recalculate all gaps.
db2 <- mutate(db2, PFY = ifelse(PY-PFY<0, PY, PFY),
              EY = ifelse(PY-EY<0, PY, EY),
              TEY = ifelse(PY-TEY<0, PY, TEY),
              Ycor = ifelse(PY-Ycor<0, PY, Ycor),
              Y = ifelse(PY-Y<0, PY, Y))

# Calculate TYG using UY as reference
db2 <- db2 %>% 
  mutate(
    ERROR_l = Ycor-Y,      # Error gap
    ERROR_s = Y/Ycor,      # Error gap
    TEYG_l = TEY-Ycor,     # Technical efficiency yield gap using Ycor as basis
    TEYG_s = Ycor/TEY,     # Technical efficiency yield gap using Ycor as basis
    EYG_l = EY-TEY,        # Economic yield gap
    EYG_s = TEY/EY,        # Economic yield gap
    EUYG_l = PFY-EY,       # Feasible yield gap
    EUYG_s = EY/PFY,       # Feasible yield gap
    TYG_l = PY-PFY,        # Technology yield gap
    TYG_s = PFY/PY,        # Technology yield gap
    YG_l = PY-Y,           # Yield gap
    YG_s = Y/PY,           # Yield gap
    YG_l_Ycor = PY-Ycor,   # Yield gap with Ycor as reference
    YG_s_Ycor = Ycor/PY)   # Yield gap with Ycor as reference

# Consistency check of yield gaps.
# ERROR
X_ERROR_check <- filter(db2, ERROR_l<0) # Half of observation has a negative error which is what would be expected
mean(db2$ERROR_l, na.rm=TRUE)
mean(db2$ERROR_s, na.rm=TRUE)

# TEYG
X_TEYG_check <- filter(db2, TEYG_l<0) # Should be zero
mean(db2$TEYG_s, na.rm=TRUE)

# EYG
# A number of plots will have to decrease N use Npm < N. In several cases also plots that do no use N
# will have lower Y when they start using N. This is because there yield can be located above the frontier (based on fertilizer users) because of the positive effect of noN.
# If we believe that these plots are structurally different and do not use fertilizer because of better soils, they will in fact use too much N and have to decrease.
X_EYG_check <- filter(db2, EYG_l<0)        
mean(db2$EYG_s)

# EUYG
# A number of plots have negative EUYG_l because Npm is
# larger than Npy, the nitrogen that is required to
# achieve Potential yield (Yw). We have corrected this
# so check should be 0.
# CHECK: we stil find negative values. It is possible
# that because of the interaction with labour >N results
# in <y? TO BE CHECKED
X_EUYG_check <- filter(db2, EUYG_l<0)        
mean(db2$EUYG_s)
check <- select(X_EUYG_check, hhid, holder_id, parcel_id, field_id,
                surveyyear, ZONE, REGNAME, area, crop_count2,
                lat, lon, noN, yesN, loglab, lab, Npm, mpp, Y, 
                PFY, EY, EUYG_l) 

# TYG
X_TYG_check <- filter(db2, TYG_l<0)        
mean(db2$TYG_s, na.rm=TRUE)

#YG
X_YG_check <- filter(db2, YG_l<0)        
YG_check2 <- filter(db2, YG_l_Ycor<0)        

# Check if separate yield gaps add up to total yield gap
Overall_check <- db2 %>%
  mutate(check_l = YG_l/(ERROR_l + TEYG_l + EYG_l + EUYG_l + TYG_l), # Note that for a small number of observatios YG_l=0 resulting in 0/0 which is NaN
         check_s = YG_s/(ERROR_s * TEYG_s * EYG_s * EUYG_s * TYG_s),
         check_l2 = YG_l_Ycor/(TEYG_l + EYG_l + EUYG_l + TYG_l),
         check_s2 = YG_s_Ycor/(TEYG_s * EYG_s * EUYG_s * TYG_s))
summary(Overall_check)


# Create database with relevant variables for further analysis
db3 <- select(db2, hhid, ZONE,
              REGNAME, crop_count2, area,
              Npm, yesN, Y, N, Ycor, TEY, EY, PFY, PY, ERROR_l,
              ERROR_s, TEYG_l, TEYG_s, EYG_l, EYG_s, EUYG_l,
              EUYG_s, TYG_l, TYG_s, YG_l, YG_s, YG_l_Ycor, YG_s_Ycor)

# save db3 for further analysis
saveRDS(db3, "Cache/db3.rds")
