# PROJECT: IPOP/CIMMYT/DFID
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# R code to link spatial data with LSMS-ISA NGA Household data
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

if(Sys.info()["user"] == "Tomas"){
  dataPath <- "C:/Users/Tomas/Documents/LEI/data/GHA/Data/EGC-ISSER Public Cleaned Data"
} else {
  dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/GHA/2010/Data"
}

# INSTALL PACKAGES AND SET WORKING DIRECTORY
BasePackages <- c("foreign", "stringr", "gdata", "car", "reshape2", "RColorBrewer", "plyr", "dplyr", "tidyr", "haven")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("GSIF", "SPEI", "assertive", "countrycode", "sjmisc")
lapply(AdditionalPackages, library, character.only = TRUE)

# OPTIONS
options(scipen = 999) # surpress scientific notation
options("stringsAsFactors" = FALSE) 
options(digits = 4)

# SET WORKING DIRECTORY
dataPath <- "N:/Internationaal Beleid  (IB)/Projecten/2285000066 Africa Maize Yield Gap/SurveyData/GHA/2010/Data"
setwd(dataPath)

# SET COUNTRY, YEAR and ids
iso3c <- "GHA"
surveyYear <- 2010


# SOURCE FUNCTIONS

# DOWNLOAD BASEMAP
basemapPath = paste0(dataPath, "/../../../",  "Other/Spatial/Maps", "/",iso3c)


# Obtain country coordinates for target country
Get.country.shapefile.f <- function(iso3c, lev=0, proj = "+proj=longlat +datum=WGS84"){
  
  #download country boundary as spatialpolygonDF (and rewrite as .shp for convenience)
  targetfile <- paste(iso3c, paste("_adm", lev, ".Rdata", sep=""), sep="")
  if(file.exists(paste(basemapPath, targetfile, sep="/"))){
    load(paste(basemapPath, targetfile, sep="/"))
  } else {
    gadm=getData('GADM', country=iso3c, level=lev, path=basemapPath)
  }
  
  # change projection 
  projection <- proj
  country.sp <- spTransform(gadm, CRS(projection))
  return(country.sp)
}

country.map <- Get.country.shapefile.f(iso3c, 2)

# PREPARE LSMS SPATIAL DATAPOINTS
# Get y2_hhid-GIS link
HH.geo <- read_dta(file.path(dataPath, "ShiftedGPSData.dta")) %>%
            dplyr::select(lat = latitude, lon = longitude, id3 = district, id4 = hh_index) 

HH.geo2 <- read_dta(file.path(dataPath, "SEC0.dta")) %>%
            dplyr::select(id1, id2, id3, id4, hh2010 = hhno) %>%
            mutate(id1 = as_factor(id1))

# ea.geo <- read_dta(file.path(dataPath, "EGC-ISSER Public Cleaned Data\\EAs.dta")) %>%
#             dplyr::select(id1, id3, eaid = eano) # NOTE: eaid is not unique!


# create list of HH level variables
# lat, lon, and id3, id4 combination is not unique, which should not be possible
geo.hh <- remove_all_labels(HH.geo) %>%
  unique() %>%
  group_by(id3, id4) %>%
  mutate(n=n())

# create list of HH2 level variables
geo.hh2 <- remove_all_labels(HH.geo2) %>% unique() 

# select only unique lat , lon combinations.
geo.base <- left_join(HH.geo2, HH.geo) %>% 
            na.omit %>% 
            dplyr::select(lat, lon) %>% 
            unique %>%
            mutate(eaid = seq(1:length(lat)))


# Create spatial points 
standardproj<-"+proj=longlat +datum=WGS84"
geo.coord <- geo.base %>% 
              dplyr::select(lon, lat) %>%
              as.data.frame(.)%>%
              SpatialPoints(., proj4string=CRS(standardproj))

# REGION INFORMATION
geo.region  <- over(geo.coord, country.map) %>%
  cbind(geo.base,.) %>%
  dplyr::transmute(lat, lon, region_name = as.factor(NAME_1), district_name = as.factor(NAME_2))  


# # CROP CALENDAR - NOT USED
# CropCalendarPath <- "D:\\Data\\IPOP\\CropCalendar\\Processed"
# cc <-stack(paste(CropCalendarPath, "CropCalendar", sep="\\"))
# 
# # Extract data
# geo.cc <- raster::extract(cc, geo.coord) %>%
#   cbind(geo.base,.) %>%
#   rename(start_planting = Maize..Start.of.planting, end_planting = Maize..End.of.planting,
#          start_harvest = Maize..Start.of.harvest, end_harvest = Maize..End.of.harvest)
         
# MONTHLY RAINFALL DATA
montlyRainfallPath <- "D:\\Data\\IPOP\\CRU_TS_3.22\\Processed"
pet <-brick(paste(montlyRainfallPath, "MonthlyEvapotranspiration190101_201312", sep="\\"))
pre <-brick(paste(montlyRainfallPath, "MonthlyPrecipitation190101_201312", sep="\\"))

# extract data
geo.pet <- raster::extract(pet, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pet, X1901.01.16:X2013.12.16)

geo.pre <- raster::extract(pre, geo.coord) %>%
  cbind(geo.base,.) %>% 
  gather(date, pre, X1901.01.16:X2013.12.16)

# Calculate SPEI
# CHECK: http://www.fews.net/west-africa/nigeria
# Check: http://www.fao.org/giews/countrybrief/country.jsp?code=NGA

# Crop calendars indicate that there are two or three maize seasons and sometimes there is even a second crop
# More info is needed if we want to calculate SPEI. For now we select May to Oct

# Funtion to calculate SPEI
spei.f <- function(data, speiperiod, sd, ed){
  spei.calc <- spei(data$speicalc, speiperiod)
  spei.calc <- spei.calc$fitted
  t <- seq(ymd(sd),ymd(ed), by='months')
  spei.df <- data.frame(speicalc=as.matrix(spei.calc), date= t) %>%
    mutate(month = month(date, label=TRUE),
           year = year(date))
  return(spei.df)
}

# Period over which SPEI is calculated corresponding with three month growth period
speiendmonth <- "October"
speiperiod <- 5
gsmonths <- c("May", "Jun", "Jul", "Aug", "Sep")

# Period for which SPEI is estimated. To be increased with more historical data
sd <- "1901-01-01"
ed <- "2013-12-01"

# Calculate SPEI
geo.spei <- left_join(geo.pre, geo.pet) %>%
  do(filter(., complete.cases(.)))

# Add date information. Lubridate used to crash in R because of a problem in dplyr. This is solved by using the development version.
# I use base code here.
library(lubridate)
geo.spei <- geo.spei %>%  mutate(date = gsub("X","", date))
geo.spei$date <- as.Date(geo.spei$date, "%Y.%m.%d")
geo.spei$year <- format(geo.spei$date, "%Y")
geo.spei$month <- format(geo.spei$date, "%b")
geo.spei$days_in_month = days_in_month(geo.spei$date)
geo.spei <- geo.spei %>%
  mutate(petmonth =  pet*days_in_month,
         speicalc = pre-petmonth) %>%
  arrange(eaid, year, month) %>%
  do(filter(., complete.cases(.)))%>%
  ddply(.,.(eaid, lat, lon), spei.f, speiperiod, sd, ed) %>%
  filter(month == speiendmonth & year == surveyYear) %>%
  dplyr::select(-month, - year, -date)
names(geo.spei)[4] <- "SPEI" 

# Total rainfall in growing season
# Add date information. Lubridate used to crash in R because of a problem in dplyr. This is solved by using the development version.
# I use base code here.
geo.monthlyrainfall <- geo.pre %>%  mutate(date = gsub("X","", date))
geo.monthlyrainfall$date <- as.Date(geo.monthlyrainfall$date, "%Y.%m.%d")
geo.monthlyrainfall$year <- format(geo.monthlyrainfall$date, "%Y")
geo.monthlyrainfall$month <- format(geo.monthlyrainfall$date, "%b")
geo.monthlyrainfall <- geo.monthlyrainfall %>%
  filter(month %in% gsmonths) %>%
  group_by(eaid, lat, lon, year) %>%
  summarize(gsRainfall = sum(pre, na.rm=T)) %>%
  filter(year==surveyYear) %>%
  dplyr::select(-year)

# SOIL DATA
# Extract soil data and link to GIS coordinates of plots/housholds
soilPath <- paste("D:\\Data\\IPOP\\AFSIS\\Processed",iso3c, sep="\\")
setwd(soilPath)
countryFiles1km <- list.files(pattern = "\\__1km.tif$")
soil <-stack(countryFiles1km)


# ROOT DEPTH
RootDepthPath <- paste("D:\\Data\\IPOP\\AFSIS\\Processed",iso3c, sep="\\")
setwd(RootDepthPath)
RootDepth <-raster(paste(iso3c, "_", "af_agg_ERZD_TAWCpF23mm__M_1km.tif", sep=""))

# extract data
geo.rootdepth <- raster::extract(RootDepth, geo.coord) %>%
  cbind(geo.base,.) %>%
  do(filter(., complete.cases(.))) # NA values for some regions removed
names(geo.rootdepth)[4] <- "RootDepth"
  
# CARBON STOCK AND PH
# extract data
geo.soil <- raster::extract(soil, geo.coord) %>% 
  cbind(geo.base,.) %>%
  gather(property, value, -eaid, - lat, -lon) %>% 
  separate(property, c("iso3c", "af", "Property", "T", "M1", "M2", "sd", "x", "grid"), sep = "_") %>%
  dplyr::select(-af, -T, -M1, -M2, -x, -grid)


# Prepare final soil variables
# Carbon stock (kg/m2) for a depth of 0-30 cm (sd1-sd3) and 0-100 cm (sd1-sd5)
# http://www.eoearth.org/view/article/156087/ for refs

# Add standard column depths
# Six standard depths following the Global-
# SoilMap.net specifications: sd1 = 2.5 cm (0-5), sd2 = 10 cm (5-15), sd3 = 22.5 cm (15-30), sd4
# = 45 cm (30-60), sd5 = 80 cm (60-100), sd6 = 150 cm (100-200). (p. 34 package documentation)
sds <- get("stsize", envir = GSIF.opts)
soilsize <- data.frame(sd = c("sd1", "sd2", "sd3", "sd4", "sd5", "sd6"), sds = sds)

# SOC sd1-sd3
geo.socsd1_sd3 <- geo.soil %>% 
  filter(Property %in% c("ORCDRC", "CRFVOL", "BLD") & sd %in% c("sd1", "sd2", "sd3")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  spread(Property, value) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  mutate(SOC = as.numeric(OCSKGM(ORCDRC, BLD, CRFVOL, sds*100))) %>%
  group_by(eaid, lat, lon) %>%
  summarize(SOC = sum(SOC, na.rm=FALSE))

# SOC sd1-sd5
geo.socsd1_sd5 <- geo.soil %>% 
  filter(Property %in% c("ORCDRC", "CRFVOL", "BLD") & sd %in% c("sd1", "sd2", "sd3", "sd4", "sd5")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  spread(Property, value) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  mutate(SOC = as.numeric(OCSKGM(ORCDRC, BLD, CRFVOL, sds*100))) %>%
  group_by(eaid, lat, lon) %>%
  summarize(SOC2 = sum(SOC, na.rm=FALSE))

geo.soc <- left_join(geo.socsd1_sd3, geo.socsd1_sd5)

# Ph sd1_sd3
geo.phsd1_sd3 <- geo.soil %>% 
  filter(Property %in% c("PHIHOX") & sd %in% c("sd1", "sd2", "sd3")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  group_by(eaid, lat, lon)  %>%
  summarize(ph = sum(value*sds)/sum(sds)) # Weighted average
  
# Ph sd1_sd5
geo.phsd1_sd5 <- geo.soil %>% 
  filter(Property %in% c("PHIHOX") & sd %in% c("sd1", "sd2", "sd3", "sd4", "sd5")) %>%
  arrange(lat, lon, Property) %>%
  left_join(., soilsize) %>%
  do(filter(., complete.cases(.))) %>% # NA values for some regions removed
  group_by(eaid, lat, lon)  %>%
  summarize(ph2 = sum(value*sds)/sum(sds)) # Weighted average

geo.ph <- left_join(geo.phsd1_sd3, geo.phsd1_sd5)
            

# GYGA DATA
GYGApath <- "D:\\Data\\IPOP\\GYGA\\"

# Get GYGA map
dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA <- readOGR(dsn, layer = "CZ_AFRGYGACNTRY") %>%
  spTransform(., CRS("+proj=longlat +datum=WGS84"))

# Link yield gap data
# Yield gap data is provided in a separate file.
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.

GYGA.yield.data <- read.xls(file.path(GYGApath, "GygaRainfedMaizeSubSaharanAfrica.xlsx"), sheet="Climate zone") %>%
  rename(GRIDCODE = CLIMATEZONE, REG_NAME = COUNTRY) %>%
  mutate(iso = countrycode(REG_NAME,"country.name", "iso3c")) 
GYGA@data <- GYGA@data %>% mutate(iso = countrycode(REG_NAME,"country.name", "iso3c")) %>%
  left_join(., GYGA.yield.data)

# Extract data
geo.GYGA <- raster::extract(GYGA, geo.coord) %>%
  dplyr::select(CROP, YA, YW, YW.YA, YP, YP.YA, iso) %>%
  cbind(geo.base,.)

# FARMING SYSTEMS 
# http://rpackages.ianhowson.com/rforge/raster/man/factor.html on factor values in raster
# http://oscarperpinan.github.io/rastervis/ for plotting raster
fsPath <- "D:\\Data\\IPOP\\FarmingSystems\\fs_2012_tx--ssa.tif_5"
fs <-raster(file.path(fsPath, "FS_2012_TX--SSA.tif"))

# plot maps
p <- crop (fs, country.map)
levelplot(p) +layer(sp.polygons(country.map, col='black')) 

# Extract factor values
geo.fs <- raster::extract(fs, geo.coord)
fs_val <- factorValues(fs, geo.fs)

# Extract data, remove numbering and turn into factor
geo.fs <- raster::extract(fs, geo.coord) %>% 
  cbind(geo.base,.) %>%
  cbind(., fs_val) %>%
  mutate(fs = str_sub(category, 4),
         fs = factor(trimws(fs))) %>%
  dplyr::select(-.,-category)

# ADDITIONAL DATA THAT IS NOT PRESENT IN THE GHA SURVEY BUT ALREADY ADDED IN THE LSMS
# AEZ
AEZPath <- "D:\\Data\\IPOP\\AEZ\\AEZ_CODE_2"
AEZ <- raster(file.path(AEZPath, "AEZ_CODE.asc"))

# Get data
geo.aez <- raster::extract(AEZ, geo.coord) %>%
    cbind(geo.base,.) %>%
    setNames(c("lat", "lon", "eaid", "AEZ")) 

# ELEVATION
# Elevation is already presented by the survey
geo.elev <- read_dta(file.path(dataPath, "ShiftedGPSData.dta")) %>%
  dplyr::select(lat = latitude, lon = longitude, elevation = altitude) 


# BIND ALL SPATIAL INFORMATION
geo.total <-  left_join(geo.region, geo.monthlyrainfall) %>%
  left_join(., geo.spei) %>%
  left_join(., geo.rootdepth) %>%
  left_join(., geo.ph) %>% 
  left_join(., geo.soc) %>%
  left_join(., geo.GYGA) %>%
  left_join(., geo.fs) %>%
  left_join(., geo.aez) %>%
  left_join(., geo.elev)


# plot all points on map
plot(GYGA[GYGA$iso==iso3c,])
plot(country.map)
points(geo.coord, pch = 18, col="red")

# check missing values (not including GYGA data for which we know data is missing)
geo.check <- geo.total %>% dplyr::select(-CROP:-iso) %>% do(filter(., !complete.cases(.))) 
geo.check.plot <- geo.check %>% 
  dplyr::select(lon, lat) %>%
  SpatialPoints(., proj4string=CRS(standardproj))

# Plot missing values in GoogleMaps
# Does NOT work in explorer => set default browser to Firefox
# CHECK all SPEI values are missing!
library(plotGoogleMaps)
geo.check.google <- spTransform(geo.check.plot, CRS('+init=epsg:28992'))
m <- plotGoogleMaps(geo.check.google)

# Plot missing values on map
# Separate plot for missing AEZ values shows that all missing points are located om the coast.
# CHECK: TRY http://r-sig-geo.2731867.n2.nabble.com/Assign-a-point-to-its-nearest-polygon-td6482330.html
# To locate points to nearest polygon

plot(country.map)
points(geo.check.plot, pch = 18, col="red")


# MERGE LSMS AND GEO DATA 

# Merge plot and household level data
# Several plots have more than two lat, lon codes, which is impossible => deleted.
# A number of hh do not have geo-coordinates.
geo.hh <- filter(geo.hh, n <2) %>% dplyr::select(-n)

# Rename and recode 
AEZ_code <- read.csv(file.path(paste0(dataPath,"/../../.."), "Other\\Spatial\\Other\\AEZ_code.csv"))

distance <- read_dta(file.path(dataPath, "S4AII.dta")) %>%
              dplyr::select(hhno, plotno = plot_no, dist_hh = s4aii_a15b) %>%
              mutate(dist_hh = zap_empty(dist_hh)) %>%
              remove_all_labels()

# make two new variables called dist and unit
# watch out for how punctuation is delat with.
# for example "V" in units actually belongs
# to "V.CLOSE" and there is also "VERY CLOSE"

dist <- strsplit(distance$dist_hh, "[[:alpha:]]")
dist <- lapply(dist, function(x) x[!x %in% ""])
dist <- unlist(lapply(dist, function(x) ifelse(length(x)==0, NA, x)))
dist <- as.numeric(dist)

unit <- strsplit(distance$dist_hh, "[^[:alpha:]]")
unit <- lapply(unit, function(x) x[!x %in% ""])
unit <- unlist(lapply(unit, function(x) ifelse(length(x)==0, NA, x)))

# many spellings of kilometer, miles, meters
KM <- grep("KILO", unit)
unit[KM] <- "KM"; rm(KM)
MILE <- grep("MIL", unit)
unit[MILE] <- "MILE"; rm(MILE)
METRE <- grep("MET", unit)
unit[METRE] <- "METRE"; rm(METRE)

# remaining spellings are much more
# idiosyncratic

unit <- ifelse(unit %in% "K", "KM", unit)
unit <- ifelse(unit %in% "KMS", "KM", unit)
unit <- ifelse(unit %in% "M", "METRE", unit)
unit <- ifelse(unit %in% "MLIE", "MILE", unit)
unit <- ifelse(unit %in% "MIN", "MINS", unit)

# make assumption about "AROUND" == 10 metres
dist <- ifelse(unit %in% "AROUND", 10, dist)
unit <- ifelse(unit %in% "AROUND", "METRE", unit)

distance$dist_hh <- dist; rm(dist)
distance$dist_unit <- unit; rm(unit)

# convert everything to KM
distance$dist_hh <- ifelse(distance$dist_unit %in% "METRE", distance$dist_hh/1000, distance$dist_hh)
distance$dist_hh <- ifelse(distance$dist_unit %in% "MILE", distance$dist_hh*1.609, distance$dist_hh) # conversion from google
distance$dist_hh[!distance$dist_unit %in% c("METRE", "KM", "MILE")] <- NA

# unit no longer needed as everything in KM
distance$dist_unit <- NULL

geo.total.plot <- left_join(geo.hh2, geo.hh) %>%
            rename(hhno = hh2010) %>%
            left_join(., distance) %>%
            left_join(., geo.total) %>%
                  transmute(
                  lat,
                  lon,
                  AEZ,
                  hhno, 
                  plotno,
                  dist_hh, 
                  elevation,
                  region_name, district_name, 
                  ph, ph2,
                  SOC, SOC2, rain_CRU=gsRainfall,
                  SPEI, RootDepth,
                  fs, 
                  YA, YW, YP
                ) %>%
                mutate(AEZ = droplevels(factor(AEZ,
                                               levels = AEZ_code$code,
                                               labels = AEZ_code$AEZ)))
              
# Write file
saveRDS(geo.total.plot, file = "D:\\Data\\Projects\\GHAYG\\Cache\\GHA_geo_2010.rds")

