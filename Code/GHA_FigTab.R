##################################################################################################'
#' PROJECT: DFID yield gap
#' Purpose: Maps and tables for reports and presentations
##################################################################################################'

### PACKAGES
BasePackages<- c("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "ggthemes")
lapply(BasePackages, library, character.only = TRUE)
SpatialPackages<-c("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <- c("frontier", "stargazer", "haven")
lapply(AdditionalPackages, library, character.only = TRUE)

### SET WORKING DIRECTORY
wdPath <- "D:/Data/Github/GHAYG/"
setwd(wdPath)

dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData"

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


###################################
## SUMMARY STATISTICS AND TABLES ##
###################################

# Load data
# Can be sourced later
db1 <- readRDS("Cache/db1.rds")
#db9 <- readRDS("Cache/db9.rds")
#db_sfaCD_CRE_Z <- readRDS("Cache/db_sfaCD_CRE_Z.rds")
#Prices <- readRDS("Cache/Prices_ETH.rds") %>% rename(Pm = maize, Pn = fertilizer)
#source("Code/waterfall_plot.R")
source("Code/sfaTable.R")


# Number of unique households
#length(unique(db9$hhid))

# summary statistics
dbsum <- db1 %>% dplyr::select(Yield = yld, Elevation = elevation, 
                                       yesN, Nitrogen = N, Irrigation = irrig, SOC = SOC2, phdum, 
                                       Labour = lab,  Area = area, Assets = asset, crop_count2, AEZ) 

# dbsum <- db_sfaCD_CRE_Z %>% dplyr::select(Yield = yld, ImprovedSeeds = impr, Slope = slope, 
#                                       yesN, Nitrogen = N, Rain = rain_wq, Irrigation = irrig, SOC = SOC2, phdum, 
#                                       Labour = lab,  Area = area, crop_count2, AEZ, surveyyear2) 

#stargazer(as.data.frame(dbsum), type = "text", digits=2, out=".\\FigTab\\summaryStat.html")


### SFA ANALYSIS
sfaTL <- sfa(logyld ~  
               logN + loglab + logasset +
               logN2 + loglab2 + logasset2 + logNlab + logNasset +
               #logNirrig +  logNrain + loglabirrig +
               logarea +
               irrig + 
               manure + herb + fung + insec +
               mech +
               elevation +
               SOC2 + phdum2 + 
               #rain_wq + 
               AEZ +
               crop_count2 +  
               r
             ,data = db1, maxit = 1500, restartMax = 20, tol = 0.000001)
summary(sfaTL, extraPar = TRUE)
lrtest(sfaTL)

model <- sfaTL
rawTable <-as.data.frame(summary(model, extraPar = F)$mleParam)
varnames <- rownames(rawTable)
xvar <- varnames[c(1:26)]
  
rawTable <- rawTable %>%
    mutate(variable = varnames) %>%
    dplyr::rename(P = `Pr(>|z|)`) %>%
    mutate(sign = ifelse(P <= 0.001, "***",
                         ifelse(P <= 0.01, "**",
                                ifelse(P <= 0.05, "*", ""))))
  
sfaTable <- data.frame(variable = varnames, stringsAsFactors=FALSE)
sfaTable$Coef. <- sprintf("%.3f", round(rawTable$Estimate, 2))
sfaTable$"Std. Error" <- sprintf("%.3f", round(rawTable$"Std. Error", 2))
sfaTable$sign <- rawTable$sign
  
xtable <- filter(sfaTable, variable %in% xvar) %>%  setNames(c("", "Coef.", "std. Error", ""))


###################################
################ MAPS #############
###################################

### GYGA MAP PREPARATION

# Load polygon map and cut out tartet country
GYGApath <- "D:\\Data\\IPOP\\GYGA\\"
dsn=paste(GYGApath, "\\CZ_SubSaharanAfrica\\CZ_AFRGYGACNTRY.shp", sep="")
ogrListLayers(dsn)
ogrInfo(dsn, layer="CZ_AFRGYGACNTRY")
GYGA.Africa <- readOGR(dsn, layer = "CZ_AFRGYGACNTRY")
projection(GYGA.Africa) # check projection
GYGA.Africa <- spTransform(GYGA.Africa, CRS("+proj=longlat +datum=WGS84")) 

# Cut out GHA from GYGA map
GYGA_country <- GYGA.Africa[GYGA.Africa$REG_NAME=="Ghana",]
plot(GYGA_country)
rm(GYGA.Africa)

# Get GYGA data and add YG
GYGA_yg_df <- read_excel(file.path(GYGApath, "GygaGhana.xlsx"), sheet=3) %>%
  dplyr::filter(CROP=="Rainfed maize") %>%
  mutate(YG = (YW-YA)/YW*100)

# Link yield gap data
# in order to merge data with spatialpolygondataframe the row.names need to be the same.
# For this reason I first merge the additionald data and then create a spatialpolygondataframe
# again ensuring that the rownames are preserved.

GYGA_df <- as(GYGA_country, "data.frame") %>%
  rename(CLIMATEZONE = GRIDCODE)
GYGA_df$id <-row.names(GYGA_df)
GYGA_df <- left_join(GYGA_df, GYGA_yg_df)
row.names(GYGA_df) <- GYGA_df$id
GYGA_country <- SpatialPolygonsDataFrame(as(GYGA_country, "SpatialPolygons"),data=GYGA_df)

# Maps with GYGA yield potential and plot information
# transform shapefile in dataframe for ggplot. rownames are used as ID for the countries. Using ID gives strange results. 
# Additional data is linked back again
GYGA_df_f <- fortify(GYGA_country) %>%
  left_join(., GYGA_df)

### GYGA YIELD GAP 
# Draw map
GYGA_YG <- ggplot()+
  geom_polygon(data=GYGA_df_f, aes(x=long, y=lat, group=group, fill=YG), colour="black")+
  geom_polygon(data=subset(GYGA_df_f, is.na(YG)), aes(x=long, y=lat, group=group), fill="white", colour="black") +
  #scale_fill_gradientn(colours = terrain.colors(10)) +
  #scale_fill_gradient(low = "light green", high = "dark green") +
  scale_fill_distiller(palette = "Spectral", name = "%") +
  coord_equal() +
  labs(
    title = "Water-limited yield gap in Ghana (%)",
    #subtitle = "check",
    caption = "Source: Global Yield Gap Atlas (www.yieldgap.org)",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
GYGA_YG



### COMBINED YIELD GAP AND LSMS-ISA MAP
# 
# # Add clases for YW
# GYGA.country.fort$yieldclass <- cut(GYGA.country.fort$YW, breaks=c(6, 8.5, 11, 13.5, 16, 19))
# 
# # Prepare mean yield data
# # meanYield <- db9 %>%
# #   group_by(lon, lat) %>%
# #   summarize(n=n(),
# #             meanYield = (sum(Y*area)/sum(area))/1000) %>%
# #   filter(n>1)
# # meanYield$meanYield2 <- cut(meanYield$meanYield, breaks=c(0, 1, 2, 3, 12))
# 
# # Draw map
# GYGA_LSMS <- ggplot()+
#   geom_polygon(data=GYGA.country.fort, aes(x=long, y=lat, group=group, fill=yieldclass), colour="black")+
#   geom_polygon(data=subset(GYGA.country.fort, is.na(YW)), aes(x=long, y=lat, group=group), fill="white", colour="black") +
#   scale_fill_discrete(name="Potential water\nlimited yield (tons)") +
#   geom_point(data=meanYield, aes(x=lon, y=lat, size=(meanYield2)), colour="black")+
#   scale_size_manual(name="Average yield (tons)", values=c(1, 2, 3, 4, 5)) +
#   coord_equal()+
#   labs(x="", y="")+
#   theme_classic() +
#   theme(legend.key=element_blank(),
#         line = element_blank(),
#         axis.text = element_blank())
# 
# GYGA_LSMS
# ggsave(plot = GYGA_LSMS, ".\\Graphs\\GYGA_LSMS.png", height = 150, width = 200, type = "cairo-png", units="mm")


### ADMINISTRATIVE MAPS
# Load maps at region and district level
Ghana1 <- readRDS(file.path(dataPath, "Other\\Spatial\\GHA\\GADM_2.8_GHA_adm1.rds"))
Ghana2 <- readRDS(file.path(dataPath, "Other\\Spatial\\GHA\\GADM_2.8_GHA_adm2.rds"))

# get regions to color the map
regions <- unique(dplyr::select(Ghana1@data, OBJECTID, REGION_CODE=ID_1, REGION_NAME=NAME_1))
regions$REGION_NAME <- toupper(regions$REGION_NAME)
regions$OBJECTID <- as.character(regions$OBJECTID)

# fortify maps and add region names
gf1 <- fortify(Ghana1, regions=OBJECTID)
gf1 <- left_join(gf1, regions, by=c("id"="OBJECTID"))

# base plot for the regions with text labels for
# each region instead of a legend using the
# coordinates function from sp package

coords <- as.data.frame(coordinates(Ghana1))
names(coords) <- c("x", "y")
coords <- cbind(REGION_NAME = unique(Ghana2$NAME_1), coords)

gmap1 <- ggplot(gf1) + 
  geom_polygon(data=gf1, aes(x=long, y=lat, group=group, fill=REGION_NAME), colour="black",
               size = .1) +
  coord_map("mercator") +
  theme_map() +
  guides(fill=FALSE) +
  geom_text(data=coords, aes(label = REGION_NAME, x = x, y = y)) 
gmap1

# -------------------------------------
# add locations of the households
# -------------------------------------

gps <- read_dta(file.path(dataPath, "GHA//2010//Data/ShiftedGPSData.dta"))
gmap2 <- gmap1 + geom_point(data=gps, aes(x=longitude, y = latitude), col="black", alpha = 0.3)
gmap2

# -------------------------------------
# Look at the districts
# -------------------------------------

gf2 <- fortify(Ghana2)
districts <- unique(dplyr::select(Ghana2@data, OBJECTID, NAME_1, ID_2, NAME_2))
districts$OBJECTID <- as.character(districts$OBJECTID)
gf2.2 <- left_join(gf2, districts, by=c("id"="OBJECTID"))

# we can highlight any district we want
# to focus on.

dis_name = c("Tolon-Kumbungu", "Nkoranza")
centroid <- as.data.frame(coordinates(Ghana2))
names(centroid) <- c("x", "y")
centroid <- cbind(DISTRICT_NAME = Ghana2$NAME_2, centroid)
dis_points <- centroid[centroid$DISTRICT_NAME %in% dis_name,]


gmap3 <- ggplot(gf2.2, aes(long, lat)) +
  coord_map() +
  geom_polygon(aes(group=group, fill=NAME_1), colour="black") +
  guides(fill=FALSE) +
  geom_polygon(data=subset(gf2.2, NAME_2 %in% dis_name),
               aes(group=group), fill = "white", color="white", size=1) +
  guides(fill=FALSE) +
  geom_text(data=dis_points, aes(label = DISTRICT_NAME, x = x, y = y)) +
  #theme_map() + 
  geom_point(data=gps, aes(x=longitude, y = latitude), col="black", size = 0.5, alpha = 0.5) +
  labs(
    title = "Location of sample sites and demonstration plots",
    #subtitle = "check",
    caption = "Source: EGC-ISSER Ghana Panel Survey",
    x="", y="") +
  theme_classic() +
  theme(legend.key=element_blank(),
        line = element_blank(),
        axis.text = element_blank())
gmap3

# Save image for use in Rmarkdown
save.image("Cache/FigTab.Rdata")

