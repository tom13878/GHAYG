# -------------------------------------
# GHA maps
# -------------------------------------

library(haven)
library(raster)
library(ggplot2)
library(dplyr)

# put dataPath up to and including the GHA_presentation folder
#dataPath <- "D:/Data/Projects/GHAYG"
#dataPath <- "N:\\Internationaal Beleid  (IB)\\Projecten\\2285000066 Africa Maize Yield Gap\\SurveyData\\GHA\\2010"
dataPath <- "C:\\Users\\dijk158\\OneDrive - IIASA\\SurveyData"

# install a map gist from github (not mine, so do not publish!!!)

devtools::source_gist("33baa3a79c5cfef0f6df")

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
               aes(group=group, fill=NAME_1), color="white", size=1) +
  guides(fill=FALSE) +
  geom_text(data=dis_points, aes(label = DISTRICT_NAME, x = x, y = y)) +
  theme_map() + 
  geom_point(data=gps, aes(x=longitude, y = latitude), col="black", cex=1)
gmap3

# take out the trash
rm("coords", "gf1", "Ghana1", "Ghana2", "gps", "regions",
   "theme_map", "centroid", "dis_name", "dis_points", "districts",
   "gf2", "gf2.2")
