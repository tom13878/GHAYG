# -------------------------------------
# mapping for presentation 25/04/2016
# dataframes are saved to be used in 
# the presentation.
# -------------------------------------

library(raster)
library(ggplot2)
library(dplyr)
library(haven)

# put dataPath up to and including the GHA_presentation folder
#dataPath <- "C:/Users/Tomas/Documents/LEI/GHA_presentation"
dataPath <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\GHA\\Presentations\\GHA_presentation"

# install a map gist from github (not mine, so do not publish!!!)
devtools::source_gist("33baa3a79c5cfef0f6df")

# get maps at region and district level

Ghana1 <- getData('GADM', country = "GHA", level = 1) # regions
Ghana2 <- getData('GADM', country = "GHA", level = 2) # districts

# get regions to color the map

regions <- unique(select(Ghana1@data, OBJECTID, REGION_CODE=ID_1, REGION_NAME=NAME_1))
regions$REGION_NAME <- toupper(regions$REGION_NAME)
regions$OBJECTID <- as.character(regions$OBJECTID)

# fortify maps and add region names

gf1 <- fortify(Ghana1, regions=OBJECTID)
gf1 <- left_join(gf1, regions, by=c("id"="OBJECTID"))

# base plot for the regions, color is region name

base <- ggplot(gf1) + 
  geom_polygon(data=gf1, aes(x=long, y=lat, group=group, fill=REGION_NAME), colour="black",
               alpha = .3, size = .1) + coord_map("mercator") + theme_map()

# or with text labels for each region instead of a legend
# using the coordinates function from sp package

coords <- as.data.frame(coordinates(Ghana1))
names(coords) <- c("x", "y")
coords <- cbind(REGION_NAME = unique(Ghana2$NAME_1), coords)

base <- ggplot(gf1) + 
  geom_polygon(data=gf1, aes(x=long, y=lat, group=group, fill=REGION_NAME), colour="black",
               alpha = .3, size = .1) +
  coord_map("mercator") +
  theme_map() +
  guides(fill=FALSE) +
  geom_text(data=coords, aes(label = REGION_NAME, x = x, y = y)) 

# save this for use in presentation
saveRDS(gf1, paste0(dataPath, "/Region_map.rds"))
saveRDS(coords, paste0(dataPath, "/Region_map_coords.rds"))

# -------------------------------------
# add locations of the households
# -------------------------------------

gps <- read_dta(file.path(dataPath, "data/ShiftedGPSData.dta"))
base + geom_point(data=gps, aes(x=longitude, y = latitude), col="red", shape=2)



# -------------------------------------
# now read in the data
# -------------------------------------

GHAdata <- readRDS(file.path(dataPath, "GHA.rds"))
names(GHAdata)

# Start with some summary statistics at the national level
# first remove variables that are unwanted for this summary

unwanted <- c("hhno", "fallowB", "fallowF", "REGION_CODE", "DISTRICT_CODE")
vars <- names(GHAdata)[!names(GHAdata) %in% unwanted]
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# function for calucating summary stats of variables. 
# must be numeric/integer variables
# data: data
# varNames: names of vars you are interested in
# probs: probabilities for quantile function

summTab <- function(data, varNames, probs){
  mean <- sapply(data[, varNames], mean, na.rm=TRUE)
  sd <- sapply(data[, varNames], sd, na.rm=TRUE)
  f <- function(x) quantile(x, probs=probs, na.rm=TRUE)
  quantiles <- t(sapply(data[, varNames], f))
  out <- cbind(mean, sd, quantiles)
  as.data.frame(round(out, 2))
}

# overall mean, sd, and percentiles.
overall <- summTab(GHAdata, vars, probs)
overall
saveRDS(overall, file.path(dataPath, "national_summary.rds"))

# now break the data down into the ten regions and
# find the mean values for variables per region. Now
# we do need REGION_CODE variable

GHAdata2 <- GHAdata[, c(vars, "REGION_CODE")]

by_region_x <- group_by(GHAdata2, REGION_CODE) %>% 
  summarise(n=n())
by_region_y <- group_by(GHAdata2, REGION_CODE) %>% 
  summarise_each(funs(mean))

# means of ever variables by region table
# notice the problem with maize prices, for
# some regions there is simply no data!

by_region <- left_join(by_region_x, by_region_y)
View(by_region)

# -------------------------------------
# now some region level maps to show this
# data
# -------------------------------------

# first get the Region names from the 
# GHA_REG_DIS.csv file. These were copied
# and double checked from the codebook

REGDIS <- read.csv(file.path(dataPath, "GHA_REG_DIS.csv"))
REG <- unique(select(REGDIS, REGION_CODE, REGION_NAME)) # all regions and code from data
by_region <- left_join(REG, by_region) %>% select(-REGION_CODE)

gf1.1 <- left_join(gf1, by_region)
saveRDS(gf1.1, file.path(dataPath, "Region_Map_summary.rds"))

# -------------------------------------
# now some regional level plots
# simply choose the variable you want
# to see and include it in the plot
# using the following function. I'm not 
# sure what sort of style and things you
# prefer; legends etc. So just add to 
# the function whatever you like and
# it should work!!!! Probably need differnt
# colours!!!!
# -------------------------------------

regMap <- function(var) {
  ggplot(gf1.1) + 
  geom_polygon(data=gf1.1,
               aes_string(x="long", y="lat", group="group", fill=var),
               colour="black",
               alpha = .3, size = .1) +
  coord_map("mercator") +
  theme_map()
}

# Examples
# names(GHAdata)
 var = "n"
# var = "asset"
# var = "crop_count"
var = "inorg"
print(regMap(var))

# -------------------------------------
# the more difficult part is to get the
# districts. This is a problem because
# the districts were not properly recorded
# in the actual data files.
# -------------------------------------

# as before the dataframe REGDIS contains all
# of the district regions and names

head(REGDIS)

# and it should be possible to join this file 
# with the data so we can work with districts

test <- left_join(GHAdata, REGDIS)
summary(test)

# but 646 missing region/district combinations.
# upon closer insepection we find there are
# region/district combinations in actual data
# which make no sense compared to the codebook
# for example region 2 should begin with district
# 18, according to the codebook. Instead, district
# 2 is apprently in region 2! But district 2 
# is also in region 1!

test[is.na(test$REGION_NAME), c("REGION_CODE", "DISTRICT_CODE")][1,]
test[test$REGION_CODE %in% 1, c("REGION_CODE", "DISTRICT_CODE")][1:10,]

# Even if we could match the districts, we have another problem. Map
# and data don't match up

unique(Ghana2$ID_2)
unique(GHAdata$DISTRICT_CODE)

# Even if we could rely on the names of the districts, there
# would still be no guarantee these would be spelt the same
# across maps and data! If I recall correctly, and judging
# from the older file, we got the district map from ArcGIS.
# unfortunately I do not have a copy of that one, and it doesn't
# solve the unreliability of the districts in the first place,
# as they do not conform to the codebook.

# we can of course still make maps with the data that
# "matches up" but no guarantees this is actually correct!
# and I don;t think we should match it up with maps. Also,
# while it is interesting for the other project to compare 
# the districts in the national surve with the smaller 
# survey, we cannot draw any conclusions at the district level
# because the survey is not designed to be representative at
# the district level

GHAdata3 <- GHAdata[, c(vars, "REGION_CODE", "DISTRICT_CODE")]

by_district_x <- group_by(GHAdata3, REGION_CODE, DISTRICT_CODE) %>% 
  summarise(n=n())
by_district_y <- group_by(GHAdata3, REGION_CODE, DISTRICT_CODE) %>% 
  summarise_each(funs(mean))

# here are the summarised values for each district

by_district <- left_join(by_district_x, by_district_y)
by_district <- left_join(REG, by_district) %>% select(-REGION_CODE)
View(by_district)

# now make a map of the districts in Ghana

gf2 <- fortify(Ghana2)
districts <- unique(select(Ghana2@data, OBJECTID, ID_2, NAME_2))
districts$OBJECTID <- as.character(districts$OBJECTID)
gf2.2 <- left_join(gf2, districts, by=c("id"="OBJECTID"))
saveRDS(gf2.2, file.path(dataPath, "district_map.rds"))

# and we can also highlight the districts that we are
# interested in by adding another polygon on top of
# the normal one. For example, by selection a district from
# the following list

unique(select(Ghana2@data, ID_2, NAME_2))

# tolon district is called Tolon-Kumbungu
# and Nkoranza is called . . . Nkoranza
# change these to which ever districts are
# best!

dis_name = c("Savelugu Nanton", "Nkoranza")
centroid <- as.data.frame(coordinates(Ghana2))
names(centroid) <- c("x", "y")
centroid <- cbind(DISTRICT_NAME = Ghana2$NAME_2, centroid)
dis_points <- centroid[centroid$DISTRICT_NAME %in% dis_name,]
saveRDS(dis_points, file.path(dataPath, "dis_points.rds"))

ggplot(gf2.2, aes(long, lat)) +
  coord_map() +
  geom_polygon(aes(group=group),fill="light blue", colour="black") +
  guides(fill=FALSE) +
  geom_polygon(data=subset(gf2.2, NAME_2 %in% dis_name),
               aes(group=group, color="white"), size=1, fill="light blue") +
  guides(fill=FALSE) +
  geom_text(data=dis_points, aes(label = DISTRICT_NAME, x = x, y = y)) +
  theme_map()
  
# and the same map again but this time also with the gps points

ggplot(gf2.2, aes(long, lat)) +
  coord_map() +
  geom_polygon(aes(group=group),fill="light blue", colour="black") +
  guides(fill=FALSE) +
  geom_polygon(data=subset(gf2.2, NAME_2 %in% dis_name),
               aes(group=group, color="white"), size=1, fill="light blue") +
  guides(fill=FALSE) +
  geom_text(data=dis_points, aes(label = DISTRICT_NAME, x = x, y = y)) +
  theme_map() + 
  geom_point(data=gps, aes(x=longitude, y = latitude), col="red", shape=2)

# -------------------------------------
# area measurment problem
# -------------------------------------

GHAdata2 <- GHAdata[GHAdata$area < 10, ] # otherwise there is an outlier
saveRDS(GHAdata2, file.path(dataPath, "GHAdata2.rds"))
ggplot(GHAdata2, aes(x=area)) + geom_histogram(binwidth=0.05, fill="red", color="black") +
  theme_bw() + ggtitle("Histogram of land holdings and discretization")

