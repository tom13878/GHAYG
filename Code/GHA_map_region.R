# ghana maps of fertilizer use, maize output etc.
# 11/06/2015

library(rgeos)
library(maptools)
library(raster)
library(Hmisc)
library(rgdal)
library(ggplot2)
library(plyr)
library(data.table)
library(dplyr)
library(haven)

wd <- "D:\\Dropbox\\Michiel_research\\2285000066 Africa Maize Yield Gap\\Analysis\\GHA"
dataPath <- "D:\\Data\\IPOP\\SurveyData\\GHA\\2010\\Data"
mapPath <- "D:\\Data\\IPOP\\SurveyData\\GHA\\2010\\Data\\Maps\\GHA_Districts"

setwd(wd)
GHA <- readOGR(file.path(mapPath, "GHA.shp"), layer="GHA")
ogrListLayers(file.path(mapPath, "GHA.shp"))

map_data <- unique(select(GHA@data, REGION, DISTRICT))
map_data$DISTRICT <- as.character(map_data$DISTRICT)
map_data$REGION <- as.character(map_data$REGION)

# remove "GA SOUTH MUNICIPAL" from Greater Accra region and add "WEIJA"
# map_data <- ddply(map_data, .(REGION)) 
map_data <- map_data[!(map_data$DISTRICT %in% "WA MUNICIPAL"),]
map_data <- rbind(map_data, data.table(REGION=c("Greater Accr", "Upper West"), DISTRICT=c("WEIJA", "WA")))
map_dta <- group_by(map_data, REGION) %>% arrange(DISTRICT)

# now add a regional code to the map_dta dataframe
map_dta <- left_join(map_dta, data.table(REGION=unique(map_dta$REGION), regional_code=c(6,7,2,5,3,8,9,10,4,1)))
map_dta <- map_dta[order(map_dta$regional_code),]
map_dta$district_code <- 1:nrow(map_dta) %>% data.table

# now look at some of the actual data - need regions to be the same names as in
# the map data
chem_tot <- read_dta(file.path(dataPath, "chem.dta"))
# output_maize <- read_dta("M:/GHAYG/data/output_maize.dta")
land_area <- read_dta(file.path(dataPath, "land.dta"))
land_area$reg <- as.character(as_factor(land_area$reg))
chem_tot$type <- as_factor(chem_tot$type)

# now it should be possible to join by region and district code.
# strange dupy values should appear as NA's
chem_tot <- rename(chem_tot, REGION=reg, district_code=district)
chem_tot <- left_join(chem_tot, map_dta)

# DOES NOT WORK
table(tst$REGION[!is.na(tst$DISTRICT)]) # these are the guys that we have districts for

# output_maize <- rename(output_maize, REGION=reg, district_code=district)
# tst <- left_join(output_maize, map_dta)
# table(tst$REGION[!is.na(tst$DISTRICT)])

land_area <- rename(land_area, REGION=reg, district_code=district)
land_area <- left_join(land_area, map_dta)
table(land_area$REGION[!is.na(land_area$DISTRICT)])

# now look at the number of fert users by district in the Ghana data
by_HH <- group_by(chem_tot, DISTRICT, hhno) %>% summarise(Ifert_users=any(type %in% c("Fertilizer (inorganic)")))
by_district <- group_by(by_HH, DISTRICT) %>% summarise(Ifert_users=sum(Ifert_users), N=n())
by_district <- mutate(by_district, prc=Ifert_users/N)

# now try mapping this information with your tanzania map
by_district <- na.omit(by_district)
GHA@data$id <- 0:(nrow(GHA@data)-1)
GHA@data <- left_join(GHA@data, by_district)

gf <- fortify(GHA, regio="id")
gf <- rename(gf, FID_=id)
GHA@data$id <- as.character(GHA@data$id)
lf <- left_join(gf, GHA@data) # DOES NOT WORK

# WHAT ARE YOU INSTALLING HERE?
devtools::source_gist("33baa3a79c5cfef0f6df")
gg <- ggplot(lf) + geom_polygon(aes(long, lat, group = group, fill = prc), color="#0e0e0e", size=0.15)
gg <- gg + theme_map()
gg <- gg + scale_fill_gradientn(colours=c("#ffffff", brewer.pal(n=9, name="YlOrRd")),
                                na.value="#ffffff")
