#'========================================================================================================================================
#' Project:  DFID
#' Subject:  Compare maize yield internationally
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### SET DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


# LOAD DATA
# Read data for all countries
maize <- read_csv(file.path(root, "Data/Maize_yield.csv"))

# Read data for ETH which consisted of two parts but has been merged again
ETH <- read_csv(file.path(root, "Data/ETH_maize.csv")) %>% 
  select(value = Value, Year) %>%
  mutate(iso3c ="ETH")


# ADD ETH to maize file
maize <- gather(maize, iso3c, value, -Year) %>%
  mutate(value = as.numeric(value)) %>%
  filter(iso3c != "ETH") %>%
  bind_rows(., ETH) %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"),
         region = countrycode(iso3c, "iso3c", "region"),
         continent = countrycode(iso3c, "iso3c", "continent"),
         value = as.numeric(value)/10000)

# Calculate regions averages
maize_reg <- maize %>%
  group_by(region, Year) %>%
  summarize (value = mean(value, na.rm=T)) %>%
  filter(region %in% "South America")

maize_con <- maize %>%
  group_by(continent, Year) %>%
  summarize (value = mean(value, na.rm=T)) %>%
  rename(region = continent) %>%
  filter(region %in% c("Asia", "Africa", "Americas"))

maize_iso <- maize %>%
  filter(iso3c %in% c("ETH", "GHA")) %>%
  select(region = iso3c, value, Year)

df <- bind_rows(maize_reg, maize_con, maize_iso) %>%
  filter(Year >=1980)

Fig_maize_yield <- ggplot(data = df, aes(x = Year, y = value, colour = region)) +
  geom_point() +
  geom_smooth(se = F, size = 2) +
  theme_bw() +
  labs(x = "",
       y = "tons/ha",
       title = "Maize yield by region",
       caption = "Source: FAOSTAT") +
  ylim(0, 7.5)
