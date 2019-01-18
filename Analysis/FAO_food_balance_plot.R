#'==============================================================================
#' Project:  
#' Subject:  
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   
#'==============================================================================

# ------------------------------------------------------------------------------
#' Notes: 
# ------------------------------------------------------------------------------

library(pacman)
p_load(char=c("countrycode", "rprojroot", "dplyr", "readxl", "tidyr", "WDI"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)

# get datapath
source(file.path(root, "code/get_dataPath.r"))

# ------------------------------------------------------------------------------
# food balance information is gathered using bulk download data from
# FAOSTAT and then filtered on world, 
dat_raw <-read.csv(file.path(data_path, "data/FoodBalanceSheets_E_All_Data_(Normalized).csv")) %>%
  select(country.code = Area.Code,
         country = Area,
         item=Item,
         variable=Element,
         year=Year,
         value=Value,
         Unit) %>%
  filter(country.code < 5000,
         year > 2004,
         item %in% c("Maize and products", "Rice (Milled Equivalent)", "Wheat and products", "Grand Total"),
         variable %in% c("Domestic supply quantity", "Food", "Food supply (kcal/capita/day)")) %>%
  select(-Unit)

  # ==============================================================================
# calculate the percentage of domestic supply of wheat, rice and maize
# that goes to food


# calculate the percentage of domestic supply goes to food
dat <- dat_raw %>%
  filter(item != "Grand Total") %>%
  spread(variable, value)

# assumption is that the share of food in domestic supply is the same as the
# share of production which goes to food. 
dat <- mutate(dat,
               food_prop = pmin(1, (`Food`)/(`Domestic supply quantity`)))
  
dat <- select(dat, country, country.code, year, item, Food, food_prop, avg_kcal = `Food supply (kcal/capita/day)`)

dat$item <- factor(dat$item)
levels(dat$item) <- c("maize", "rice", "wheat")

# spread data - yields are in hg we convert to tonnes/ha
dat <- gather(dat, variable, value, -country, -country.code, -year, -item) %>%
  unite(variable, c("item", "variable"), remove = TRUE) %>%
  spread(variable, value)

# set any NA values to zero
dat[is.na(dat)] <- 0

# ==============================================================================
# we use the country code package to make sure that all different data sources
# have consistent names on which we can combine information.
# countrycode(dat$country, origin = "country.name", destination = "country.name")
# worked for all countries except Serbia and Montenegro which is no longer a country
dat$country <- countrycode(dat$country, origin = "country.name", destination = "country.name")

# china appears more than once - we take china with country code 351 which is the
# biggest version of china
dat <- filter(dat, !(country %in% "China" & country.code %in% 351))

# remove any NA value countries
dat <- filter(dat, !is.na(country))

# remove country.code varaible (no longer needed - all mathcing across datasets done by country name)
dat <- select(dat, -country.code)

# ==============================================================================
# save for use later
write.csv(dat, file = file.path(root, "data/fao_crop_food_prop.csv"), row.names=FALSE)

# ==============================================================================
# Create file with tot kcal/cap/day

tot_cal <- dat_raw %>%
  filter(item %in% "Grand Total") %>%
  rename(tot_cal = value) %>%
  dplyr::select(country, year, tot_cal)

write.csv(tot_cal, file = file.path(root, "data/fao_tot_cal.csv"), row.names=FALSE)
