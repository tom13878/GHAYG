#'========================================================================================================================================
#' Project:  IMAGINE
#' Subject:  Script to create figures
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### LOAD DATA
yield_gaps_gha <- readRDS(file.path(root, "Cache/db3.rds"))

# get rid of NA values for the yield gap. caused by NA values
# for the Npm variable
yield_gaps_gha <- filter(yield_gaps_gha, !is.na(Npm)) # For some observations we cannot find the optimal N

# make regname lower case and remove region suffix
yield_gaps_gha$REGNAME <- gsub(" REGION", "", yield_gaps_gha$REGNAME)
yield_gaps_gha$REGNAME <- str_to_title(tolower(yield_gaps_gha$REGNAME))


# Table with yield levels across each region
YieldLevels <- bind_rows(
  yield_gaps_gha %>% 
    dplyr::select(REGNAME, Y, Ycor, TEY, EY, PFY, PY, area) %>%
    group_by(REGNAME) %>%
    dplyr::summarize(Y = (sum((Y)*area)/sum(area)),
              Ycor = (sum((Ycor)*area)/sum(area)),
              TEY = (sum((TEY)*area)/sum(area)),
              EY = (sum((EY)*area, na.rm=TRUE)/sum(area)), 
              PFY = (sum((PFY)*area)/sum(area)),
              PY = (sum((PY)*area)/sum(area))
    ),
  yield_gaps_gha %>% 
    dplyr::select(REGNAME, Y, Ycor, TEY, EY, PFY, PY, area) %>%
    dplyr::summarize(REGNAME = "Total", 
              Y =(sum((Y)*area)/sum(area)),
              Ycor = (sum((Ycor)*area)/sum(area)),
              TEY = (sum((TEY)*area)/sum(area)),
              EY = (sum((EY)*area, na.rm=TRUE)/sum(area)),
              PFY = (sum((PFY)*area)/sum(area)),
              PY = (sum((PY)*area)/sum(area)))) %>%
  dplyr::select(REGNAME, Y, Ycor, TEY, EY, PFY, PY)
YieldLevels <- dplyr::select(YieldLevels, - Ycor)
names(YieldLevels) <- c("region", "yield", "TEY", "AY", "EY", "PY")


# Table with absolute yield gap information per zone
# Note that by definition, YG_s computed by weighting individual YG_s values is not the same as multiplication of weighted TEYG_s etc.
# We therefore calculate YG_s as the product of the weighted components.
regionYieldGap_l <- bind_rows(
  yield_gaps_gha %>% 
    dplyr::select(REGNAME, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area) %>%
    group_by(REGNAME) %>%
    dplyr::summarize(ERROR_l =(sum((ERROR_l)*area, na.rm=TRUE)/sum(area)),
              TEYG_l = (sum((TEYG_l)*area, na.rm=TRUE)/sum(area)),
              AYG_l = (sum((EYG_l)*area, na.rm=TRUE)/sum(area)),
              EYG_l = (sum((EUYG_l)*area, na.rm=TRUE)/sum(area)),
              TYG_l = (sum((TYG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l = (sum((YG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area, na.rm=TRUE)/sum(area)),
              YG_lcheck = (ERROR_l+TEYG_l+AYG_l+EYG_l+TYG_l)),
  yield_gaps_gha %>% 
    dplyr::select(REGNAME, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area) %>%
    dplyr::summarize(REGNAME = "Total", 
              ERROR_l =(sum((ERROR_l)*area, na.rm=TRUE)/sum(area)),
              TEYG_l = (sum((TEYG_l)*area, na.rm=TRUE)/sum(area)),
              AYG_l = (sum((EYG_l)*area, na.rm=TRUE)/sum(area)),
              EYG_l = (sum((EUYG_l)*area, na.rm=TRUE)/sum(area)),
              TYG_l = (sum((TYG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l = (sum((YG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area, na.rm=TRUE)/sum(area)),
              YG_lcheck = (ERROR_l+TEYG_l+AYG_l+EYG_l+TYG_l))) %>%
  dplyr::select(-ERROR_l,-YG_l, -YG_lcheck)
names(regionYieldGap_l) <- c("region", "TEYG_l", "AYG_l", "EYG_l", "TYG_l", "YG_l_Ycor")

regionYieldGap_l_sh <- regionYieldGap_l %>%
  mutate(
    TEYG = 100*TEYG_l/YG_l_Ycor,
    AYG = 100*AYG_l/YG_l_Ycor,
    EYG = 100*EYG_l/YG_l_Ycor,
    TYG = 100*TYG_l/YG_l_Ycor,
    YG = 100*(TEYG_l + AYG_l + EYG_l + TYG_l)/YG_l_Ycor) %>%
  dplyr::select(-TEYG_l:-YG_l_Ycor) 

# ggplot of the yield gaps in percentage
tmp <- gather(regionYieldGap_l_sh[-6], variable, value, - region)
tmp$variable <- factor(tmp$variable, levels=c("TEYG", "AYG", "EYG", "TYG"))
ggyg <- ggplot(data = tmp, aes(x=region, y = value, fill=variable)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c(brewer.pal(3, "Dark2"), "grey60")) +
  theme_classic()
