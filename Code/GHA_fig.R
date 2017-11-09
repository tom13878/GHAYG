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
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("frontier")

### DETERMINE ROOT PATH
root <- "c:/users/morle001/WEcR/GHAYG"

### DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### SOURCE
source(file.path(root, "Code/waterfall_plot.r"))

### LOAD DATA
db3 <- readRDS(file.path(root, "Cache/db3.rds"))

# in the original data, what we call a zone is
# called a region.
db3$REGNAME <- gsub(" REGION", "", db3$REGNAME)
db3$ZONE <- db3$REGNAME
db3$mpp <- ifelse(db3$mpp > 40|db3$mpp == 0, NA, db3$mpp) # remove outliers
db3$lab <- ifelse(db3$lab > 3000, NA, db3$lab)
db3$asset <- ifelse(db3$asset > 3000, NA, db3$asset)
db3$relprice <- ifelse(db3$relprice > 40, NA, db3$relprice)



# basic zonal summary of yields
by_zone <- group_by(db3, ZONE) %>%
  summarise(n = n(),
            Yield = round(mean(Y), 0),
            Yf = round(mean(Y[yesN == 1]), 0),
            Ynf = round(mean(Y[yesN == 0]), 0),
            PY = round(mean(PY), 0))
Total <- summarise(db3, ZONE = "Total", n = n(),
                   Yield = round(mean(Y), 0),
                   Yf = round(mean(Y[yesN == 1]), 0),
                   Ynf = round(mean(Y[yesN == 0]), 0),
                   PY = round(mean(PY), 0))
by_zone <- rbind(by_zone, Total)
stargazer(by_zone, summary=FALSE)

# basic zonal summary of key variables
by_zone <- group_by(db3, ZONE) %>%
  summarise(n = n(),
            nitrogen = round(sum(yesN)/n, 2) * 100,
            n1 = round(mean(N, na.rm=TRUE), 2),
            n2 = round(mean(N[yesN == 1], na.rm=TRUE), 2),
            area = round(mean(area), 2),
            crops = round(mean(crop_count), 2)
            )
Total <- summarise(db3, ZONE = "Total",
                   n = n(),
                   nitrogen = round(sum(yesN)/n, 2) * 100,
                   n1 = round(mean(N, na.rm=TRUE), 2),
                   n2 = round(mean(N[yesN == 1], na.rm=TRUE), 2),
                   area = round(mean(area), 2),
                   crops = round(mean(crop_count), 2))

by_zone <- rbind(by_zone, Total)
stargazer(by_zone, summary=FALSE)


# nitrogen users, prices, mpp and avcr
by_zone <- group_by(db3, ZONE) %>%
  summarise(n = n(),
            nitrogen = round(sum(yesN)/n, 2) * 100,
            N = round(mean(N[yesN == 1], na.rm=TRUE), 2),
            Npm = round(mean(Npm, na.rm=TRUE), 2),
            MPP = round(mean(mpp, na.rm=TRUE), 2),
            Pn = round(mean(Pns, na.rm=TRUE), 2),
            Pm = round(mean(Pm, na.rm=TRUE), 2),
            relprice = round(mean(relprice, na.rm=TRUE), 2),
            MVCR = round(mean(mpp * (1/relprice), na.rm=TRUE), 2))

Total <- summarise(db3, ZONE = "Total",
                   n = n(),
                   nitrogen = round(sum(yesN)/n, 2) * 100,
                   N = round(mean(N[yesN == 1], na.rm=TRUE),2),
                   Npm = round(mean(Npm, na.rm=TRUE), 2),
                   MPP = round(mean(mpp, na.rm=TRUE), 2),
                   Pn = round(mean(Pns, na.rm=TRUE), 2),
                   Pm = round(mean(Pm, na.rm=TRUE), 2),
                   relprice = round(mean(relprice, na.rm=TRUE), 2),
                   MVCR = round(mean(mpp * (1/relprice), na.rm=TRUE), 2))

by_zone <- rbind(by_zone, Total)
stargazer(by_zone, summary=FALSE)




# Table with yield levels
YieldLevels <- bind_rows(
  db3 %>% 
    select(Zone = ZONE, Y, Ycor, TEY, EY, PFY, PY, area) %>%
    group_by(Zone) %>%
    summarize(Y = (sum((Y)*area)/sum(area)),
              Ycor = (sum((Ycor)*area)/sum(area)),
              TEY = (sum((TEY)*area)/sum(area)),
              EY = (sum((EY)*area, na.rm=TRUE)/sum(area)), # there are NA values -> hence understimate of true EY
              PFY = (sum((PFY)*area)/sum(area)),
              PY = (sum((PY)*area)/sum(area))
    ),
  db3 %>% 
    dplyr::select(Zone = ZONE, Y, Ycor, TEY, EY, PFY, PY, area) %>%
    summarize(Zone = "Total", 
              Y =(sum((Y)*area)/sum(area)),
              Ycor = (sum((Ycor)*area)/sum(area)),
              TEY = (sum((TEY)*area)/sum(area)),
              EY = (sum((EY)*area, na.rm=TRUE)/sum(area)),
              PFY = (sum((PFY)*area)/sum(area)),
              PY = (sum((PY)*area)/sum(area)))) %>%
  select(Zone, Y, Ycor, TEY, EY, PFY, PY)


# Table with absolute yield gap information per zone
# Note that by definition, YG_s computed by weighting individual YG_s values is not the same as multiplication of weighted TEYG_s etc.
# We therefore calculate YG_s as the product of the weighted components.
ZonalYieldGap_l <- bind_rows(
  db3 %>% 
    select(Zone = ZONE, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area) %>%
    group_by(Zone) %>%
    summarize(ERROR_l =(sum((ERROR_l)*area, na.rm=TRUE)/sum(area)),
              TEYG_l = (sum((TEYG_l)*area, na.rm=TRUE)/sum(area)),
              EYG_l = (sum((EYG_l)*area, na.rm=TRUE)/sum(area)),
              EUYG_l = (sum((EUYG_l)*area, na.rm=TRUE)/sum(area)),
              TYG_l = (sum((TYG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l = (sum((YG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area, na.rm=TRUE)/sum(area)),
              YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l)),
  db3 %>% 
    dplyr::select(Zone = ZONE, ERROR_l, TEYG_l, EYG_l, EUYG_l, TYG_l, YG_l_Ycor, YG_l, area) %>%
    summarize(Zone = "Total", 
              ERROR_l =(sum((ERROR_l)*area, na.rm=TRUE)/sum(area)),
              TEYG_l = (sum((TEYG_l)*area, na.rm=TRUE)/sum(area)),
              EYG_l = (sum((EYG_l)*area, na.rm=TRUE)/sum(area)),
              EUYG_l = (sum((EUYG_l)*area, na.rm=TRUE)/sum(area)),
              TYG_l = (sum((TYG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l = (sum((YG_l)*area, na.rm=TRUE)/sum(area)),
              YG_l_Ycor = (sum((YG_l_Ycor)*area, na.rm=TRUE)/sum(area)),
              YG_lcheck = (ERROR_l+TEYG_l+EYG_l+EUYG_l+TYG_l))) %>%
  dplyr::select(-ERROR_l,-YG_l, -YG_lcheck)


ZonalYieldGap_l_sh <- ZonalYieldGap_l %>%
  mutate(
    TEYG = 100*TEYG_l/YG_l_Ycor,
    EYG = 100*EYG_l/YG_l_Ycor,
    EUYG = 100*EUYG_l/YG_l_Ycor,
    TYG = 100*TYG_l/YG_l_Ycor,
    YG = 100*(TEYG_l + EYG_l + EUYG_l + TYG_l)/YG_l_Ycor) %>%
  dplyr::select(-TEYG_l:-YG_l_Ycor) 



# Calculation of potential increase in production when gap is closed on the basis of sample
GapClose1 <- mutate(db3, PROD = Y * area,
                    ERROR_close = ERROR_l*area,
                    TEYG_close = TEYG_l*area,
                    EYG_close = EYG_l*area,
                    EUYG_close = EUYG_l*area,
                    TYG_close = TYG_l*area,
                    POTPROD = PROD + ERROR_close + TEYG_close + EYG_close + TYG_close + EUYG_close)

# Total increase in yield per year
GapClose1a <- GapClose1 %>%
  #group_by(ZONE) %>%
  summarize(PROD = sum(PROD, na.rm=T)/1000000,
            ERROR_close = sum(ERROR_close, na.rm=T)/1000000,
            TEYG_close = sum(TEYG_close, na.rm=T)/1000000,
            EYG_close = sum(EYG_close, na.rm=T)/1000000,
            EUYG_close = sum(EUYG_close, na.rm=T)/1000000,
            TYG_close = sum(TYG_close, na.rm=T)/1000000,
            POTPROD = sum(POTPROD, na.rm=T)/1000000)

# Calculation of potential increase in production when gap is closed on the basis of SPAM and FAO data weighted over two surveyyears.
# As the yield in the LSMS is much lower than the FAO/SPAM yield and also that of the LSMS we apply the different yield shares as found above to the base yield of SPAM.
# We use Ycor as base. This means we assume there was an error (e) and this is corrected for.

SPAMData <- readRDS(file.path(root, "Cache/SPAMData_GHA.rds"))  %>%
  rename(ZONE = zone, Y_SPAM = yield, PROD = TargetProduction) %>%
  mutate(ZONE = toupper(ZONE))


# Closing of yield gaps per zone
# Note that for some regions, notably those with very low potential yield (Central and Western), closing TEY and EY already results in
# Closing the gap. To avoid negative closing the EYG, EUYG and TYG are capped.
# The reason for overshooting can be caused by a variety of factors, including mismeasurement. Most likely is that Nyp is too high for regions
# With a very low potential. The all over impact is low as the involved regions have very limited maize production.


GapClose2 <- db3 %>% 
  group_by(ZONE) %>%
  summarize(
    TEYG_s = sum(TEYG_s*area)/sum(area),
    EYG_s = sum(EYG_s*area)/sum(area),
    #TYG_s = (sum((TYG_s)*area)/sum(area)), # TYG_s based on LSMS yield, not used
    #YG_s = (sum((YG_s)*area)/sum(area)), # YG_s based on LSMS yield, not used
    EUYG_s = sum(EUYG_s*area)/sum(area[!is.na(EUYG_s)]),
    PY = mean(PY, na.rm=T)) %>% # Average of potential yield from GYGA
  left_join(SPAMData, .) %>%
  mutate(
    TEY_SPAM = Y_SPAM/TEYG_s, # TEY using SPAM yield as reference
    EY_SPAM = TEY_SPAM/EYG_s, # EY using SPAM yield as reference
    EY_SPAM = ifelse(PY-EY_SPAM<0, PY, EY_SPAM), # Correct EY if impact of TEYG and EYG results in yield larger than PY.
    EYG_s_SPAM =  TEY_SPAM/EY_SPAM, # Recalculate EYG_s
    UY_SPAM = EY_SPAM/EUYG_s, # UY using SPAM yield as reference
    UY_SPAM = ifelse(PY-UY_SPAM<0, PY, UY_SPAM), # Correct UY if impact of TEYG and EYG results in yield larger than PY.
    EUYG_s_SPAM =  EY_SPAM/UY_SPAM, # Recalculate UEYG_s
    TYG_s_SPAM = UY_SPAM/PY, # Recalculate TYG_s 
    check = TEYG_s*EYG_s_SPAM*EUYG_s_SPAM*TYG_s_SPAM, #check if multiplication of different parts is the same as total
    YG_s = Y_SPAM/PY, # YG_s using SPAM yield as reference
    PTEYG = PROD/TEYG_s, # Total production when TEYG is closed
    PEYG = PTEYG/EYG_s_SPAM, # Total production when EYG is closed
    PEUYG = PEYG/EUYG_s_SPAM, # Total production when EUYG is closed
    PTYG = PEUYG/TYG_s_SPAM, # Total production when TYG is closed
    POTPROD = PROD/YG_s, # Total production when YG is closed
    TEYG_close = PTEYG - PROD, # Additional production when closing TEYG
    EYG_close = PEYG - PTEYG, # Additional production when closing EYG
    EUYG_close = PEUYG - PEYG, # Additional production when closing EUYG
    TYG_close = POTPROD - PEUYG) %>%
  mutate(check2 = TEYG_close + EYG_close + EUYG_close + TYG_close+PROD)

GapClose2a <- GapClose2 %>% 
  summarize(PROD = sum(PROD/1000000), # in million tons
            TEYG_close = sum(TEYG_close/1000000),
            EYG_close = sum(EYG_close/1000000),
            TYG_close = sum(TYG_close/1000000),
            EUYG_close = sum(EUYG_close/1000000), 
            POTPROD = sum(POTPROD/1000000)) %>%
  mutate(check2 = TEYG_close + EYG_close + EUYG_close + TYG_close+PROD)

# Closing of yield gap assuming decomposition of levels.

GapClose3 <- SPAMData %>%
  rename(Zone = ZONE) %>%
  left_join(ZonalYieldGap_l_sh, .) %>%
  left_join(.,YieldLevels) %>%
  filter(Zone != "Total") %>%
  mutate(POTPROD = PY/Y_SPAM*PROD, # Total production when YG is closed
         TEYG_close = (POTPROD-PROD)*TEYG/100, # Additional production when closing TEYG
         EYG_close = (POTPROD-PROD)*EYG/100, # Additional production when closing EYG
         EUYG_close = (POTPROD-PROD)*EUYG/100, # Additional production when closing EUYG
         TYG_close = (POTPROD-PROD)*TYG/100, # Additional production when closing TYG
         check2 = TEYG_close + EYG_close + EUYG_close + TYG_close+PROD)

GapClose3a <- GapClose3 %>% 
  summarize(PROD = sum(PROD/1000000, na.rm = TRUE), # in million tons
            TEYG_close = sum(TEYG_close/1000000, na.rm = TRUE),
            EYG_close = sum(EYG_close/1000000, na.rm = TRUE),
            TYG_close = sum(TYG_close/1000000, na.rm = TRUE),
            EUYG_close = sum(EUYG_close/1000000, na.rm = TRUE), 
            POTPROD = sum(POTPROD/1000000, na.rm = TRUE)) 

# http://www.r-bloggers.com/waterfall-plots-in-r/
# Create database for waterfall plot
# Add error, which is very small to target production
wf.df <- GapClose3a %>% 
  dplyr::select(PROD,  TEYG_close, EYG_close, EUYG_close, TYG_close, POTPROD)

wf.df <- as.data.frame(t(wf.df)) %>%
  mutate(category =c("Actual \n production", " Closing \n technical efficiency \n yield gap", " Closing \n allocative \n yield gap",
                     " Closing \n economic \n yield gap", " Closing \n technical \n yield gap", "Potential \n production"),
         sector = category) %>%
  rename(value = V1)

# Create waterfall plot
cbPalette <- c("#009E73", "#CC79A7", "#0072B2", "#D55E00", "black", "#999999")
#waterfall_f(wf.df)

## Determines the spacing between columns in the waterfall chart
offset <- 0.3

Fig_waterfall <- waterfall_f(wf.df, offset=offset) +
  scale_fill_manual(guide="none", values=cbPalette)+
  labs(x="", y="Maize production (million tons)") +
  scale_y_continuous(breaks=seq(0, 45, 5), labels = comma) +
  theme_classic() + 
  theme(axis.text=element_text(size=30),
                        axis.title=element_text(size=30,face="bold"),
                        legend.text=element_text(size=30))

# bar charts
# Compare yield levels
# Note we use Y not Ycor
fig_yld_levels_df <- db3 %>% 
  select(ZONE, Ya = Y, Yte = TEY, Ye = EY, Yf = PFY, Yp = PY, area) %>%
  gather(yld_level, yld, -ZONE, -area) %>%
  mutate(yld_level = factor(yld_level, 
                            levels = c("Ya", "Yte", "Ye", "Yf", "Yp"))) %>%
  group_by(yld_level, ZONE) %>%
  summarize(w_yld = (sum((yld)*area)/sum(area)),
            n = n(), 
            min = w_yld - 2*(sd(yld)/sqrt(n)),
            max = w_yld + 2*(sd(yld)/sqrt(n)))

fig_yld_levels_df <- filter(fig_yld_levels_df, !ZONE %in% c("GREATER ACCRA", "WESTERN"))

fig_yld_levels <- ggplot(data = fig_yld_levels_df, aes(x = yld_level, y = w_yld, fill = yld_level)) +
  scale_fill_manual(values = c("grey", "blue", "dark green", "purple", "dark orange")) +
  geom_bar(stat="identity", colour = "black", position = position_dodge(width = 0.1), width=1) +
  geom_errorbar(aes(ymax = max, ymin = min), position = position_dodge(width = 0.1), width = 0.25) +
  facet_grid(~ ZONE) +
  labs(
    title = "Yield levels per zone",
    #subtitle = "check",
    caption = "Note: Error bars measure standard error",
    x = "" , y = "Yield Levels kg/ha") +
  theme_bw() +
  guides(fill = "none") +
  scale_y_continuous(expand = c(0,0), labels=comma)
#theme(panel.spacing = unit(0, "lines"))
fig_yld_levels + theme(axis.text=element_text(size=25),
                       axis.title=element_text(size=25,face="bold"),
                       strip.text.x = element_text(size = 22))
  

yg_share2 <- gather(ZonalYieldGap_l_sh[, -6], gap, level, -Zone)
yg_share2 <- filter(yg_share2, !Zone %in% "Total")

yg_share2$gap <- factor(yg_share2$gap)
levels(yg_share2$gap) <- c("EYG", "AYG", "TEYG", "TYG")

yg_share2$Zone <- ifelse(yg_share2$Zone %in% "GREATER ACCRA", "G. ACCRA", yg_share2$Zone)

ggplot(yg_share2, aes(x = Zone, y = level, fill = gap)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=cbPalette[c(1, 2, 4, 3 )]) +
  ggtitle("Relative Yield Gap") +
  ylab("Share") +
  labs(fill="") +
  theme_classic() +
  theme(axis.text=element_text(size=12.5, face="bold"),
        axis.title=element_text(size=20, face="bold"),
        legend.text=element_text(size=20),
        legend.title=element_blank(),
        strip.text.x = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size=20, face="bold"),
        plot.subtitle = element_text(hjust = 0.5, size=20),
        plot.caption = element_text(size=20)) 
