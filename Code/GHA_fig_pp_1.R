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
p_load("cowplot", "countrycode")

### DETERMINE ROOT PATH
root <- find_root(is_rstudio_project)

### DATAPATH

### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)

### YIELD FIGURE
# -------------------------------------
# use countrycode package to get
# region and continent for all 
# countries
# -------------------------------------

# Load and process data
maize <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_maize.csv"))
maize_con <- read.csv(file.path(root, "Data/FAOSTAT_data_3-21-2017_maize_continents.csv"))
land <- read.csv(file.path(root, "Data/FAOSTAT_data_1-24-2017_land.csv"))
maize$Area <- as.character(maize$Area)

# select only maize yield and create table
# hectograms are 0.1 kilograms -> convert
maize <- dplyr::select(maize, country=Area, variable = Element,
                       year=Year, value = Value) %>%
  filter(variable=="Yield", country == "Ghana") %>%
  mutate(value = value*0.1)

maize_con <- dplyr::select(maize_con, country=Area, variable = Element,
                           year=Year, value = Value) %>%
  filter(variable=="Yield") %>%
  mutate(value = value*0.1)

Fig_maize_yield_df <- bind_rows(maize_con, maize) 

Fig_maize_yield <- ggplot(data = Fig_maize_yield_df, aes(x = year, y = value, colour = country)) +
  geom_line(size = 1.2) +
  #geom_point(size = 2) +
  #theme_bw() +
  labs(x = "",
       y = "kg/ha",
       title = "") +
  theme(
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position="bottom") +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1960, 2015, 10)) +
  scale_colour_brewer("", palette="Set1") +
  #scale_shape_discrete("") +
  #scale_linetype_manual("", values = c("solid", "dotted", "dashed", "dotdash")) +
  theme(legend.justification=c(0,1), legend.position=c(0.1,0.9)) 

Fig_maize_yield
ggsave("FigTabMap/Fig_maize_yield.png")



### SELF SUFFICIENCY FIGURE
target_scen <- c("Actual farmers' yield 2010", 
                 "Actual yield increase maintained to 2050",
                 "Yield gaps closed to 80% of water-limited potential")

# Load data
ss_ratio_raw <- read_excel(file.path(root, "Data/PNAS_GHA_processed.xlsx"), sheet = "ss_ratio") %>%
  mutate(scenario = factor(scenario))

ss_ratio <- filter(ss_ratio_raw, scenario %in% target_scen)

# Graph
col = rev(brewer.pal(9,"PuBuGn")[c(3,5,7,9)])
ss2010 <- round(ss_ratio_raw$ss_ratio[ss_ratio_raw$type == "Line"], 2)

p_ss = ggplot() +
  theme_classic(10) +
  geom_bar(data = filter(ss_ratio, type == "Bar"), aes(x = type, y = ss_ratio, fill = scenario), colour = "black", stat = "identity", position = "dodge", alpha = 1) +
  scale_y_continuous(limits = c(0,1.2), expand=c(0,0), breaks = c(seq(0, 1.2, 0.2), 1, ss2010)) +
  #scale_y_continuous(limits = c(0,1), expand=c(0,0), breaks = c(seq(0, 2, 0.5), 1, ss2010)) +
  #scale_fill_manual(values = col) +
  scale_fill_brewer(palette="Set1") +
  labs(x = "", y = "Self-sufficiency ratio",
       title = "Self-sufficiency ratios in 2050") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(colour="black"),
        plot.title=element_text(size=10, hjust = 0.5)) +
  geom_hline(aes(yintercept = 1), linetype = "dashed") +
  geom_hline(aes(yintercept = ss2010), linetype = "dashed") +
  annotate("text", 0.55, ss2010+0.04, label = "Self-sufficiency ratio 2010", hjust = 0) +
  annotate("text", 0.55, 1.04, label = "Self-suffient", hjust = 0) 

p_ss 



# ### AREA FIGURE
# # Load data
# ss_area_raw <- read_excel(file.path(root, "Data/PNAS_GHA_processed.xlsx"), sheet = "ss_area") %>%
#   na.omit()
# 
# # Graph
# #col = brewer.pal(9,"PuBuGn")[c(3,5,7,9)]
# a2010 <- round(ss_area_raw$area[ss_area_raw$scenario == "Current cereal area (Mha)"], 2)
# aPotential <- round(ss_area_raw$area[ss_area_raw$scenario == "Potential available cereal area (Mha)"], 2)
# 
# p_area = ggplot() +
#   theme_classic() +
#   geom_bar(data = filter(ss_area_raw, type == "Bar"), aes(x = type, y = area, fill = scenario), colour = "black", stat = "identity", position = "dodge", alpha = 1) +
#   scale_y_continuous(limits = c(0, 9), expand=c(0,0), breaks = c(seq(0, 9, 3), a2010, aPotential)) +
#   scale_fill_manual(values = col) +
#   labs(x = "", y = "Cereal area (Mha)",
#        title = "Cereal area for self-sufficiency of 1 in 2050") +
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.text.y=element_text(colour="black"),
#         plot.title=element_text(size=14, hjust = 0.5)) +
#   geom_hline(aes(yintercept = a2010), linetype = "dashed") +
#   geom_hline(aes(yintercept = aPotential), linetype = "dashed") +
#     annotate("text", 1.45, aPotential+0.2, label = "Potential available cereal area", hjust = 1) +
#     annotate("text", 1.45, a2010+0.2, label = "Current cereal area", hjust = 1)  


### YIELD PROJECTIONS
# Set 2010 value
yield_gha <- filter(maize, country == "Ghana" & year <= 2010)
yield2010 <- yield_gha$value[yield_gha$year == 2010]

# Load projections
ss_yield_raw <- read_excel(file.path(root, "Data/PNAS_GHA_processed.xlsx"), sheet = "ss_yield")

# Replace YA from GYGA with that of FAOSTAT for consistency
ss_yield <- ss_yield_raw %>%
  mutate(year = 2050,
         yield = ifelse(scenario == "Actual farmers' yield 2010", yield2010, yield)) %>%
  filter(scenario %in% target_scen) 

p_yield_proj <- ggplot() +
  theme_classic(10) +
  #scale_colour_manual(values = col) +
  scale_colour_brewer(palette="Set1") +
  labs(x = "", y = "Maize yield (kg/ha)",
       title = "Historical maize yield and scenarios") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_text(colour="black"),
        #axis.ticks.x=element_blank(),
        axis.text.y=element_text(colour="black"),
        plot.title=element_text(size=10, hjust = 0.5)) +
  scale_y_continuous(labels = comma) +
  geom_line(data = yield_gha, aes(x = year, y = value), size = 1.2) + 
  #geom_point(data = ss_yield, aes(x = year, y = yield, colour = scenario)) +
  geom_segment(data = ss_yield, aes(x = 2010, y = yield2010, 
                                    xend = year, yend = yield, colour = scenario,
                                    linetype = scenario), size = 1.2) +
  theme(legend.justification = c(0,1), legend.position = c(0,1))

p_yield_proj


### COMBINE GRAPHS
p_tot <- plot_grid(p_yield_proj + theme(legend.position="none"),
                   p_ss + theme(legend.position="none"), align = "h")
legend <- get_legend(p_ss + theme(legend.position="bottom") + 
                       guides(fill= guide_legend("", ncol=2)))

Fig_ss_a <- plot_grid(p_tot, legend, ncol = 1, rel_heights = c(1, .2))
Fig_ss_a
ggsave("FigTabMap/Fig_ss_a.png", Fig_ss_a, dpi = 600)




