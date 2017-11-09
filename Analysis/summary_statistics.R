#'==============================================================================
#' Project:  IMAGINE GHA
#' Subject:  summary statistics of GHA yield gap data
#' Author:   Michiel van Dijk & Tom Morley
#' Contact:  michiel.vandijk@wur.nl, tomas.morley@wur.nl
#' Output:   summary statistics table
#'==============================================================================

# get packages
library(pacman)
p_load(char=c("dplyr", "haven", "tidyr", "rprojroot", "ggplot2", "stargazer", "stringr"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)
gha_data <- readRDS(file.path(root, "Cache/db1.rds"))

# table of summary statistics for wave 2 and wave 3
# for use with stargazer in report
gha_data <- mutate(gha_data,
                   N_users = ifelse(yesN == 0, NA, N),
                   maize_area = ifelse(crop_count > 1, NA, area)) %>%
  ungroup()
desc_stats <- dplyr::select(gha_data,
                            "Yield (kg/ha)" = yld,
                            "Nitrogen Users (kg/ha)" = N_users,
                            "Nitrogen All (kg/ha)" = N,
                            "Labor (hours/ha)" = lab,
                            "Assets (GHC/ha)" = asset,
                            "Plot Area (kg/ha)" = area,
                            "Maize Area (kg/ha)" = maize_area,
                            Manure = manure,
                            Elevation = elevation,
                            "Ph dummy" = phdum2,
                            SOC2,
                            "Single Crop == 1" = crop_count2,
                            "Maize price (GHC/kg)" = Pm,
                            "Nitrogen price (GHC/kg)" = Pn,
                            "Sub nitrogen price (GHC/kg)" = Pns) %>% as.data.frame

# also make a plot of key variables
tmp <- select(gha_data, yld, N_users, lab, asset, Pm, Pns, area) %>%
  gather(variable, value)

# rename the variables in tmp
tmp$variable <- gsub("yld", "Yield", tmp$variable)
tmp$variable <- gsub("N_users", "Nitrogen", tmp$variable)
tmp$variable <- gsub("lab", "Labor", tmp$variable)
tmp$variable <- gsub("asset", "Asset", tmp$variable)
tmp$variable <- gsub("Pm", "Maize Price", tmp$variable)
tmp$variable <- gsub("Pns", "Nitrogen Price", tmp$variable)
tmp$variable <- gsub("area", "Plot Area", tmp$variable)

ggvars <- ggplot(data=tmp, aes(x = value, fill=variable)) +
  geom_histogram(colour="black", alpha=0.5) +
  facet_wrap(~variable, scale="free") +
  theme_bw() +
  guides(fill=FALSE)

# compare maize yields across single and mixed plots
gha_data$plot_type <- ifelse(gha_data$crop_count2 %in% 1, "PURE", "MIXED")
yield_tab <- group_by(gha_data, plot_type) %>%
  summarise(count = n(),
            "Yield\n(kg/ha)" = round(mean(yld, na.rm=TRUE), 3),
            "area\n(ha)" = round(mean(area, na.rm=TRUE), 3),
            "fertilizer\n(%)" = round(sum(N > 0)/count*100, 1),
            "Nitrogen\n(kg/ha)" = round(mean(N, na.rm=TRUE), 2))
names(yield_tab)[1] <- "Plot Type"

# read in results from the sf estimation from SF_yield_gap_analysis.R file
sf_results <- readRDS(file.path(root, "Cache/CD_sf_res.rds"))
sf_results <- rbind(sf_results, c(1226, "-", "-", "-"))
row.names(sf_results) <- c("Intercept",
                           "ln(nitrogen)",
                           "ln(labour)",
                           "ln(asset)",
                           "ln(area)",
                           "herbicide == 1",
                           "mechanisation == 1",
                           "elevation",
                           "manure == 1",
                           "Soil organic content",
                           "Ph dummy",
                           "crop_count == 1",
                           "nitrogen use == 1",
                           "sigmaSq",
                           "gamma",
                           "observations")


# # by region
# # source in the marginal physical products and prices information
# to make table
yield_gaps_gha <- readRDS(file.path(root, "Cache/db3.rds"))
econ_analysis <- group_by(yield_gaps_gha, REGNAME) %>%
   summarise("Nit Users" = sum(yesN==1, na.rm=TRUE),
             "Nit price (GHC/kg)" = round(mean(Pns, na.rm=TRUE), 2),
             "Maize price (GHC/kg)" = round(mean(Pm, na.rm=TRUE), 2),
             "Rel price" = round(mean(relprice, na.rm=TRUE), 2),
             "MPP" = round(mean(mpp, na.rm=TRUE), 2),
             "VCR" = round(mean(mpp/relprice, na.rm=TRUE), 2)) %>%
   rename(Region = REGNAME) %>%
   mutate(Region = tolower(Region))
econ_analysis$Region <- gsub(" region", "", econ_analysis$Region)
econ_analysis$Region <- str_to_title(econ_analysis$Region)

# optimal nitrogen vs actual nitrogen with Yield and EY
opt_nitrogen <- group_by(yield_gaps_gha, REGNAME) %>%
  summarise(Count = n(),
            Nit = sum(yesN == 1),
            "Nit all (kg/ha)" = round(mean(N), 2),
            "Nit users (kg/ha)" = round(mean(N[yesN == 1]), 2),
            "Opt. Nit (kg/ha)" = round(mean(Npm, na.rm=TRUE), 2)) %>%
  rename(Region = REGNAME) %>%
  mutate(Region = tolower(Region))
opt_nitrogen$Region <- gsub(" region", "", opt_nitrogen$Region)
opt_nitrogen$Region <- str_to_title(opt_nitrogen$Region)

