#'==============================================================================
#' Project:  DFID Yield Gap 
#' Subject:  data preparation file
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   plot level area measurements wave 1 (2009-2010)
#'==============================================================================

# ------------------------------------------------------------------------------
# Notes:
# ------------------------------------------------------------------------------

# get packages
library(pacman)
p_load(char=c("dplyr", "haven", "tidyr", "rprojroot", "sjlabelled"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)
source("Code/get_dataPath.r")

# ------------------------------------------------------------------------------
# read in data file
# change plot_no to plotno as it is in the fertilizer data
land <- read_dta(file.path(dataPath, "Data/S4AII.dta")) %>%
  as_factor() %>%
  select(hhno, plotno = plot_no,
         area_orig = s4aii_a10,
         unit = s4aii_a11,
         area_ha,
         test_area = s4aii_a12,
         plot_rel_test = s4aii_a13,
         test_in_plot = s4aii_a14,
         plot_in_test = s4aii_a15a) %>%
  mutate(EA_No = substr(hhno, 1, 6))

# read in section 5B of rural community questionnaire to find out what 
# the test plot size was in each area.
RSEC5B <- read_dta(file.path(dataPath, "RURAL/SEC 5B.dta")) %>%
  select(reg:plot_number, test_plot_ha = s5b_q4) %>%
  mutate(EA_No = as.character(EA_No))

# some test plots have values in 1000s, these have not be converted
# from sq meters to ha
RSEC5B <- mutate(RSEC5B,
                 test_plot_ha = ifelse(test_plot_ha >= 1799.40,
                                       test_plot_ha/10000,
                                       test_plot_ha)) %>%
  spread(plot_number, test_plot_ha) %>%
  rename(test1_area = "1",
         test2_area = "2")

#
tmp <- left_join(land, RSEC5B) %>%
  sjlabelled::remove_all_labels()

# using their information on the test plot get a better area measuremenr
land <- mutate(tmp,
              area_by_test = ifelse(!is.na(test_in_plot),
                                    test1_area * test_in_plot,
                                    NA),
              area_by_test = ifelse(!is.na(plot_in_test),
                                    test1_area/plot_in_test,
                                    area_by_test))




# take out trash
rm(dataPath)

