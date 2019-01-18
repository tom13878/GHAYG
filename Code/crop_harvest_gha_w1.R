#'==============================================================================
#' Project:  DFID Yield Gap 
#' Subject:  data preparation file
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   crop harvest wave 1 (2009-2010)
#'==============================================================================

# ------------------------------------------------------------------------------
# Notes:
#       To check crop prices, there is a section in the rural questionnaire
#       section 4. In section 7c there is information on previous crop prices
#       which is recorded with regard to shocks.
#
#       Section 5A of the rural questionnaire has unit conversions to kilograms
#       for goods sold in local markets.
#
#       Some sources tell us that mini bag of maize should be about 50kg,
#       and maxi bags should be 100 kg
#
#       maxi bag = 18, mini bag = 19, kilogram = 14
#       crop harvest is asked per crop from most important to least important
# ------------------------------------------------------------------------------

# get packages
library(pacman)
p_load(char=c("dplyr", "haven", "tidyr", "rprojroot", "sjlabelled"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)
source("Code/get_dataPath.r")

# ------------------------------------------------------------------------------
# read in data file
crop1 <- read_dta(file.path(dataPath, "Data/S4AV1.dta")) %>%
  select(reg=id1, id3, hhno, plotno=s4av1_plotno,
         nharvests = s4v_a79,
         crop1 = s4v_a78i, crop1ID = s4v_a80i,
         harvest_part = s4v_a80ii,
         crop1qty = s4v_a81i, crop1unit = s4v_a81ii,
         crop1_val_c = s4v_a82i, crop1_val_p = s4v_a82ii, # market values (not necessarily sold)
         crop1_revenue_c = s4v_a83i, crop1_revenue_p = s4v_a83ii,
         not_harv = s4v_a84, not_harv_val_c=s4v_a85i, not_harv_val_p = s4v_a85ii,
         loss = s4v_a86, loss_percent = s4v_a87) %>%
  as_factor() %>%
  sjlabelled::remove_all_labels() %>%
  filter(crop1 == "Maize")

# read in land holdings and calcualte yield
source(file.path(root, "Code/land_holdings_gha_w1.R"))
tmp <- left_join(crop1, land, by=c("hhno"="hhno", "plotno"="plotno")) %>%
  filter(crop1unit %in% c(18, 19)) %>%
  mutate(crop1qty_conv = ifelse(crop1unit %in% 18, (crop1qty * 100), crop1qty),
         crop1qty_conv = ifelse(crop1unit %in% 19, (crop1qty * 50), crop1qty_conv),
         yield = crop1qty_conv/area_ha,
         yield2 = crop1qty_conv/area_by_test,
         price = crop1_revenue_c/crop1qty_conv)

ropes <- filter(tmp, unit == "Rope") %>%
  mutate(rope_yield = crop1qty_conv/(as.numeric(unit) * 0.045))

poles <- filter(tmp, unit == "Pole") %>%
  mutate(rope_yield = crop1qty_conv/(as.numeric(unit) * 0.045))
