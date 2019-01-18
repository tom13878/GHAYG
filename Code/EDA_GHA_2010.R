#'==============================================================================
#' Project:  DFID Yield Gap 
#' Subject:  data preparation file
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   plot level chemical variables wave 1 (2009-2010)
#'==============================================================================

# ------------------------------------------------------------------------------
# Notes:
# ------------------------------------------------------------------------------

# get packages
library(pacman)
p_load(char=c("dplyr", "haven", "tidyr", "rprojroot", "sjlabelled", "ggplot2"), install=TRUE)

# set project root and directory path
root <- find_root(is_rstudio_project)

# ==============================================================================
# read in data
source(file.path(root, "Code/GHA_2010.R"))

# ==============================================================================
# Make a yield variable
GHA2010$yld <- GHA2010$crop_qty_harv/GHA2010$area

# ==============================================================================
# histogram of yields
gg_yield_hist <- ggplot(data = GHA2010, aes(x=yld)) +
  geom_histogram(bins = 250, color = "black") +
  scale_x_continuous(limits = c(0, NA),
                     breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 770, color = "red") +
  geom_vline(xintercept = 2340, color = "blue") +
  geom_vline(xintercept = 1887, color = "green") +
  theme_bw() +
  coord_cartesian(xlim = c(0, 5000)) +
  labs(title = "Maize Yield histogram",
       x = "Yield (kg/ha)",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))

# ==============================================================================
# 
# bad <- c(106232070, 106232161, 106230047,
#          105148005, 105165053, 105135076)
# bad <- filter(GHA2010, hhid %in% bad) %>%
#   select(hhid, crop_wgt_unit, crop_qty_harv, value_c, crop_price)
# 
# bad <- GHA2010[grep("106", GHA2010$hhid), ]

gg_qty_cost <- ggplot(data = GHA2010, aes(x = crop_qty_harv, y = value_c)) +
  geom_point(aes(color = crop_wgt_unit), pch = 21) +
  #geom_text(aes(label = hhid)) +
  geom_abline(intercept = 0, slope = 0.35) +
  #facet_wrap(~REGNAME, nrow = 5) +
  theme_bw()


# ==============================================================================
#
gg_qty_area <- ggplot(data = GHA2010, aes(x = area, y = crop_qty_harv)) +
  geom_point(aes(color = unit, shape = factor(crop_count))) +
  geom_abline(intercept = 0, slope = 1750) +
  #facet_wrap(~REGNAME, ncol =2) +
  theme_bw() +
  coord_cartesian(xlim = c(0, 10))




