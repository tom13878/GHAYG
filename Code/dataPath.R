#'==============================================================================
#' Project:  Food Systems (FORESIGHT, A4NH)
#' Subject:  Get data path
#' Author:   Tomas Morley
#' Contact:  Tomas.morley@wur.nl
#' Output:   correct datapath for user
#'==============================================================================

# Use this file to set your path to the data
# used for the nigeria LSMS-ISA projects
# check your computer username using
# Sys.info()["user"] and use this in the if
# statement. Then add your dataPath within 
# the {} brackets

# the data has been directly downloaded from
# the lsms-isa website. It should therefore
# be possible to download the data, set your
# path and work immediately from the files.
# However, occasionally the world bank also 
# updates the data and introduces new versions.

# Tom
if(Sys.info()["user"] == "morle001") {
  dataPath <- "C:/Users/morle001/WEcR/OneDrive - WageningenUR"}
