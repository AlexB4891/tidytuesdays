# -------------------------------------------------------------------------#
#                       Analysing student loan debts                       #
# -------------------------------------------------------------------------#
# Elaborated by: Alex Bajaña

# Libraries:

library(tidyverse)
library(tidytuesdayR)

# Get the data:

sld <- tidytuesdayR::tt_load("2019-11-26")

sld_data <- sld$loans


