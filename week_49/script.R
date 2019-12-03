# -------------------------------------------------------------------------#
#                    Philadelphia PArkin Violations                        #
# -------------------------------------------------------------------------#
# Elaborated by: Alex Bajaña

# Libraries:

library(tidytuesdayR)
library(tidyverse)

# Create the directory if it's ausent:

if(!dir.exists("week_49")){
dir.create("week_49")
}

# Obtain the data: may takes some minutes.

philly <- tidytuesdayR::tt_load("2019-12-03")


# 